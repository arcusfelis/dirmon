-module(dirmon_server).
-behaviour(gen_server).

%% Client API
-export([start_link/1,
         start_link/2,
         monitor/2,
         match_and_monitor/2,
         demonitor/2,
         update/1]).
         
%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



-record(state, {
    file_tree, 
    last_scan_begun,
    patterns, 
    check_timeout, 
    exit_timeout,
    %% A timestamp (in milliseconds)
    last_timeout_check
}).
-record(monitor_msg, {pattern, match}).
-record(demonitor_msg, {ref}).

-type server() :: gen_server:name().

%% ------------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------------

start_link(DirName) ->
    start_link(DirName, []).

%% `Options' are:
%%
%% - `check_timeout' - how often the the directory will be scanned (in milliseconds).
%% The default value is 5000.
%% - `exit_timeout' - the minimum period of time beetween X and Y.
%% The default value is infinity.
%% Event X: the last client unregistered.
%% Event Y: the server is closed automatically.
%%
%%  If `check_timeout' is infinity, than `exit_timeout' will be 
%%  treated as `infinity'.
start_link(DirName, Options) ->
    gen_server:start_link(?MODULE, [DirName|Options], []).

monitor(Server, Re) ->
    gen_server:call(Server, #monitor_msg{pattern = Re, match = false}).

-spec match_and_monitor(Server, Re) -> {ok, MatchedFileNames, Ref}
    when
    Server :: server(),
    Re :: re:mp(),
    MatchedFileNames :: [file:filename()],
    Ref :: reference().
match_and_monitor(Server, Re) ->
    gen_server:call(Server, #monitor_msg{pattern = Re, match = true}).


demonitor(Server, Ref) ->
    gen_server:call(Server, #demonitor_msg{ref = Ref}).

%% @doc Ask server to collect information again.
%% This action is called automatically every few seconds.
-spec update(Server) -> ok when Server :: server().
update(Server) ->
    gen_server:call(Server, update).

%% ------------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------------

init([DirName|Options]) ->
    {ok, FileTree} = dirmon_lib:new(DirName),
    Patterns = dirmon_pattern:new(),
    CheckTimeout = proplists:get_value(check_timeout, Options, 5000),
    ExitTimeout = proplists:get_value(exit_timeout, Options, infinity),
    State = #state{file_tree = FileTree,
                   last_scan_begun = erlang:localtime(),
                   patterns = Patterns,
                   exit_timeout = ExitTimeout},
    timer:send_interval(CheckTimeout, check),
    {ok, State}.

handle_call(#monitor_msg{pattern = Re, match = false},
            {ClientPid, _}, State=#state{patterns = PS}) ->
    {ok, PS2, Ref, _} = dirmon_pattern:add(PS, Re, ClientPid), %% usb is better!
    {reply, {ok, Ref}, State#state{patterns = PS2}};
handle_call(#monitor_msg{pattern = Re, match = true},
            {ClientPid, _}, State=#state{file_tree = Tree, patterns = PS}) ->
    {ok, PS2, Ref, CompiledRe} = dirmon_pattern:add(PS, Re, ClientPid),
    Match = dirmon_lib:match_tree(Tree, CompiledRe),
    {reply, {ok, Match, Ref}, State#state{patterns = PS2}};
handle_call(#demonitor_msg{ref = Ref}, _, State=#state{patterns = PS}) ->
    erlang:demonitor(Ref, [flush]),
    {ok, PS2} = dirmon_pattern:unregister_reference(PS, Ref),
    {reply, {ok, Ref}, State#state{patterns = PS2}};
handle_call(update, _, State=#state{}) ->
    {reply, ok, check(State)}.

handle_cast(_Mess, State) ->
    {noreply, State}.


%% Find new files and inform clients.
%% It is a timer.
%%
%% The idea is to evaluate `exit_timeout' and `check_timeout' using only
%% one timeout.
handle_info(check, 
            State=#state{patterns = PS,
                         last_timeout_check = LastCheck,
                         exit_timeout = ExitTimeout,
                         check_timeout = CheckTimeout}) ->
    case dirmon_pattern:is_empty(PS) of
    %% There are clients, work as usual.
    false -> {noreply, check(State#state{last_timeout_check=timestamp_ms()})};
    %% Ignore exit timeout.
    true when ExitTimeout =:= infinity -> {noreply, State};
    true ->
        %% If there are no more new client within at least `ExitTimeout' ms,
        %% than we will close the server.
        %%
        %% For any `LastCheck' this condition is true, when the next case is
        %% true.
        case timestamp_ms() - LastCheck > ExitTimeout + CheckTimeout of
        %% Exit, because no new client was for a long period of time.
        true -> {stop, normal, State};
        false -> {noreply, State}
        end
    end;
%% The client is dead.
handle_info({'DOWN', MonitorRef, _, _, _}, State=#state{patterns = PS}) ->
    {ok, PS2} = dirmon_pattern:unregister_reference(PS, MonitorRef),
    {noreply, State#state{patterns = PS2}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


check(State=#state{patterns = PS, file_tree = Tree,
                   last_scan_begun = PrevTime}) ->
    Patterns = dirmon_pattern:patterns(PS),
    StartTime = erlang:localtime(),
    {ok, Tree2, Events} = dirmon_lib:check(Tree, PrevTime, []),
    case Events of
        [] -> ok;
        _  -> [case dirmon_lib:match(Events, Re) of 
                    [] -> ok;
                    [_|_]=Match -> Inf(Match)
               end || {Re, Inf} <- Patterns]
    end,
    State#state{file_tree = Tree2, last_scan_begun = StartTime}.

timestamp_ms() ->
    timestamp() div 1000.

timestamp() ->
    timestamp(erlang:now()).

timestamp({Mega, Secs, Micro}) ->
        Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.
