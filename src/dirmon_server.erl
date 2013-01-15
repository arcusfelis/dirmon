-module(dirmon_server).
-behaviour(gen_server).

%% Client API
-export([start_link/1,
         monitor/2]).
         
%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



-record(state, {file_tree, last_scan_begun, patterns}).
-record(monitor_msg, {pattern}).

%% ------------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------------

start_link(DirName) ->
    gen_server:start_link(?MODULE, DirName, []).

monitor(Server, Re) ->
    gen_server:call(Server, #monitor_msg{pattern = Re}).

%% ------------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------------

init(DirName) ->
    {ok, FileTree} = dirmon_lib:new(DirName),
    Patterns = dirmon_patterns:new(),
    State = #state{file_tree = FileTree, 
                   last_scan_begun = erlang:localtime(),
                   patterns = Patterns},
    timer:send_after(5000, check),
    {ok, State}.

handle_call(#monitor_msg{pattern = Re},
            {ClientPid, _}, State=#state{patterns = PS}) ->
    {ok, PS2, Ref} = dirmon_patterns:add(PS, Re, ClientPid), %% usb is better!
    {reply, {ok, Ref}, State#state{patterns = PS2}}.

handle_cast(_Mess, State) ->
    {noreply, State}.


%% Find new files and inform clients.
handle_info(check, State=#state{patterns = PS, file_tree = Tree,
                                last_scan_begun = PrevTime}) ->
    Patterns = dirmon_patterns:patterns(PS),
    StartTime = erlang:localtime(),
    {ok, Tree2, Events} = dirmon_lib:check(Tree, PrevTime, []),
    case Events of
        [] -> ok;
        _  -> [case dirmon_lib:match(Events, Re) of 
                    [] -> ok;
                    [_|_]=Match -> Inf(Match)
               end || {Re, Inf} <- Patterns]
    end,
    State2 = State#state{file_tree = Tree2, last_scan_begun = StartTime},
    {noreply, State2}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
