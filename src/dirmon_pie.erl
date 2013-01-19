%% This server combines few `dirmon_server' and allows to monitor
%% files from different directories.
%% This module and autofs have a simular semantics.
-module(dirmon_pie).
-behaviour(gen_server).

%% Client API
-export([start_link/2,
         monitor/1,
         match_and_monitor/1,
         demonitor/2,
         add_watcher/2,
         remove_watcher/2,
         update/1]).
         
%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).



-record(state, {
        pattern, 
        watchers, 
        key_maker,
        key2filenames,
        clients
}).


-record(add_watcher, {server}).
-record(remove_watcher, {server}).
-record(monitor, {match}).
-record(demonitor, {reference}).

-type server() :: gen_server:name().

%% ------------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------------

start_link(Re, KeyMaker) ->
    gen_server:start_link(?MODULE, [Re, KeyMaker], []).

add_watcher(Server, Watcher) ->
    gen_server:call(Server, #add_watcher{server = Watcher}).

remove_watcher(Server, Watcher) ->
    gen_server:call(Server, #remove_watcher{server = Watcher}).

monitor(Server) ->
    gen_server:call(Server, #monitor{match = false}).

match_and_monitor(Server) ->
    gen_server:call(Server, #monitor{match = true}).

demonitor(Server, Ref) ->
    gen_server:call(Server, #demonitor{reference = Ref}).

update(Server) ->
    gen_server:call(Server, update).

%% ------------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------------

init([Re, KeyMaker]) ->
    State = #state{pattern = Re, key_maker = KeyMaker, 
                   watchers = dirmon_watchers:new(), 
                   key2filenames = dict:new(),
                   clients = []},
    {ok, State}.

handle_call(#add_watcher{server = Watcher}, _, 
            State=#state{pattern = Re, watchers = WS, key_maker = KeyMaker,
                         key2filenames = Key2FileNames}) ->
    case dirmon_watchers:exists(Watcher, WS) of
    false ->
        %% TODO: Handle `{DOWN,....}'.
        erlang:monitor(process, Watcher),
        {ok, MatchedFileNames, Ref} = 
            dirmon_watcher:match_and_monitor(Watcher, Re),
        {ok, Num, WS2} = dirmon_watchers:add(Watcher, Ref, WS),
        Key2FN = [{KeyMaker(FN), [{Num, FN}]} || FN <- MatchedFileNames],
        %% The new files are added in the end.
        F = fun(_Key,X,Y) -> Y ++ [X] end,
        Key2FileNames2 = dict:merge(F, dict:from_list(Key2FN), Key2FileNames),
        %% Are there new files?
        Events = dict:fold(fun(K,[FN],Acc) -> [{added,K,FN}|Acc];
                          (_,_,Acc) -> Acc end, [], Key2FileNames2),
        inform_clients(Events, State),
        {reply, {ok, Ref}, State#state{key2filenames = Key2FileNames2,
                                       watchers = WS2}};
    true ->
        {reply, {error, already_added}, State}
    end;
handle_call(#monitor{match = true}, {Pid,Ref},
            State=#state{clients = Cs, key2filenames = Key2FileNames}) ->
    Events = dict:fold(fun(K,[FN|_],Acc) -> [{added,K,FN}|Acc]
                       end, [], Key2FileNames),
    {reply, {ok, Ref, Events}, State#state{clients = [{Pid,Ref}|Cs]}};
handle_call(#monitor{match = false}, {Pid,Ref},
            State=#state{clients = Cs}) ->
    {reply, {ok, Ref}, State#state{clients = [{Pid,Ref}|Cs]}}.



handle_cast(_Mess, State) ->
    {noreply, State}.


handle_info({dirmon, ServerRef, Events}, 
            State=#state{watchers = WS, key_maker = KeyMaker,
                         key2filenames = Key2FileNames}) ->
    io:format(user, "dirmon: ~p ~p~n", [ServerRef, Events]),
    {ok, ServerNum} = dirmon_watchers:reference_to_number(ServerRef, WS),
    {Events2, Key2FileNames2} = 
        handle_events(Events, Key2FileNames, KeyMaker, ServerNum, []),
    inform_clients(Events2, State),
    {noreply, State#state{key2filenames = Key2FileNames2}}.


handle_events([{deleted, FN}|Es], Key2FileNames, KeyMaker, ServerNum, Acc) ->
    Key = KeyMaker(FN),
    Elem = {ServerNum,FN},
    case dict:find(Key, Key2FileNames) of
        %% File was deleted -> delete a key
        {ok, [Elem]} ->
            Key2FileNames2 = dict:delete(Key, Key2FileNames),
            Acc2 = [{deleted, Key, FN}|Acc],
            handle_events(Es, Key2FileNames2, KeyMaker, ServerNum, Acc2);

        %% File was deleted -> set a file from other watcher
        {ok, [Elem|Elems]} ->
            {_Num, NewFN} = hd(Elems),
            Key2FileNames2 = dict:store(Key, Elems, Key2FileNames),
            Acc2 = [{modified, Key, NewFN}|Acc],
            handle_events(Es, Key2FileNames2, KeyMaker, ServerNum, Acc2);

        %% File was deleted -> do not inform our clients.
        {ok, Elems} ->
            Elems2 = lists:delete(Elem, Elems),
            Key2FileNames2 = dict:store(Key, Elems2, Key2FileNames),
            handle_events(Es, Key2FileNames2, KeyMaker, ServerNum, Acc);

        error -> 
            error({unknown_filename, FN})
    end;
handle_events([{added, FN}|Es], Key2FileNames, KeyMaker, ServerNum, Acc) ->
    Key = KeyMaker(FN),
    Elem = {ServerNum,FN},
    case dict:find(Key, Key2FileNames) of
        %% A key exists.
        {ok, Elems} ->
            Elems2 = ordsets:add_element(Elem, Elems),
            Key2FileNames2 = dict:store(Key, Elems2, Key2FileNames),
            Acc2 = case Elems2 of
                    [Elem|_] -> [{modified, Key, FN}|Acc];
                    [] -> Acc
                end,
            handle_events(Es, Key2FileNames2, KeyMaker, ServerNum, Acc2);

        %% Add a new key.
        error ->
            Acc2 = [{added, Key, FN}|Acc],
            Key2FileNames2 = dict:store(Key, [Elem], Key2FileNames),
            handle_events(Es, Key2FileNames2, KeyMaker, ServerNum, Acc2)
    end;
%% This event do not change `Key2FileNames'.
handle_events([{modified, FN}|Es], Key2FileNames, KeyMaker, ServerNum, Acc) ->
    Key = KeyMaker(FN),
    Elem = {ServerNum,FN},
    case dict:find(Key, Key2FileNames) of
        %% File was changed -> inform clients
        {ok, [Elem|_Elems]} ->
            Acc2 = [{modified, Key, FN}|Acc],
            handle_events(Es, Key2FileNames, KeyMaker, ServerNum, Acc2);

        %% File was updated -> ignore
        {ok, _Elems} ->
            handle_events(Es, Key2FileNames, KeyMaker, ServerNum, Acc);

        error -> 
            error({unknown_filename, FN})
    end;
handle_events([], Key2FileNames, _KeyMaker, _ServerNum, Acc) ->
    {Acc, Key2FileNames}.



terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ----------------------------------------------------------------------
%% Helpers
%% ----------------------------------------------------------------------

inform_clients([], _) ->
    ok;
inform_clients([_|_]=Events, #state{clients = Cs}) ->
    [CPid!{pie,CRef,Events} || {CPid,CRef} <- Cs].
