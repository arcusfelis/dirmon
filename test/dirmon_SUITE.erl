-module(dirmon_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([simple_case/0,
         simple_case/1,
         server_case/0,
         server_case/1,
         server_match_and_monitor_case/0,
         server_match_and_monitor_case/1,
         pie_case/0,
         pie_case/1,
         server_delete_event_case/0,
         server_delete_event_case/1,
         server_delete2_event_case/0,
         server_delete2_event_case/1,
         simple_delete_event_case/0,
         simple_delete_event_case/1
        ]).

%-compile([{parse_transform, lager_transform}]).

suite() ->
    [{timetrap, {minutes, 3}}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    %% We should really use priv_dir here, but as we are for-once creating
    %% files we will later rely on for fetching, this is ok I think.
    DataDir = ?config(data_dir, Config),
%   lager:info("Data directory: ~s~n", [DataDir]),
    ok = ensure_dir(DataDir),
    file:set_cwd(DataDir),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    io:format(user, "~2nBEGIN TESTCASE ~p~n", [Case]),
    Config.

end_per_testcase(Case, Config) ->
    io:format(user, "END TESTCASE ~p~3n", [Case]),
    DataDir = ?config(data_dir, Config),
    io:format(user, "Cleaning~n", []),
    [file:delete(filename:join(DataDir, X)) || X <- files()],
    [file:del_dir(filename:join(DataDir, X)) || X <- directories()],
    ok.

%% Configuration
%% ----------------------------------------------------------------------

files() ->
    ["d1/f1"
    ,"d2/f2"
    ,"d1/1.x"
    ,"d1/2.x"
    ,"d2/2.x"
    ,"d2/3.x"
    ,"f3"
    ].

directories() ->
    ["d1"
    ,"d2"
    ].


%% Tests
%% ----------------------------------------------------------------------
groups() ->
    [{main_group, [], [
        simple_case,
        server_case,
        server_match_and_monitor_case,
        simple_delete_event_case,
        server_delete_event_case,
        server_delete2_event_case,
        pie_case
    ]}].

all() ->
    [{group, main_group}].


simple_case() ->
    [{require, common_conf, dirmon_common_config}].

server_case() ->
    [{require, common_conf, dirmon_common_config}].

server_match_and_monitor_case() ->
    [{require, common_conf, dirmon_common_config}].

pie_case() ->
    [{require, common_conf, dirmon_common_config}].

server_delete_event_case() ->
    [{require, common_conf, dirmon_common_config}].

server_delete2_event_case() ->
    [{require, common_conf, dirmon_common_config}].

simple_delete_event_case() ->
    [{require, common_conf, dirmon_common_config}].

-include_lib("eunit/include/eunit.hrl").


simple_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    T = erlang:localtime(),
    {ok, D} = dirmon_lib:new(DataDir),

    io:format(user, "Time: ~p~n", [T]),

    %% Let T =/= MTime.
    timer:sleep(1000),
    F3 = filename:join(DataDir, f3),
    ok = touch(F3),

    T1 = erlang:localtime(),
    {ok, D2, Es} = dirmon_lib:check(D, T, []),
    io:format(user, "Events: ~p~n", [Es]),
    M1 = dirmon_lib:match(Es, "f3"),
    io:format(user, "Match: ~p~n", [M1]),
    ?assertEqual([{added, F3}], M1),

    F1 = filename:join([DataDir, d1, f1]),
    filelib:ensure_dir(F1),
    ok = touch(F1),

    %% In this situation, T1 can be equal or less that MTime.
    T2 = erlang:localtime(),
    {ok, D3, Es2} = dirmon_lib:check(D2, T1, []),
    io:format(user, "Dir3: ~p~n", [D3]),
    io:format(user, "Events: ~p~n", [Es2]),
    M2 = dirmon_lib:match(Es2, "f."),
    io:format(user, "Match: ~p~n", [M2]),
    ?assertEqual([{added, F1}], M2),


    %% Check deletion of the F1 file.
    ok = file:delete(F1),
    T3 = erlang:localtime(),
    {ok, D4, Es3} = dirmon_lib:check(D3, T2, []),
    io:format(user, "Dir4: ~p~n", [D4]),
    M3 = dirmon_lib:match(Es3, "f."),
    io:format(user, "Match: ~p~n", [M3]),
    ?assertEqual([{deleted, F1}], M3),

    %% Check creation after deletion.
    ok = touch(F1),
    T4 = erlang:localtime(),
    {ok, D5, Es4} = dirmon_lib:check(D4, T3, []),
    M4 = dirmon_lib:match(Es4, "f."),
    ?assertEqual([{added, F1}], M4),

    %% Check changing.
    %%
    %% Without this timer the next check may return an empty list.
    %% The situation is that old and new mtimes are the same.
    %% It is because we don't want to compare file hashes.
    timer:sleep(1000),
    ok = touch(F1),
    T5 = erlang:localtime(),
    {ok, D6, Es5} = dirmon_lib:check(D5, T4, []),
    M5 = dirmon_lib:match(Es5, "f."),
    ?assertEqual([{modified, F1}], M5),
    ok.


server_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    {ok, S} = dirmon_watcher:start_link(DataDir),
    {ok, Ref} = dirmon_watcher:monitor(S, ""),

    timer:sleep(1000),
    F3 = filename:join(DataDir, f3),
    ok = touch(F3),
    dirmon_watcher:update(S),

    %% Wait for a message.
    ?assertEqual({ok, [{added,F3}]}, wait_event(Ref, 1000)),

    D1 = filename:join([DataDir, d1]),
    F1 = filename:join([DataDir, d1, f1]),
    filelib:ensure_dir(F1),
    ok = touch(F1),
    dirmon_watcher:update(S),
    ?assertEqual({ok, [{added, F1}, {added,D1}]}, wait_event(Ref, 1000)),

    ok = file:delete(F1),
    dirmon_watcher:update(S),
    ?assertEqual({ok, [{deleted, F1}]}, wait_event(Ref, 1000)),

    ok.


server_match_and_monitor_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    {ok, S} = dirmon_watcher:start_link(DataDir),
    {ok, Match, Ref} = dirmon_watcher:match_and_monitor(S, ""),
    ok.


%% Add, Start, Delete.
simple_delete_event_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    D2 = filename:join(DataDir, d2),
    D2F2 = filename:join(D2, "2.x"),
    ensure_dir(D2),
    ok = touch(D2F2),

    T = erlang:localtime(),
    %% DT stands for DirectoryTree.
    {ok, DT} = dirmon_lib:new(D2),

    %% Check deletion of the F1 file.
    ok = file:delete(D2F2),
    {ok, _DT, Es} = dirmon_lib:check(DT, T, []),
    M = dirmon_lib:match(Es, "\\.x$"),
    ?assertEqual([{deleted, D2F2}], M),
    ok.


%% It is a minimal test case for the error in `pie_case'.
%% Add, Start, Delete.
server_delete_event_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    D2 = filename:join(DataDir, d2),
    D2F2 = filename:join(D2, "2.x"),
    ensure_dir(D2),
    ok = touch(D2F2),
    {ok, S2} = dirmon_watcher:start_link(D2),
    {ok, Match, Ref} = dirmon_watcher:match_and_monitor(S2, "\\.x$"),

    ok = file:delete(D2F2),
    dirmon_watcher:update(S2),

    ?assertEqual({ok, [{deleted, D2F2}]}, wait_event(Ref, 1000)),
    ok.


%% Start, Add, Delete.
server_delete2_event_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    D1 = filename:join(DataDir, d2),
    D1F2 = filename:join(D1, "2.x"),
    ensure_dir(D1),
    {ok, S1} = dirmon_watcher:start_link(D1),
    {ok, Match, Ref} = dirmon_watcher:match_and_monitor(S1, "\\.x$"),
    ok = touch(D1F2),
    dirmon_watcher:update(S1),
    ?assertEqual({ok, [{added, D1F2}]}, wait_event(Ref, 1000)),

    ok = file:delete(D1F2),
    dirmon_watcher:update(S1),
    ?assertEqual({ok, [{deleted, D1F2}]}, wait_event(Ref, 1000)),
    ok.


pie_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    D1 = filename:join(DataDir, d1),
    D2 = filename:join(DataDir, d2),
    D1F1 = filename:join(D1, "1.x"),
    D1F2 = filename:join(D1, "2.x"),
    D2F2 = filename:join(D2, "2.x"),
    D2F3 = filename:join(D2, "3.x"),
    ensure_dir(D1),
    ensure_dir(D2),
    ok = touch(D2F2),
    {ok, S1} = dirmon_watcher:start_link(D1),
    {ok, S2} = dirmon_watcher:start_link(D2),
    {ok, P1} = dirmon_pie:start_link("\\.x$", fun key_maker/1),
    dirmon_pie:add_watcher(P1, S1),
    %% No clients will receive `added' event from S1.
    {ok, _Ref} = dirmon_pie:add_watcher(P1, S2),
    %% Start listening.
    {ok, PRef} = dirmon_pie:monitor(P1),
    ok = touch(D1F2),
    dirmon_watcher:update(S1),
    ?assertEqual({ok, {pie, PRef, [{modified, {"2", ".x"}, D1F2}]}},
                 wait_message(1000)),

    ok = file:delete(D1F2),
    dirmon_watcher:update(S1),
    %% `D1F2' is replaced by `D2F2'.
    ?assertEqual({ok, {pie, PRef, [{modified, {"2", ".x"}, D2F2}]}},
                 wait_message(1000)),

    ok = file:delete(D2F2),
    dirmon_watcher:update(S2),
    ?assertEqual({ok, {pie, PRef, [{deleted, {"2", ".x"}, D2F2}]}},
                 wait_message(1000)),

    timer:sleep(1000),
    print_mailbox(),
    ok.

key_maker(FileName) ->
    BaseName = filename:basename(FileName),
    Ext      = filename:extension(BaseName),
    RootName = filename:rootname(BaseName),
    {RootName, Ext}.


wait_event(Ref, Timeout) ->
    receive
        {dirmon, Ref, Mess} -> {ok, Mess}
    after Timeout -> {error, noevent}
    end.

wait_message(Timeout) ->
    receive
        Mess -> {ok, Mess}
    after Timeout -> {error, noevent}
    end.

print_mailbox() ->
    receive
        X -> io:format(user, "Mess: ~p~n", [X]), print_mailbox()
    after 0 -> ok
    end.

%% Helpers
%% ----------------------------------------------------------------------

ensure_dir(Dir) ->
    filelib:ensure_dir(filename:join(Dir, "sub_file")).

touch(FileName) ->
    io:format(user, "Touch ~ts~n", [FileName]),
    case file:change_time(FileName, erlang:localtime()) of
        {error,enoent} -> file:write_file(FileName, <<>>);
        Error -> Error
    end.

