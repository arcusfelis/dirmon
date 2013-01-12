-module(dirmon_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
        simple_case/0,
        simple_case/1
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

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
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
        simple_case
    ]}].

all() ->
    [{group, main_group}].


simple_case() ->
    [{require, common_conf, dirmon_common_config}].

-include_lib("eunit/include/eunit.hrl").


simple_case(Cfg) ->
    DataDir = ?config(data_dir, Cfg),
    T = erlang:localtime(),
    {ok, D} = dirmon:new(DataDir),

    io:format(user, "Time: ~p~n", [T]),

    %% Let T =/= MTime.
    timer:sleep(1000),
    F3 = filename:join(DataDir, f3),
    ok = touch(F3),

    T1 = erlang:localtime(),
    {ok, D2, Es} = dirmon:check(D, T, []),
    io:format(user, "Events: ~p~n", [Es]),
    M1 = dirmon:match(Es, "f3"),
    io:format(user, "Match: ~p~n", [M1]),
    ?assertEqual([{added, F3}], M1),

    F1 = filename:join([DataDir, d1, f1]),
    filelib:ensure_dir(F1),
    ok = touch(F1),

    %% In this situation, T1 can be equal or less that MTime.
    T2 = erlang:localtime(),
    {ok, D3, Es2} = dirmon:check(D2, T1, []),
    io:format(user, "Dir3: ~p~n", [D3]),
    io:format(user, "Events: ~p~n", [Es2]),
    M2 = dirmon:match(Es2, "f."),
    io:format(user, "Match: ~p~n", [M2]),
    ?assertEqual([{added, F1}], M2),


    %% Check deletion of the F1 file.
    ok = file:delete(F1),
    T3 = erlang:localtime(),
    {ok, D4, Es3} = dirmon:check(D3, T2, []),
    io:format(user, "Dir4: ~p~n", [D4]),
    M3 = dirmon:match(Es3, "f."),
    io:format(user, "Match: ~p~n", [M3]),
    ?assertEqual([{deleted, F1}], M3),

    %% Check creation after deletion.
    ok = touch(F1),
    T4 = erlang:localtime(),
    {ok, D5, Es4} = dirmon:check(D4, T3, []),
    M4 = dirmon:match(Es4, "f."),
    ?assertEqual([{added, F1}], M4),

    %% Check changing.
    %%
    %% Without this timer the next check may return an empty list.
    %% The situation is that old and new mtimes are the same.
    %% It is because we don't want to compare file hashes.
    timer:sleep(1000),
    ok = touch(F1),
    T5 = erlang:localtime(),
    {ok, D6, Es5} = dirmon:check(D5, T4, []),
    M5 = dirmon:match(Es5, "f."),
    ?assertEqual([{modified, F1}], M5),


%%  touch(F1),
    ok.


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

