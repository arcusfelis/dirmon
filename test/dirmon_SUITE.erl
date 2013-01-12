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
    {ok, D} = dirmon:new(DataDir),
    T = erlang:localtime(),
    io:format(user, "Time: ~p~n", [T]),
    timer:sleep(1000),
    F3 = filename:join(DataDir, f3),
    ok = touch(F3),

    {ok, D1} = dirmon:new(DataDir),
    io:format(user, "Dir: ~p~n", [D1]),

    {ok, D2, Es} = dirmon:check(D, T, []),
    io:format(user, "Events: ~p~n", [Es]),
    M1 = dirmon:match(Es, "f3"),
    io:format(user, "Match: ~p~n", [M1]),
    ?assertEqual([{added, F3}], M1),
    T1 = erlang:localtime(),
    timer:sleep(1000),

    F1 = filename:join([DataDir, d1, f1]),
    filelib:ensure_dir(F1),
    ok = touch(F1),
    {ok, D3, Es2} = dirmon:check(D2, T1, []),
    io:format(user, "Dir: ~p~n", [D3]),
    io:format(user, "Events: ~p~n", [Es2]),
    M2 = dirmon:match(Es2, "f."),
    io:format(user, "Match: ~p~n", [M2]),
    ?assertEqual([{added, F1}], M2),

    touch(F1),
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

