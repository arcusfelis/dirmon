%% Stores information about clients and their queries.
-module(dirmon_pattern).
-export([new/0,
         add/3,
         unregister_process/2,
         unregister_reference/2,
         patterns/1]).

%% pattern is a regular expression as a string.
%% cpattern is a compiled version of it.
%% ref is a registration reference.
%% pid is a client process id.
-record(p, {ref, pid, re}).
-type ps() :: [#p{}].

new() ->
    [].


add(PS, Re, ClientPid) ->
    {ok, CompiledRe} = re:compile(Re),
    Ref = erlang:monitor(process, ClientPid),
    {ok, [#p{ref = Ref, pid = ClientPid, re = CompiledRe}|PS], Ref}.
    

unregister_process(PS, ClientPid) ->
    {ok, [P || P=#p{pid = X} <- PS, ClientPid =/= X]}.


unregister_reference(PS, Ref) ->
    {ok, [P || P=#p{ref = X} <- PS, Ref =/= X]}.


%% @doc Generate a list of regular expressions for checking with.
%% `Informator' will be called, if the expession matches.
-spec patterns(Ps) -> [{Re, Informator}] when
    Ps :: ps(),
    Re :: re:mp(),
    Informator :: fun((Re, Mess) -> ok),
    Mess :: term().

patterns(Ps) ->
    [{Re, fun(Mess) -> Pid ! {dirmon, Ref, Mess} end}
      || #p{ref = Ref, pid = Pid, re = Re} <- Ps].

