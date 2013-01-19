-module(dirmon_watchers).
-export([new/0,
         add/3,
         remove/2,
         exists/2,
         reference_to_number/2]).

-record(ws, {next_num, pid2ref, ref2num}).

new() ->
    #ws{next_num = 0, pid2ref = dict:new(), ref2num = dict:new()}.

add(Pid, Ref, WS=#ws{next_num=Num, pid2ref = P2R, ref2num = R2N}) 
    when is_reference(Ref) ->
    P2R2 = dict:store(Pid, Ref, P2R),
    R2N2 = dict:store(Ref, Num, R2N),
    {ok, Num, WS#ws{next_num=Num+1, pid2ref=P2R2, ref2num=R2N2}}.

remove(Pid, WS=#ws{pid2ref = P2R, ref2num = R2N}) ->
    case dict:find(Pid, P2R) of
        error ->
            {error, unknown_watcher};
        {ok, Ref} ->
            P2R2 = dict:erase(Pid, P2R),
            R2N2 = dict:erase(Ref, R2N),
            {ok, WS#ws{pid2ref=P2R2, ref2num=R2N2}}
    end.

exists(Pid, #ws{pid2ref = P2R}) ->
    dict:is_key(Pid, P2R).

reference_to_number(Ref, #ws{ref2num = R2N}) ->
    case dict:find(Ref, R2N) of
        error ->
            {error, unknown_watcher};
        {ok, Num} ->
            {ok, Num}
    end.

