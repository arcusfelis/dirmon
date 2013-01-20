-module(dirmon_watchers).
-export([new/0,
         add/4,
         remove/2,
         exists/2,
         tag_to_number/2]).

-record(ws, {next_num, list}).
-record(w, {pid, tag, mon, num}).

new() ->
    #ws{next_num = 0, list = []}.

add(Pid, Tag, Mon, WS=#ws{next_num=Num, list=Ws}) 
    when is_reference(Tag), is_reference(Mon) ->
    W = #w{pid=Pid, tag=Tag, mon=Mon, num=Num},
    {ok, Num, WS#ws{next_num=Num+1, list=[W|Ws]}}.

remove(Pid, WS=#ws{list=Ws}) ->
    case lists:keytake(Pid, #w.pid, Ws) of
        {value, #w{pid=Pid, tag=Tag, mon=Mon, num=Num}, Ws2} ->
            {ok, Pid, Tag, Mon, Num, WS#ws{list=Ws2}};
        false ->
            {error, unknown_watcher}
    end.

exists(Pid, #ws{list=Ws}) ->
    lists:keymember(Pid, #w.pid, Ws).

tag_to_number(Tag, #ws{list=Ws}) ->
    case lists:keyfind(Tag, #w.tag, Ws) of
        #w{num=Num} ->
            {ok, Num};
        error ->
            {error, unknown_watcher}
    end.


