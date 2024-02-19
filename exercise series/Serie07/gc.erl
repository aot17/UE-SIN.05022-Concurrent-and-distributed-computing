-module(gc).
-export([new_group/1, end_group/1, b_multicast/2, start_member/0, member/2, start/0]).

new_group(N) ->
    lists:map(fun(_) -> spawn(gc, start_member, []) end, lists:seq(1, N)).

start_member() ->
    member(self(), []).

member(Pid, MsgQueue) ->
    receive
        {From, Message} ->
            timer:sleep(rand:uniform(1000)), 
            io:format("Process ~p received message from ~p: ~s~n", [Pid, From, Message]),
            member(Pid, [{From, Message} | MsgQueue]);
        stop ->
            io:format("Process ~p is terminating~n", [Pid])
    end.

end_group(Members) ->
    lists:foreach(fun(Pid) -> Pid ! stop end, Members).

b_multicast(Members, Message) ->
    Self = self(),
    lists:foreach(fun(Pid) -> Pid ! {Self, Message} end, Members).

start() ->
    Group = gc:new_group(6),
    io:format("Group members : ~p~n", [Group]),
    timer:sleep(1000),
    gc:b_multicast(Group, "Hello, World! FIRST"),
    gc:b_multicast(Group, "Hello, World! SECOND"),
    gc:b_multicast(Group, "Hello, World! THIRD"),
    gc:end_group(Group).
