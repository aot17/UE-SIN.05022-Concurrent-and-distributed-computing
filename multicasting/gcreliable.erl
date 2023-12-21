-module(gcreliable).
-export([start/0, r_handler/3, r_multicast/2]).

start() ->
    testr(),
    ok.

testr() ->
    io:format("~n======= TESTING R_MULTICAST =======~n"),
    ReliableGroup = rCreateProcesses(5),
    Messages = ["message 1", "message 2", "message 3"],
    lists:foreach(fun(Message) -> 
        r_multicast(ReliableGroup, {message, Message}),
        timer:sleep(1000)
    end, Messages).

rCreateProcesses(N) ->
    [spawn(?MODULE, r_handler, [Id, self(), N]) || Id <- lists:seq(0, N-1)].

r_multicast(Group, Message) ->
    % Send the message to all group members
    lists:foreach(fun(PID) -> PID ! {send, Message, self()} end, Group),
    % Wait for acknowledgments from all group members
    wait_for_acks(Group, length(Group)).

wait_for_acks([], _) ->
    ok;
wait_for_acks(Group, Count) when Count > 0 ->
    receive
        {ack, Pid} ->
            % Ack received, remove the PID from the list and decrement the count
            wait_for_acks(Group -- [Pid], Count - 1)
    after 5000 ->
        % Timeout occurred, resend to all members who have not acknowledged
        lists:foreach(fun(PID) -> PID ! {resend, self()} end, Group),
        wait_for_acks(Group, Count)
    end.

%%%%% RELIABLE HANDLER %%%%%
r_handler(Id, Sender, GroupSize) ->
    receive
        {send, Msg, Sender} ->
            % Simulate message delivery and send ack back to the sender
            io:format("Process ~w RECEIVED message: ~p~n", [Id, Msg]),
            Sender ! {ack, self()},
            r_handler(Id, Sender, GroupSize);
        {resend, Sender} ->
            % If a resend is requested, resend ack
            Sender ! {ack, self()},
            r_handler(Id, Sender, GroupSize)
    end.
