-module(gcfifo).
-export([start/0, f_handler/1]).

start() ->
    testf(),
    ok.

testf() ->
    io:format("~n======= TESTING F_MULTICAST =======~n"),
    FifoGroup = fifoCreateProcesses(4),
    % Send messages out of order with random delays
    spawn(fun() -> f_multicast(FifoGroup, 3), timer:sleep(rand:uniform(1000)) end),
    spawn(fun() -> f_multicast(FifoGroup, 1), timer:sleep(rand:uniform(1000)) end),
    spawn(fun() -> f_multicast(FifoGroup, 2), timer:sleep(rand:uniform(1000)) end).


fifoCreateProcesses(N) ->
    [spawn(?MODULE, f_handler, [Id]) || Id <- lists:seq(0, N-1)].

f_multicast(Group, Num) ->
    [PID ! {f_message, Num, self()} || PID <- Group].

%%%%% FIFO HANDLER %%%%%
f_handler(Id) ->
    f_handler(Id, 0, []).

f_handler(Id, LastDelivered, Buffer) ->
    receive
        {f_message, Num, Sender} ->
            if Num == LastDelivered + 1 ->
                % This is the next message we expect, so deliver it
                io:format("Process ~w RECEIVED and DELIVERED F-message ~w~n", [Id, Num]),
                f_handler(Id, Num, Buffer); % Increment LastDelivered
            true ->
                % Message is out of order, buffer it
                io:format("Process ~w BUFFERED F-message ~w~n", [Id, Num]),
                f_handler(Id, LastDelivered, [{Num, Sender} | Buffer])
            end
    after 1000 ->
        % Check if we can deliver any buffered messages
        {NewLast, NewBuffer} = checkBuffer(LastDelivered, Buffer, []),
        f_handler(Id, NewLast, NewBuffer)
    end.

checkBuffer(LastDelivered, Buffer, NewBuffer) ->
    case lists:keytake(LastDelivered + 1, 1, Buffer) of
        {value, {NextNum, _Sender}, RemainingBuffer} ->
            % We found the next message in the buffer
            io:format("Delivering BUFFERED F-message ~w~n", [NextNum]),
            checkBuffer(NextNum, RemainingBuffer, NewBuffer);
        false ->
            % No more buffered messages can be delivered
            {LastDelivered, Buffer ++ NewBuffer}
    end.
