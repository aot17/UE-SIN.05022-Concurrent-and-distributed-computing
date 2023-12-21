% Chandy-Misra shortest path algorithm
-module(cmsk_with_ack).
-export([start/0,deploy/1,cm/3]).

deploy(Topology) ->
  % Create network (use registered names for communication)
  register(a, spawn(?MODULE, cm, [{1, a}, {undefined, 0, infinity}, [{b,5}, {c,1}, {e,4}]])),
  register(b, spawn(?MODULE, cm, [{2, b}, {undefined, 0, infinity}, []])),
  register(c, spawn(?MODULE, cm, [{3, c}, {undefined, 0, infinity}, [{e,2}, {d,1}]])),
  register(d, spawn(?MODULE, cm, [{4, d}, {undefined, 0, infinity},
    if
      % Cycle (not including source)
      Topology == 1 -> [{f,-5}];
      % Cycle (including source)
      Topology == 2 -> [{f,5}, {a,-3}];
      % No cycle
      true -> [{f,5}]
    end])),
  register(e, spawn(?MODULE, cm, [{5, e}, {undefined, 0, infinity}, [{b,1}, {d,5}]])),
  register(f, spawn(?MODULE, cm, [{6, f}, {undefined, 0, infinity}, [{b,6}, {g,2}]])),
  register(g, spawn(?MODULE, cm, [{7, g}, {undefined, 0, infinity}, [{d,2}]])),
  % Start algorithm from source
  a ! start.

% Each process has an identifier (Id) and a name (Name).
% We use names instead of Erlang pids to simplify communication and have readable logs.
% Identifiers are used to mirror the original algorithm and identify the source (process 1).
% Neighbours denote successors, i.e., outgoing edges only, not incoming ones.

cm({Id, Name}, {Pred, Num, D}, Neighbours) ->
    receive
      start ->
          lists:foreach(fun({NodeName, Weight}) ->
              io:format("[~p] Sent an initial message with distance ~p to [~p]~n", [Name, Weight, NodeName]),
              NodeName ! {Weight, Name}
          end, Neighbours),
          Num1 = length(Neighbours),
          cm({Id, Name}, {undefined, Num1, 0}, Neighbours);
      {ack} ->
          NewNum1 = Num - 1,
          if
              NewNum1 =:= 0 ->
                   lists:foreach(fun({NodeName, _Weight}) ->
                      io:format("[~p] Sending STOP message to [~p]~n", [Name, NodeName]),
                      NodeName ! {stop}
                  end, Neighbours);
              true ->
                   ok
          end,
          cm({Id, Name}, {Pred, NewNum1, D}, Neighbours);
      {stop} ->
        io:format("[~p] Shortest path distance: ~p~n", [Name, D]),
        cm({Id, Name}, {Pred, Num, D}, Neighbours);
      {ReceivedWeight, Sender} ->
          io:format("[~p] Received a message with weight ~p from [~p]~n", [Name, ReceivedWeight, Sender]),
          NewD = 
            case D of
              infinity -> ReceivedWeight;
              _ when ReceivedWeight < D -> ReceivedWeight;
              _ -> D
            end,
          if 
              NewD < D ->
                  lists:foreach(fun({NodeName, Weight}) ->
                      if Num > 0 -> Sender ! {ack}, io:format("[~p] sends ~p to [~p] ~n", [Name, ack, Sender]); true -> ok end,
                      %io:format("Num[~p] ~p ~n", [Name, Num]),
                      NewDist = NewD + Weight,
                      io:format("[~p] Sent a message with distance ~p to [~p]~n", [Name, NewDist, NodeName]),
                      NodeName ! {NewDist, Name}
                  end, Neighbours),
                  NewNum = Num + length(Neighbours),
                  %io:format("NewNum[~p] ~p ~n", [Name, NewNum]),
                  cm({Id, Name}, {Sender, NewNum, NewD}, Neighbours);
              true ->
                  Sender ! {ack},
                  cm({Id, Name}, {Sender, Num, D}, Neighbours)
          end
    end.

start() ->
  io:format("Deploying~n"),
  deploy(0),
  io:format("Starting~n").

% I litterally spent 20h on this week's serie trying to make it work. 
% Ex2 runs good but Ex3 is unfortunately not working perfectly.
% I tried a lot of options but I think I'm missing something in the algorithm understanding.
% Would be helpful to have an explenation. 
% I hope it won't be too problematic.
