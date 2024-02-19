% Ring of N processes passing M messages around ring
-module(trsk_AntoineTissot).
-export([start/2,start/0]).
-export([node/2,node/3]).

% 1. Initially function 'start()' spawns the first node P0 (master process).
% 2. P0 spawns next node indicating P0's ID and 'N-1' nodes remaining to create.
%    It then calls 'ring()' to process messages, with its child's ID and M as total number of tokens.
% 3. P1 similarly spawns next node (with P0's ID and one less node to create) and calls 'ring()'
% 4. The last process in the ring sends an 'init' message to P0 and calls 'ring()'.
%
% 5. Upon receiving 'init', P0 generates the first token and sends message 'token' to its child with counter 0 and its own ID.
%    It then recursively calls 'ring()' to process the subsequent messages, with its child's ID and M as total number of tokens.
% 6. Upon receiving 'token', each node sends a new message 'token' to its child with counter incremented by 1 and its own ID.
%    It then recursively calls 'ring()' to process the subsequent messages, with its child's ID and M-1 as remaining number of tokens.
% 7. When calling 'ring()' with 0 as number of remaining token, the nodes output their data and terminate.
%    P0 is the last process to receive and process the tokens, it therefore terminates last.

% Main function called to create the ring
start(N, M) ->
  spawn(?MODULE, node, [N, M]),
  ok.

start() ->
  spawn(?MODULE, node, [6, 3]),
  ok.

% Setup of the ring (first/master process)
node(N, M) ->
  io:format("~p: Process starting (master)~n", [self()]),
  Succ = spawn(?MODULE, node, [N-1, M, self()]),
  ring(Succ, M).

% Last process in the ring
node(1, M, Ring0_Pid) ->
  io:format("~p: Process starting (last)~n", [self()]),
  Ring0_Pid ! init,
  ring(Ring0_Pid, M);

% Other processes in the ring
node(N, M, Ring0_Pid) ->
  io:format("~p: Process starting~n", [self()]),
  Succ = spawn(?MODULE, node, [N-1, M, Ring0_Pid]),
  ring(Succ, M).

% Function to terminates processes
ring(Succ, 0)->
  io:format("~p: Process exiting, terminated by ~p ~n", [self(),Succ]);

% Function to process messages in the ring
ring(Succ, M)->
  receive 
    init -> 
      Succ ! {token, 0, self()}, 
      io:format("~p: received init and sends token ~p to ~p~n", [self(), M, Succ]),
      ring(Succ, M);
    {token, N, Pred} -> 
      Succ ! {token, N+1, self()}, 
      io:format("~p: received token ~p from ~p and sends it to ~p ~n", [self(), M, Pred, Succ]), 
      ring(Succ, M-1)
  end.