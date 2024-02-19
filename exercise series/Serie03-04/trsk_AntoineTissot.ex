defmodule TokenRingSkeleton do
  # 1. Initially function 'start()' spawns the first node P0 (master process).
  # 2. P0 spawns next node indicating P0's ID and 'N-1' nodes remaining to create.
  #    It then calls 'ring()' to process messages, with its child's ID and M as total number of tokens.
  # 3. P1 similarly spawns next node (with P0's ID and one less node to create) and calls 'ring()'
  # 4. The last process in the ring sends an 'init' message to P0 and calls 'ring()'.
  #
  # 5. Upon receiving 'init', P0 generates the first token and sends message 'token' to its child with counter 0 and its own ID.
  #    It then recursively calls 'ring()' to process the subsequent messages, with its child's ID and M as total number of tokens.
  # 6. Upon receiving 'token', each node sends a new message 'token' to its child with counter incremented by 1 and its own ID.
  #    It then recursively calls 'ring()' to process the subsequent messages, with its child's ID and M-1 as remaining number of tokens.
  # 7. When calling 'ring()' with 0 as number of remaining token, the nodes output their data and terminate.
  #    P0 is the last process to receive and process the tokens, it therefore terminates last.

  # Main function called to create the ring
  def start(n, m) do
    spawn(__MODULE__, :node, [n, m])
  end

  def start() do
    spawn(__MODULE__, :node, [6, 3])
  end

  # Setup of the ring (first/master process)
  def node(n, m) do
    IO.puts("#{self() |> :erlang.pid_to_list()}: Process starting (master)")
    succ = spawn(__MODULE__, :node, [n-1, m, self()])
    ring(succ, m)
  end

  # Last process in the ring
  def node(1, m, ring0_pid) do
    IO.puts("#{self() |> :erlang.pid_to_list()}: Process starting (last)")
    send(ring0_pid, :init)
    ring(ring0_pid, m)
  end

  # Other processes in the ring
  def node(n, m, ring0_pid) do
    IO.puts("#{self() |> :erlang.pid_to_list()}: Process starting")
    succ = spawn(__MODULE__, :node, [n-1, m, ring0_pid])
    ring(succ, m)
  end

  # Function to terminates processes
  def ring(succ, 0) do
    IO.puts("#{self()|> :erlang.pid_to_list()} Process exiting, terminated by #{succ|> :erlang.pid_to_list()}")
  end

  # Function to process messages in the ring
  def ring(succ, m) do
    receive do
      :init ->
        send(succ, {:token, 0, self()})
        IO.puts("#{self() |> :erlang.pid_to_list()} received init and sends token #{m} to #{succ |> :erlang.pid_to_list()}")
        ring(succ, m)

      { :token, n, pred } ->
        send(succ, {:token,n+1, self()})
        IO.puts("#{self() |> :erlang.pid_to_list()} received token #{m} from #{pred |> :erlang.pid_to_list()} and sends it to #{succ |> :erlang.pid_to_list()}")
        ring(succ, m - 1)
    end
  end
end
