-module(server).

-export([start/3, stop/1, rpc/2, loop/3, swap_code/2, log_error/3]).

start(Name, F, State) -> register(Name, spawn(fun() -> loop(Name, F, State) end)).

stop(Name) -> Name ! stop.
  
rpc(Name, Query) ->
  Name ! {self(), Query},
  receive
    {Name, crash} -> exit(rpc);
    {Name, ok, Reply} -> Reply
  end.

loop(Name, F, State) ->
  receive
    stop -> void;
    {From, Query} ->
      case (catch F(From, Query)) of 
        {'EXIT', Why} ->
          log_error(Name, Query, Why),
          From ! crash,
          loop(Name, F, State);
        {Reply, State1} ->
          From ! {Name, ok, Reply},
          loop(Name, F, State1)      
      end 
  end.


log_error(Name, Query, Why) ->
  io:format("Server ~p Query ~p Why ~p",[Name, Query, Why]).

swap_code(a, b) ->
  ok.
