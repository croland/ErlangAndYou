-module(client).

-export([go/0]).

go() ->
  spawn_link(fun() -> init() end).

init() ->
  server:new(self())).
