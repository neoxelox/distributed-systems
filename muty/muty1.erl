-module(muty1).

-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    Main = self(),

    spawn('jhon@127.0.0.1',
          fun() -> register(w, worker:start("John", Main, Lock, 1, Sleep, Work)) end),
    spawn('ringo@127.0.0.1',
          fun() -> register(w, worker:start("Ringo", Main, Lock, 2, Sleep, Work)) end),
    spawn('paul@127.0.0.1',
          fun() -> register(w, worker:start("Paul", Main, Lock, 3, Sleep, Work)) end),
    spawn('george@127.0.0.1',
          fun() -> register(w, worker:start("George", Main, Lock, 4, Sleep, Work)) end),

    collect(4, []).

collect(N, Locks) ->
    if N == 0 ->
           lists:foreach(fun(L) -> L ! {peers, lists:delete(L, Locks)} end, Locks);
       true ->
           receive
               {ready, L} ->
                   collect(N - 1, [L | Locks])
           end
    end.

stop() ->
    {w, 'jhon@127.0.0.1'} ! stop,
    {w, 'ringo@127.0.0.1'} ! stop,
    {w, 'paul@127.0.0.1'} ! stop,
    {w, 'george@127.0.0.1'} ! stop.
