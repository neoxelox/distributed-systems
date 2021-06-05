-module(muty1_old).

-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    spawn("jhon@127.0.0.1", fun() -> register(l, Lock:start(1)),  register(w, worker:start("John", l, Sleep, Work)) end),
    spawn("ringo@127.0.0.1", fun() -> register(l, Lock:start(1)),  register(w, worker:start("Ringo", l, Sleep, Work)) end),
    spawn("paul@127.0.0.1", fun() -> register(l, Lock:start(1)),  register(w, worker:start("Paul", l, Sleep, Work)) end),
    spawn("george@127.0.0.1", fun() -> register(l, Lock:start(1)),  register(w, worker:start("George", l, Sleep, Work)) end),
    {l, "jhon@127.0.0.1"} ! {peers, [{l, "ringo@127.0.0.1"}, {l, "paul@127.0.0.1"}, {l, "george@127.0.0.1"}]},
    {l, "ringo@127.0.0.1"} ! {peers, [{l, "jhon@127.0.0.1"}, {l, "paul@127.0.0.1"}, {l, "george@127.0.0.1"}]},
    {l, "paul@127.0.0.1"} ! {peers, [{l, "jhon@127.0.0.1"}, {l, "ringo@127.0.0.1"}, {l, "george@127.0.0.1"}]},
    {l, "george@127.0.0.1"} ! {peers, [{l,"jhon@127.0.0.1"}, {l, "ringo@127.0.0.1"}, {l, "paul@127.0.0.1"}]},
    ok.

stop() ->
    {w, "jhon@127.0.0.1"} ! stop,
    {w, "ringo@127.0.0.1"} ! stop,
    {w, "paul@127.0.0.1"} ! stop,
    {w, "george@127.0.0.1"} ! stop.
