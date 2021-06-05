-module(node4).

-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),
    Store = storage:create(),
    ReplicaStore = storage:create(),
    node(MyKey, Predecessor, Successor, nil, Store, ReplicaStore).

connect(MyKey, nil) ->
    {ok, {MyKey, nil, self()}};
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Sref = monit(PeerPid),
            {ok, {Skey, Sref, PeerPid}}
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {notify, NewPeer} ->
            {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Store),
            {_Skey, _Sref, Spid} = Successor,
            Spid ! {pushreplica, NewStore},
            node(MyKey, NewPredecessor, Successor, NextSuccessor, NewStore, ReplicaStore);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {status, Pred, Nx} ->
            {NewSuccessor, NewNextSuccessor} = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, NewNextSuccessor, Store, ReplicaStore);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, NextSuccessor, Added, ReplicaStore);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {handover, Elements} ->
            NewStore = storage:merge(Store, Elements),
            {_Skey, _Sref, Spid} = Successor,
            Spid ! {pushreplica, NewStore},
            node(MyKey, Predecessor, Successor, NextSuccessor, NewStore, ReplicaStore);
        {'DOWN', Ref, process, _, _} ->
            {NewPredecessor, NewSuccessor, NewNextSuccessor, NewStore, NewReplicaStore} =
                down(Ref, Predecessor, Successor, NextSuccessor, Store, ReplicaStore),
            node(MyKey, NewPredecessor, NewSuccessor, NewNextSuccessor, NewStore, NewReplicaStore);
        {replicate, Key, Value, Qref, Client} ->
            NewReplicaStore = storage:add(Key, Value, ReplicaStore),
            Client ! {Qref, ok},
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, NewReplicaStore);
        {pushreplica, NewReplicaStore} ->
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, NewReplicaStore);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store, ReplicaStore),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore);
        {probe, RefKey, Nodes, T} ->
            forward_probe(MyKey, RefKey, [MyKey | Nodes], T, Successor, Store, ReplicaStore),
            node(MyKey, Predecessor, Successor, NextSuccessor, Store, ReplicaStore)
    end.

stabilize(Predecessor, NextSuccessor, MyKey, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Predecessor of
        nil ->
            Spid ! {notify, {MyKey, self()}},
            {Successor, NextSuccessor};
        {MyKey, _Mypid} ->
            {Successor, NextSuccessor};
        {Skey, _Spid} ->
            Spid ! {notify, {MyKey, self()}},
            {Successor, NextSuccessor};
        {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    demonit(Sref),
                    Xref = monit(Xpid),
                    self() ! stabilize,
                    {{Xkey, Xref, Xpid}, {Skey, Spid}};
                false ->
                    Spid ! {notify, {MyKey, self()}},
                    {Successor, NextSuccessor}
            end
    end.

stabilize({_Skey, _Sref, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _Sref, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            KeepStore = handover(Store, MyKey, Nkey, Npid),
            Nref = monit(Npid),
            {{Nkey, Nref, Npid}, KeepStore};
        {Pkey, Pref, _Ppid} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    KeepStore = handover(Store, MyKey, Nkey, Npid),
                    demonit(Pref),
                    Nref = monit(Npid),
                    {{Nkey, Nref, Npid}, KeepStore};
                false ->
                    {Predecessor, Store}
            end
    end.

add(Key, Value, Qref, Client, _MyKey, nil, {_Skey, _Sref, Spid}, Store) ->
    Spid ! {add, Key, Value, Qref, Client},
    Store;
add(Key, Value, Qref, Client, MyKey, {Pkey, _Pref, _Ppid}, {_Skey, _Sref, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of
        true ->
            Added = storage:add(Key, Value, Store),
            Spid ! {replicate, Key, Value, Qref, Client},
            Added;
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, _MyKey, nil, {_Skey, _Sref, Spid}, _Store) ->
    Spid ! {lookup, Key, Qref, Client};
lookup(Key, Qref, Client, MyKey, {Pkey, _Pref, _Ppid}, {_Skey, _Sref, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;
demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_Pkey, Ref, _Ppid}, {Skey, Sref, Spid}, Next, Store, Replica) ->
    NewStore = storage:merge(Store, Replica),
    NewReplica = storage:create(),
    Spid ! {pushreplica, NewStore},
    {nil, {Skey, Sref, Spid}, Next, NewStore, NewReplica};
down(Ref, Predecessor, {_Skey, Ref, _Spid}, {Nkey, Npid}, Store, Replica) ->
    Nref = monit(Npid),
    self() ! stabilize,
    {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.

create_probe(MyKey, {_Skey, _Sref, Spid}, Store, Replica) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Node ~w created probe -> Store: ~w -> Replica: ~w~n", [MyKey, Store, Replica]).

remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T, native, microsecond),
    io:format("Node ~w received probe after ~w us -> Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(MyKey, RefKey, Nodes, T, {_Skey, _Sref, Spid}, Store, Replica) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Node ~w forwarded probe started by node ~w -> Store: ~w -> Replica: ~w~n",
              [MyKey, RefKey, Store, Replica]).
