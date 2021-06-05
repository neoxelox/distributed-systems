-module(node2).

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
    Storage = storage:create(),
    node(MyKey, Predecessor, Successor, Storage).

connect(MyKey, nil) ->
    {ok, {MyKey, self()}};
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, PeerPid}}
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store);
        {notify, NewPeer} ->
            {NewPredecessor, NewStorage} = notify(NewPeer, MyKey, Predecessor, Store),
            node(MyKey, NewPredecessor, Successor, NewStorage);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(MyKey, Predecessor, Successor, Store);
        {status, Pred} ->
            NewSuccessor = stabilize(Pred, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, Store);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);
        {handover, Elements} ->
            NewStorage = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, NewStorage);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store);
        {probe, RefKey, Nodes, T} ->
            forward_probe(MyKey, RefKey, [MyKey | Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Store)
    end.

stabilize(Predecessor, MyKey, Successor) ->
    {Skey, Spid} = Successor,
    case Predecessor of
        nil ->
            Spid ! {notify, {MyKey, self()}},
            Successor;
        {MyKey, _Mypid} ->
            Successor;
        {Skey, _Spid} ->
            Spid ! {notify, {MyKey, self()}},
            Successor;
        {Xkey, _Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    self() ! stabilize,
                    Predecessor;
                false ->
                    Spid ! {notify, {MyKey, self()}},
                    Successor
            end
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            KeepStorage = handover(Store, MyKey, Nkey, Npid),
            {{Nkey, Npid}, KeepStorage}; %% TODO: ADD SOME CODE
        {Pkey, _Ppid} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    KeepStorage = handover(Store, MyKey, Nkey, Npid), %% TODO: ADD SOME CODE
                    {{Nkey, Npid}, KeepStorage};  %% TODO: ADD SOME CODE
                false ->
                    {Predecessor, Store}
            end
    end.

add(Key, Value, Qref, Client, _MyKey, nil, {_Skey, Spid}, Store) ->
    Spid ! {add, Key, Value, Qref, Client}, %% TODO: ADD SOME CODE
    Store;
add(Key, Value, Qref, Client, MyKey, {Pkey, _Ppid}, {_Skey, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) %% TODO: ADD SOME CODE
    of
        true ->
            Added = storage:add(Key, Value, Store), %% TODO: ADD SOME CODE
            Client ! {Qref, ok},
            Added;
        false ->
            Spid ! {add, Key, Value, Qref, Client}, %% TODO: ADD SOME CODE
            Store
    end.

lookup(Key, Qref, Client, _MyKey, nil, {_Skey, Spid}, _Store) ->
    Spid ! {lookup, Key, Qref, Client}; %% TODO: ADD SOME CODE
lookup(Key, Qref, Client, MyKey, {Pkey, _Ppid}, {_Skey, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) %% TODO: ADD SOME CODE
    of
        true ->
            Result = storage:lookup(Key, Store), %% TODO: ADD SOME CODE
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client} %% TODO: ADD SOME CODE
    end.

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

create_probe(MyKey, {_Skey, Spid}, Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Node ~w created probe -> Store: ~w~n", [MyKey, Store]).

remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T, native, microsecond),
    io:format("Node ~w received probe after ~w us -> Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(MyKey, RefKey, Nodes, T, {_Skey, Spid}, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Node ~w forwarded probe started by node ~w -> Store: ~w~n",
              [MyKey, RefKey, Store]).
