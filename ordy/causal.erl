-module(causal).

-export([start/3]).

start(Id, Master, Jitter) ->
    spawn(fun() -> init(Id, Master, Jitter) end).

init(Id, Master, Jitter) ->
    receive
        {peers, Nodes} ->
            MyVC = newVC(length(Nodes), []),
            server(Id, Master, lists:delete(self(), Nodes), Jitter, MyVC, [])
    end.

server(MyId, Master, Nodes, Jitter, MyVC, Queue) ->
    receive
        {send, Msg} ->
            NewVC = incrementVC(MyId, MyVC),
            multicast(Msg, Nodes, Jitter, MyId, NewVC),
            Master ! {deliver, Msg},
            server(MyId, Master, Nodes, Jitter, NewVC, Queue);
        {multicast, Msg, FromId, FromVC} ->
            case checkMsg(FromId, FromVC, MyVC, size(MyVC)) of
                ready ->
                    Master ! {deliver, Msg},
                    NewVC = incrementVC(FromId, MyVC),
                    {NewerVC, NewQueue} = deliverReadyMsgs(Master, NewVC, Queue, Queue),
                    server(MyId, Master, Nodes, Jitter, NewerVC, NewQueue);
                wait ->
                    server(MyId, Master, Nodes, Jitter, MyVC, [{FromId, FromVC, Msg} | Queue])
            end;
        stop ->
            ok
    end.

multicast(Msg, Nodes, 0, MyId, MyVC) ->
    lists:foreach(fun(Node) -> Node ! {multicast, Msg, MyId, MyVC} end, Nodes);
multicast(Msg, Nodes, Jitter, MyId, MyVC) ->
    lists:foreach(fun(Node) ->
                     T = rand:uniform(Jitter),
                     timer:send_after(T, Node, {multicast, Msg, MyId, MyVC})
                  end,
                  Nodes).

%% Create a new vector clock with all positions initialized to 0
newVC(0, List) ->
    list_to_tuple(List);
newVC(N, List) ->
    newVC(N - 1, [0 | List]).

%% Increment position N of vector clock VC
incrementVC(NId, VC) ->
    setelement(NId, VC, element(NId, VC) + 1).

%% Check if a message can be delivered to the master
checkMsg(_FromId, _FromVC, _MyVC, 0) ->
    ready;
checkMsg(FromId, FromVC, MyVC, FromId) ->
    if element(FromId, FromVC) == element(FromId, MyVC) + 1 ->
           checkMsg(FromId, FromVC, MyVC, FromId - 1);
       true ->
           wait
    end;
checkMsg(FromId, FromVC, MyVC, NId) ->
    if element(NId, FromVC) =< element(NId, MyVC) ->
           checkMsg(FromId, FromVC, MyVC, NId - 1);
       true ->
           wait
    end.

%% Deliver to the master all the ready messages in the hold-back queue
deliverReadyMsgs(_Master, MyVC, [], Queue) ->
    {MyVC, Queue};
deliverReadyMsgs(Master, MyVC, [{FromId, FromVC, Msg} | Rest], Queue) ->
    case checkMsg(FromId, FromVC, MyVC, size(MyVC)) of
        ready ->
            Master ! {deliver, Msg},
            NewVC = incrementVC(FromId, MyVC),
            NewQueue = lists:delete({FromId, FromVC, Msg}, Queue),
            deliverReadyMsgs(Master, NewVC, NewQueue, NewQueue);
        wait ->
            deliverReadyMsgs(Master, MyVC, Rest, Queue)
    end.
