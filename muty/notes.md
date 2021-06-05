CONNECT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name jhon@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name ringo@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name paul@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name george@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name muty@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>

MUTY1:
c(muty1).
c(gui).
c(lock1).
c(worker).
muty1:start(lock1, 1000, 3000).
muty1:stop().

LOCK2:
c(muty1).
c(gui).
c(lock2).
c(worker).
muty1:start(lock2, 1000, 3000).
muty1:stop().

LOCK2 WITHOUT EXTRA REF

```erlang
{request, From, Ref, FromId} ->
    if MyId > FromId ->
        From ! {ok, Ref},
        wait(Nodes, Master, Refs, Waiting, TakeRef, MyId);
    true ->
        wait(Nodes, Master, Refs, [{From, Ref} | Waiting], TakeRef, MyId)
end;
```

LOCK3:
c(muty1).
c(gui).
c(lock3).
c(worker).
muty1:start(lock3, 1000, 3000).
muty1:stop().
