CLEAR CONSOLE: io:format("\ec").

CONNECT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name chordy@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>

NODE 1:
c(node1).
c(key).

N2 = node1:start(2).
N4 = node1:start(4,N2).
N0 = node1:start(0,N2).
N3 = node1:start(3,N2).
N1 = node1:start(1,N2).
N2 ! probe.

NODE 1_2:
c(node1).
c(key).

chordy:
register(n1, node1:start(1)).
n1 ! probe.
...
n1 ! probe.
...
n1 ! probe.
node2:
N2 = node1:start(2, {n1, 'chordy@127.0.0.1'}).
node3:
N3 = node1:start(3, {n1, 'chordy@127.0.0.1'}).

NODE 2:
c(chordy).
c(node2).
c(key).
c(storage).

N2 = node2:start(2).
P = chordy:connect(N2).
P ! {add,0,0}.
P ! {add,1,1}.
P ! {add,2,2}.
N2 ! probe.
N4 = node2:start(4,N2).
N2 ! probe.
N0 = node2:start(0,N2).
N2 ! probe.
N3 = node2:start(3,N2).
N2 ! probe.
N1 = node2:start(1,N2).
N2 ! probe.
P ! {add,3,3}.
P ! {lookup,3}.
N2 ! probe.

NODE 3:
c(chordy).
c(node3).
c(key).
c(storage).

N2 = node3:start(2).
N4 = node3:start(4,N2).
N0 = node3:start(0,N2).
N3 = node3:start(3,N2).
N1 = node3:start(1,N2).
N2 ! probe.
N1 ! stop.
N2 ! probe.
N3 ! stop.
N2 ! probe.
N0 ! stop.
N2 ! probe.
N4 ! stop.
N2 ! probe.

NODE 4:
c(chordy).
c(node4).
c(key).
c(storage).

N2 = node4:start(2).
P = chordy:connect(N2).
P ! {add,0,0}.
P ! {add,1,1}.
P ! {add,2,2}.
N2 ! probe.
N0 = node4:start(0,N2).
N2 ! probe.
N1 = node4:start(1,N2).
N2 ! probe.
N1 ! stop.
N2 ! probe.
N0 ! stop.
N2 ! probe.
