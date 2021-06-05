CLEAR CONSOLE: io:format("\ec").

CONNECT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p1@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p2@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p3@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p4@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p5@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name groupy@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>

STOP:
groupy:stop().
kill -9 <PID INSTANCE>

LEADER CRASH: ctrl+c

GMS1:
c(groupy).
c(gui).
c(worker).
c(gms1).
groupy:start(gms1, 1000).

GMS2:
c(groupy).
c(gui).
c(worker).
c(gms2).
groupy:start(gms2, 1000).

GMS3:
c(groupy).
c(gui).
c(worker).
c(gms3).
groupy:start(gms3, 1000).
