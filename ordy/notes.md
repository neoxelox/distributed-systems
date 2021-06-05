CLEAR CONSOLE: io:format("\ec").

CONNECT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p1@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p2@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p3@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name p4@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name ordy@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>

BASIC:
c(ordy).
c(worker).
c(basic).
ordy:start(basic, 1000, 1000, 2000).
ordy:pause().
ordy:resume().
ordy:stop().

CAUSAL:
c(ordy).
c(worker).
c(causal).
ordy:start(causal, 1000, 100, 6000).
ordy:pause().
ordy:resume().
ordy:stop().

TOTAL:
c(ordy).
c(worker).
c(seq).
c(total).
ordy:start(total, 4000, 100, 6000).
ordy:pause().
ordy:resume().
ordy:stop().
