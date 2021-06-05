CLEAR CONSOLE: io:format("\ec").

CONNECT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name root@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name edu@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name upc@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name hosts@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name client@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>

STOP:
ctrl+c abort
kill -9 <PID INSTANCE>

    - root
    serverN:stop().
    - edu
    serverN:stop().
    - upc
    serverN:stop().
    - hosts
    hostN:stop(www).
    hostN:stop(ftp).
    - client
    resolver:stop().

PART 1:
c(cache).
c(entry).
c(host1).
c(namy).
c(resolver).
c(server1).

    - root
    server1:start().

    - edu
    server1:start(edu, {server, 'root@127.0.0.1'}).

    - upc
    server1:start(upc, {server, 'edu@127.0.0.1'}).

    - hosts
    host1:start(www, www, {server, 'upc@127.0.0.1'}).
    host1:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

    - client
    resolver:start({server, 'root@127.0.0.1'}).
    namy:ping([ftp, upc, edu], resolver).
    namy:ping([www, upc, edu], resolver).
    namy:ping([bad, upc, edu], resolver).

PART 2:
c(cache).
c(entry).
c(host).
c(namy).
c(resolver).
c(server).

    - root
    server:start().

    - edu
    server:start(edu, {server, 'root@127.0.0.1'}).

    - upc
    server:start(upc, {server, 'edu@127.0.0.1'}).
    server ! status.

    - hosts
    host:start(www, www, {server, 'upc@127.0.0.1'}).
    host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

    - client
    resolver:start({server, 'root@127.0.0.1'}).
    namy:ping([ftp, upc, edu], resolver).
    namy:ping([www, upc, edu], resolver).
    namy:ping([bad, upc, edu], resolver).

PART 3:
c(cache).
c(entry).
c(host).
c(namy).
c(resolver).
c(server).

    - root
    server:start().
    server ! {ttl, 120}.

    - edu
    server:start(edu, {server, 'root@127.0.0.1'}).
    server ! {ttl, 120}.

    - upc
    server:start(upc, {server, 'edu@127.0.0.1'}).
    server ! {ttl, 120}.

    - hosts
    host:start(www, www, {server, 'upc@127.0.0.1'}).
    host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

    - client
    resolver:start({server, 'root@127.0.0.1'}).
    namy:ping([ftp, upc, edu], resolver).
    namy:ping([www, upc, edu], resolver).
    namy:ping([bad, upc, edu], resolver).

c(cache3).
c(entry).
c(host).
c(namy).
c(resolver3).
c(server).

    - root
    server:start().
    server ! {ttl, 120}.

    - edu
    server:start(edu, {server, 'root@127.0.0.1'}).
    server ! {ttl, 120}.

    - upc
    server:start(upc, {server, 'edu@127.0.0.1'}).
    server ! {ttl, 120}.

    - hosts
    host:start(www, www, {server, 'upc@127.0.0.1'}).
    host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

    - client
    resolver3:start({server, 'root@127.0.0.1'}).
    namy:ping([ftp, upc, edu], resolver).
    namy:ping([www, upc, edu], resolver).
    namy:ping([bad, upc, edu], resolver).
    resolver ! status.
    resolver ! purge.

PART 4:
cd recursive
c(cache).
c(entry).
c(host).
c(namy).
c(resolver).
c(server).

    - root
    server:start().
    server ! {ttl, 120}.

    - edu
    server:start(edu, {server, 'root@127.0.0.1'}).
    server ! {ttl, 120}.

    - upc
    server:start(upc, {server, 'edu@127.0.0.1'}).
    server ! {ttl, 120}.

    - hosts
    host:start(www, www, {server, 'upc@127.0.0.1'}).
    host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).

    - client
    resolver:start({server, 'root@127.0.0.1'}).
    namy:ping([ftp, upc, edu], resolver).
    namy:ping([www, upc, edu], resolver).
    namy:ping([bad, upc, edu], resolver).
