CONNECT SERVER 1:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name <SERVER_INSTANCE_NAME>@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
c(server). %% Compile server module
server:start().

CONNECT CLIENT:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name <CLIENT_INSTANCE_NAME>@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
c(client). %% Compile client module
client:start({<SERVER_PROCESS_NAME>, '<SERVER_INSTANCE_NAME>@<SERVER_IP>'}, "John").

DISCONNECT SERVER:
myserver ! disconnect.

CONNECT SERVER 2:
erl -kernel inet_dist_listen_min 1025 -kernel inet_dist_listen_max 1125 -name <SERVER_INSTANCE_NAME>@<YOUR_EXTERNAL_IP> -setcookie <PASSWORD>
c(server2). %% Compile server module
server2:start(). %% BOOT SERVER
server2:start({<SERVER_PROCESS_NAME>,'<SERVER_INSTANCE_NAME>@<SERVER_IP>'}). %% OTHER SERVERS
