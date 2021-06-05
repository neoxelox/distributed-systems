-module(cache).

-export([lookup/2, add/4, remove/2]).

lookup(Name, Cache) ->
    %% keyfind(key, key_elemnth, list_tuple)
    case lists:keyfind(Name, 1, Cache) of
        {_Name, Expiration, Entry} ->
            Now = erlang:convert_time_unit(
                      erlang:monotonic_time(), native, second),
            if Now < Expiration ->
                   Entry;
               true ->
                   invalid
            end;
        false ->
            unknown
    end.

add(Name, Expiration, Entry, Cache) ->
    %% keystore(key, key_elemnth, list_tuple, new_tuple)
    lists:keystore(Name, 1, Cache, {Name, Expiration, Entry}).

remove(Name, Cache) ->
    %% keydelete(key, key_elemnth, list_tuple)
    lists:keydelete(Name, 1, Cache).
