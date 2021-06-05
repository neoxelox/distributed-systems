-module(entry).

-export([lookup/2, add/3, remove/2]).

lookup(Name, Entries) ->
    %% keyfind(key, key_elemnth, list_tuple)
    case lists:keyfind(Name, 1, Entries) of
        {_Name, Entry} ->
            Entry;
        false ->
            unknown
    end.

add(Name, Entry, Entries) ->
    %% keystore(key, key_elemnth, list_tuple, new_tuple)
    lists:keystore(Name, 1, Entries, {Name, Entry}).

remove(Name, Entries) ->
    %% keydelete(key, key_elemnth, list_tuple)
    lists:keydelete(Name, 1, Entries).
