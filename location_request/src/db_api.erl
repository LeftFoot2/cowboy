-module(db_api).
-export([get_package/2,get_location/2]).



get_package(Package_ID, Pid) ->
    {ok, FetchedObject} = riak:get(Pid, <<"packages">>, Package_ID),
    riak_object:get_key(FetchedObject).

get_location(Location_ID, Pid) ->
    {ok, FetchedObject} = riak:get(Pid, <<"locations">>, Location_ID),
    riak_object:get_key(FetchedObject).