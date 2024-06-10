-module(db_api).
-export([put_package/3, get_package/2]).


put_package(Package_ID, Location_ID, Pid) ->
	Request=riakc_obj:new(<<"packages">>, Package_ID, Location_ID),
	riakc_pb_socket:put(Pid, Request).

get_package(Package_ID, Pid) ->
	{ok, FetchedObject} = riak:get(Pid, <<"packages">>, Package_ID),
	riak_object:get_key(FetchedObject).