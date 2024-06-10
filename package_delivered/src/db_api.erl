-module(db_api).
-export([get_status/2, deliver_package/2]).


deliver_package(Package_ID,Pid) ->
	Package = riakc_obj:new(<<"deliveries">>, Package_ID),
	riakc_pb_socket:put(Pid, Package).

get_status(Package_ID, Pid) ->
	{ok, FetchedObject} = riak:get(Pid, <<"packages">>, Package_ID),
	%% Extract the value from the object
	riak_object:get_key(FetchedObject).