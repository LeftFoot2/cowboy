-module(db_api).
-export([put_location/4]).

put_location(Location_ID, Latitude, Longitude, Pid) ->
    Location = riakc_obj:new(<<"locations">>, Location_ID, {Latitude, Longitude}),
    riak:put(Pid, Location).
    
% deliver_package(Package_ID,Pid) ->
% 	Package = riakc_obj:new(<<"deliveries">>, Package_ID),
% 	riakc_pb_socket:put(Pid, Package).

% get_status(Package_ID, Pid) ->
% 	{ok, FetchedObject} = riak:get(Pid, <<"package">>, Package_ID),
% 	%% Extract the value from the object
% 	riak_object:get_key(FetchedObject).