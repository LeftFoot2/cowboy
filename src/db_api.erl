-module(db_api).
-export([put_package/3, get_location/2, deliver_package/2, get_status/2, put_location/4, get_lat_long/2]).


put_package(Package_ID, Location_ID, Pid) ->
	Object=riakc_obj:new(<<"packages">>, Package_ID, Location_ID),
	riakc_pb_socket:put(Pid, Object).

get_location(Package_ID, Pid) ->
	case riakc_pb_socket:get(Pid, <<"packages">>, Package_ID) of 
		{ok, Object} -> {ok, riakc_obj:get_value(Object)};
		{error,notfound} -> {error,notfound}
	end.


%% db functions for package_deliver and checking riak database
deliver_package(Package_ID,Pid) ->
	Object=riakc_obj:new(<<"deliveries">>, Package_ID, <<"delivered">>),
	riakc_pb_socket:put(Pid, Object).

get_status(Package_ID, Pid) ->
	case riakc_pb_socket:get(Pid, <<"deliveries">>, Package_ID) of 
		{ok, Object} -> {ok, riakc_obj:get_value(Object)};
		{error,notfound} -> {error,notfound}
	end.


%% db functions for location_update
put_location(Location_ID, Latitude, Longitude, Pid) ->
    %% Convert the tuple to a binary format
    Value = io_lib:format("{~p, ~p}", [Latitude, Longitude]),
    Object = riakc_obj:new(<<"locations">>, Location_ID, list_to_binary(Value)),
    riakc_pb_socket:put(Pid, Object).


%% db functions for location_request
get_lat_long(Location_ID, Pid) ->
    case riakc_pb_socket:get(Pid, <<"locations">>, Location_ID) of
        {ok, Object} ->
            ValueBin = riakc_obj:get_value(Object),
            {ok, parse_location(ValueBin)};
        {error, notfound} ->
            {error, notfound}
    end.


parse_location(ValueBin) ->
	ValueString = binary_to_list(ValueBin),
	{ok, [Latitude, Longitude]} = erl_scan:string(ValueString ++ "."),
	{value, {_, Latitude}} = erl_parse:parse_term(Latitude),
	{value, {_, Longitude}} = erl_parse:parse_term(Longitude),
	{Latitude, Longitude}.