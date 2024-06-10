%%%-------------------------------------------------------------------
%% @doc location_request public API
%% @end
%%%-------------------------------------------------------------------

-module(location_request_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    location_request_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
