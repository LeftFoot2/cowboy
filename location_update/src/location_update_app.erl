%%%-------------------------------------------------------------------
%% @doc location_update public API
%% @end
%%%-------------------------------------------------------------------

-module(location_update_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    location_update_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
