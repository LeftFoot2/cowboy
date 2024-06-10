%%%-------------------------------------------------------------------
%% @doc package_transfer public API
%% @end
%%%-------------------------------------------------------------------

-module(package_transfer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_transfer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
