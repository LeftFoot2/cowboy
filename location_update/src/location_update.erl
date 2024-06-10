%% package transfer gen_server and tests

-module(location_update).
-behaviour(gen_server).
-export([]).



%% API
-export([start/0,start/3,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,replace_up}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.

handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
    {noreply, term(), integer()} |
    {stop, term(), term()}.

%%UPDATE
% If either key is empty, it doesn't put_location
handle_cast({update, <<"">>, _Latitude, _Longitude}, Db_PID) ->
    {noreply,failed,Db_PID};
handle_cast({update, _Location_ID, <<"">>, _Longitude}, Db_PID) ->
    {noreply,failed,Db_PID};
handle_cast({update, _Location_ID, _Latitude, <<"">>}, Db_PID) ->
    {noreply,failed,Db_PID};
handle_cast({update, Location_ID, Latitude, Longitude}, Db_PID) ->
    db_api:put_location(Location_ID, Latitude, Longitude, Db_PID),
    {noreply, worked, Db_PID};

handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
-include_lib("eunit/include/eunit.hrl").


transfer_test_() ->
    {setup,
     fun() ->
         % This setup fun is run once before the tests are run.
         meck:new(db_api),
         meck:expect(db_api, put_location, fun(_Location_ID, _Latitude, _Longitude, _Pid) -> worked end),
         ok
     end,
     fun(_) ->
         % This is the teardown fun.
         meck:unload(db_api)
     end,
     [
         % Add the packages into the mock database
         fun() ->
            ?assertEqual({noreply,worked,some_Db_PID},
                location_update:handle_cast({update, <<"Truck101">>, <<"67.2">>, <<"57.3">>}, some_Db_PID)),
             ?assertEqual({noreply,failed,some_Db_PID},
                location_update:handle_cast({update, <<"Plane201">>, <<"67.3">>, <<"">>}, some_Db_PID)),
             ?assertEqual({noreply,failed,some_Db_PID},
                location_update:handle_cast({update, <<"Ship301">>, <<"">>, <<"57.5">>}, some_Db_PID)),
             ?assertEqual({noreply,failed,some_Db_PID},
                location_update:handle_cast({update, <<"">>, <<"67.5">>, <<"57.2">>}, some_Db_PID))
         end
     ]}.


-endif.



