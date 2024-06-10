%% package delivered gen_server and tests

-module(package_delivered).
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

handle_call({status, Package_ID}, _From, Db_PID) ->
    case Package_ID =:= <<"">> of
            true ->
                {reply,{fail,empty_key},Db_PID};
            _ ->
                {reply, db_api:get_status(Package_ID,Db_PID),Db_PID}
        end;
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

%%DELIVERED
% If key is empty, it doesn't deliver_package
handle_cast({deliver, <<"">>}, Db_PID) ->
    {noreply, Db_PID};
handle_cast({deliver, Package_ID}, Db_PID) ->
    db_api:deliver_package(Package_ID, Db_PID),
    {noreply, Db_PID};

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
         meck:expect(db_api, deliver_package, fun(_Package_ID, _Pid) -> worked end),
         meck:expect(db_api, get_status, fun(Package_ID, _Pid) ->
             case Package_ID of
                 <<"4">> -> <<"Delivered">>;
                 <<"5">> -> <<"Delivered">>;
                 <<"6">> -> <<"Delivered">>;
                 _ -> fail
             end
         end),
         ok
     end,
     fun(_) ->
         % This is the teardown fun.
         meck:unload(db_api)
     end,
     [
         % Add the packages into the mock database
         fun() ->
             package_delivered:handle_cast({deliver, <<"4">>}, some_Db_PID),
             package_delivered:handle_cast({deliver, <<"5">>}, some_Db_PID),
             package_delivered:handle_cast({deliver, <<"6">>}, some_Db_PID),
             package_delivered:handle_cast({deliver, <<"">>}, some_Db_PID),
             ok
         end,
         
         % Use get location call function to check where the packages are. Only for unit testing!
         fun() ->
             ?assertEqual({reply, <<"Delivered">>, some_Db_PID},
                          package_delivered:handle_call({status, <<"4">>}, some_from_pid, some_Db_PID))
         end,
         fun() ->
             ?assertEqual({reply, <<"Delivered">>, some_Db_PID},
                          package_delivered:handle_call({status, <<"5">>}, some_from_pid, some_Db_PID))
         end,
         fun() ->
             ?assertEqual({reply, <<"Delivered">>, some_Db_PID},
                          package_delivered:handle_call({status, <<"6">>}, some_from_pid, some_Db_PID))
         end,
        fun() ->
             ?assertEqual({reply, {fail, empty_key}, some_Db_PID},
                          package_delivered:handle_call({status, <<"">>}, some_from_pid, some_Db_PID))
         end
     ]}.


-endif.



