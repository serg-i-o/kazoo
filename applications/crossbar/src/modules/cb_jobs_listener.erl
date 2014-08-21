%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cb_jobs_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,publish_new_job/2
         ,handle_job/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("../crossbar.hrl").

-record(state, {}).

-define(APP_ROUTING, wh_util:to_binary(?MODULE)).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'delegate', [{'app_name', ?APP_ROUTING}]}]).
-define(RESPONDERS, [{{?MODULE, 'handle_job'}
                      ,{<<"delegate">>, <<"job">>}
                     }
                    ]).
-define(QUEUE_NAME, <<"resource_jobs_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[]
                           ).

-spec publish_new_job(ne_binary(), ne_binary()) -> 'ok'.
publish_new_job(AccountId, JobId) ->
    Work = wh_json:from_list([{<<"Account-ID">>, AccountId}
                              ,{<<"Job-ID">>, JobId}
                             ]),
    wh_amqp_worker:cast([{<<"Delegate-Message">>, Work}]
                        ,fun(API) -> wapi_delegate:publish_delegate(?APP_ROUTING, API) end
                       ).

-spec handle_job(wh_json:object(), wh_proplist()) -> 'ok'.
handle_job(JObj, _Props) ->
    'true' = wapi_delegate:delegate_v(JObj),
    Job = wh_json:get_value(<<"Delegate-Message">>, JObj),
    process_job(wh_json:get_value(<<"Account-ID">>, Job)
                ,wh_json:get_value(<<"Job-ID">>, Job)
               ).

-spec process_job(ne_binary(), ne_binary()) -> 'ok'.
process_job(<<_/binary>> = AccountId, <<_/binary>> = JobId) ->
    wh_util:put_callid(JobId),

    JobModb = job_modb(AccountId, JobId),
    {'ok', Job} = couch_mgr:open_cache_doc(JobModb, JobId),

    lager:debug("processing job ~s for account ~s", [JobId, AccountId]),

    maybe_start_job(Job, wh_json:get_value(<<"pvt_status">>, Job)).

-spec maybe_start_job(wh_json:object(), ne_binary()) -> 'ok'.
maybe_start_job(_Job, <<"complete">>) ->
    lager:debug("job is complete, nothing to do");
maybe_start_job(Job, <<"pending">>) ->
    lager:debug("job is pending, let's execute it!"),
    start_job(update_status(Job, <<"running">>)
              ,wh_json:get_value(<<"carrier">>, Job)
              ,wh_json:get_value(<<"numbers">>, Job)
             );
maybe_start_job(Job, <<"running">>) ->
    lager:debug("job is running, ~s is in charge", [wh_json:get_value(<<"pvt_node">>, Job)]).

-spec start_job(wh_json:object(), atom(), ne_binaries()) -> 'ok'.
start_job(Job, _CarrierModule, []) ->
    update_status(Job, <<"complete">>),
    lager:debug("successfully finished job");
start_job(_Job, _CarrierModule, _Numbers) ->
    'ok'.

-spec update_status(wh_json:object(), ne_binary()) -> wh_json:object().
update_status(Job, Status) ->
    {'ok', Job1} = couch_mgr:save_doc(wh_json:get_value(<<"pvt_account_db">>, Job)
                                      ,wh_json:set_values([{<<"pvt_status">>, Status}
                                                           ,{<<"pvt_node">>, wh_util:to_binary(node())}
                                                          ], Job)
                                     ),
    Job1.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec job_modb(ne_binary(), ne_binary()) -> ne_binary().
job_modb(AccountId, <<Year:4/binary, Month:2/binary, "-", _/binary>>) ->
    wh_util:format_account_mod_id(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month));
job_modb(AccountId, <<Year:4/binary, Month:1/binary, "-", _/binary>>) ->
    wh_util:format_account_mod_id(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)).
