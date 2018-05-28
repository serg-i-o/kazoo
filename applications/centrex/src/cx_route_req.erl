%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Handler for route requests, responds if centrex Callflows match.
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cx_route_req).

-export([handle_req/2]).

-include("centrex.hrl").
%%-include("callflow.hrl").

-define(DEFAULT_METAFLOWS(AccountId)
        ,kapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, 'false')
        ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
%% TODO Create centrex system_config section
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

%%-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
%%handle_req(JObj, Props) ->
%%    _ = kz_util:put_callid(JObj),
%%    'true' = kapi_route:req_v(JObj),
%%    Routines = [fun maybe_referred_call/1
%%                ,fun maybe_device_redirected/1
%%               ],
%%    Call = kapps_call:exec(Routines, kapps_call:from_route_req(JObj)),
%%    io:format("\n~p.handle_req/2:\nCallId=~p\nProps=~p\n",[?MODULE,kz_doc:id(Call),Props]),
%%    case centrex_maintenance:is_centrex_account(Call)
%%    of
%%        'true' ->
%%            io:format("received request ~s asking if callflows can route the call to ~s\n"
%%                ,[kapi_route:fetch_id(JObj), kapps_call:request_user(Call)]
%%            ),
%%            lager:info("received request ~s asking if callflows can route the call to ~s"
%%                      ,[kapi_route:fetch_id(JObj), kapps_call:request_user(Call)]
%%                      ),
%%            AllowNoMatch = allow_no_match(Call),
%%            case cx_flow:lookup_centrex_callflow(Call) of
%%                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
%%                %% to use it for this call
%%                {'ok', Flow, NoMatch} when (not NoMatch)
%%                                           orelse AllowNoMatch ->
%%                    io:format("\nFlowId=~p\nProps=~p\nCallId=~p\nNoMatch=~p\nAllowNoMatch=~p\n",
%%                        [kz_doc:id(Flow),Props,kz_doc:id(Call),NoMatch,AllowNoMatch]),
%%                    maybe_prepend_preflow(JObj, Props, Call, Flow, NoMatch);
%%                {'ok', _, 'true'} ->
%%                    io:format("only available callflow is a nomatch for a unauthorized call\n", []),
%%                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
%%                {'error', R} ->
%%                    io:format("unable to find callflow ~p\n", [R]),
%%                    lager:info("unable to find callflow ~p", [R])
%%            end;
%%        'false' ->
%%            lager:debug("callflow not handling fetch-id ~s", [kapi_route:fetch_id(JObj)])
%%    end.


-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    'true' = kapi_route:req_v(JObj),
    Call = kapps_call:from_route_req(JObj),
    io:format("\n\n~p.handle_req/2:\nCallId=~p\nProps=~p\nCallJObj=~p\n",[?MODULE,kz_doc:id(Call),Props,JObj]),
    case centrex_maintenance:is_centrex_account(Call)
    of
        'true' ->
            io:format("received request ~s asking if callflows can route the call to ~s\n"
                ,[kapi_route:fetch_id(JObj), kapps_call:request_user(Call)]
            ),
            lager:info("received request ~s asking if callflows can route the call to ~s"
                ,[kapi_route:fetch_id(JObj), kapps_call:request_user(Call)]
            ),
            case cx_flow:lookup_centrex_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch) ->
                    io:format("\nFlowId=~p\nProps=~p\nCallId=~p\nNoMatch=~p\n",
                        [kz_doc:id(Flow),Props,kz_doc:id(Call),NoMatch]),
                    maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    io:format("only available callflow is a nomatch for a unauthorized call\n", []),
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    io:format("unable to find callflow ~p\n", [R]),
                    lager:info("unable to find callflow ~p", [R])
            end;
        'false' ->
            lager:debug("callflow not handling fetch-id ~s", [kapi_route:fetch_id(JObj)])
    end.

-spec maybe_reply_to_req(kz_json:object(), kz_term:proplist()
    ,kapps_call:call(), kz_json:object(), boolean()) -> 'ok'.
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request for ~s",
        [kz_doc:id(Flow)
        ,kapps_call:account_id(Call)
        ,kapps_call:request_user(Call)
    ]),
    ControllerQ = props:get_value('queue', Props),
    NewCall = update_call(Flow, NoMatch, ControllerQ, Call),
    send_route_response(Flow, JObj, NewCall).


%%-spec maybe_prepend_preflow(kz_json:object(), kz_term:proplist()
%%                           ,kapps_call:call(), kzd_callflow:doc()
%%                           ,boolean()
%%                           ) -> 'ok'.
%%maybe_prepend_preflow(JObj, Props, Call, Callflow, NoMatch) ->
%%    AccountId = kapps_call:account_id(Call),
%%    io:format("\n~p.maybe_prepend_preflow/5: \nAccountId=~p\n",[?MODULE,AccountId]),
%%    case kzd_accounts:fetch(AccountId) of
%%        {'error', _E} ->
%%            lager:warning("could not open account doc ~s : ~p", [AccountId, _E]),
%%            maybe_reply_to_req(JObj, Props, Call, Callflow, NoMatch);
%%        {'ok', AccountDoc} ->
%%            case kzd_accounts:preflow_id(AccountDoc) of
%%                'undefined' ->
%%                    io:format("ignore preflow, not set\n"),
%%                    lager:debug("ignore preflow, not set"),
%%                    maybe_reply_to_req(JObj, Props, Call, Callflow, NoMatch);
%%                PreflowId ->
%%                    NewCallflow = kzd_callflow:prepend_preflow(Callflow, PreflowId),
%%                    maybe_reply_to_req(JObj, Props, Call, NewCallflow, NoMatch)
%%            end
%%    end.

%%-spec maybe_reply_to_req(kz_json:object(), kz_term:proplist()
%%                        ,kapps_call:call(), kz_json:object(), boolean()) -> 'ok'.
%%maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
%%    lager:info("callflow ~s in ~s satisfies request for ~s", [kz_doc:id(Flow)
%%                                                             ,kapps_call:account_id(Call)
%%                                                             ,kapps_call:request_user(Call)
%%                                                             ]),
%%    io:format("\n~p.maybe_reply_to_req/5:  has_tokens=~p\n",[?MODULE, has_tokens(Call, Flow)]),
%%    case has_tokens(Call, Flow) of
%%        'false' -> 'ok';
%%        'true' ->
%%            ControllerQ = props:get_value('queue', Props),
%%            NewCall = update_call(Flow, NoMatch, ControllerQ, Call),
%%            send_route_response(Flow, JObj, NewCall)
%%    end.
%%
%%-spec has_tokens(kapps_call:call(), kz_json:object()) -> boolean().
%%has_tokens(Call, Flow) ->
%%    case kapps_config:get_is_true(?CF_CONFIG_CAT, <<"calls_consume_tokens">>, 'true') of
%%        'false' ->
%%            %% If configured to not consume tokens then don't block the call
%%            'true';
%%        'true' ->
%%            {Name, Cost} = bucket_info(Call, Flow),
%%            case kz_buckets:consume_tokens(?APP_NAME, Name, Cost) of
%%                'true' -> 'true';
%%                'false' ->
%%                    lager:warning("bucket ~s doesn't have enough tokens(~b needed) for this call", [Name, Cost]),
%%                    'false'
%%            end
%%    end.
%%
%%-spec bucket_info(kapps_call:call(), kz_json:object()) ->
%%                         {kz_term:ne_binary(), pos_integer()}.
%%bucket_info(Call, Flow) ->
%%    case kz_json:get_value(<<"pvt_bucket_name">>, Flow) of
%%        'undefined' -> {bucket_name_from_call(Call, Flow), bucket_cost(Flow)};
%%        Name -> {Name, bucket_cost(Flow)}
%%    end.
%%
%%-spec bucket_name_from_call(kapps_call:call(), kz_json:object()) -> kz_term:ne_binary().
%%bucket_name_from_call(Call, Flow) ->
%%    <<(kapps_call:account_id(Call))/binary, ":", (kz_doc:id(Flow))/binary>>.
%%
%%-spec bucket_cost(kz_json:object()) -> pos_integer().
%%bucket_cost(Flow) ->
%%    Min = kapps_config:get_integer(?CF_CONFIG_CAT, <<"min_bucket_cost">>, 5),
%%    case kz_json:get_integer_value(<<"pvt_bucket_cost">>, Flow) of
%%        'undefined' -> Min;
%%        N when N < Min -> Min;
%%        N -> N
%%    end.

%%%%------------------------------------------------------------------------------
%%%% @doc Should this call be able to use outbound resources, the exact opposite
%%%% exists in the handoff module.  When updating this one make sure to sync
%%%% the change with that module.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec allow_no_match(kapps_call:call()) -> boolean().
%%allow_no_match(Call) ->
%%    IsValid = is_valid_endpoint(kapps_call:custom_channel_var(<<"Referred-By">>, Call), Call)
%%        orelse is_valid_endpoint(kapps_call:custom_channel_var(<<"Redirected-By">>, Call), Call)
%%        orelse allow_no_match_type(Call),
%%    io:format("\n~p.allow_no_match/1:  IsValid=~p\n",[?MODULE, IsValid]),
%%    IsValid.
%%
%%-spec allow_no_match_type(kapps_call:call()) -> boolean().
%%allow_no_match_type(Call) ->
%%    io:format("\n~p.allow_no_match_type/1:  authorizing_type=~p\n",[?MODULE, kapps_call:authorizing_type(Call)]),
%%    case kapps_call:authorizing_type(Call) of
%%        'undefined' -> 'false';
%%        <<"resource">> -> 'false';
%%        <<"sys_info">> -> 'false';
%%        _ -> 'true'
%%    end.

%%%%------------------------------------------------------------------------------
%%%% @doc Determine if centrex callflow should respond to a route request.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec callflow_should_respond(kapps_call:call()) -> boolean().
%%callflow_should_respond(Call) ->
%%    case kapps_call:authorizing_type(Call) of
%%        <<"account">> -> 'true';
%%        <<"user">> -> 'true';
%%        <<"device">> -> 'true';
%%        <<"mobile">> -> 'true';
%%        <<"callforward">> -> 'true';
%%        <<"clicktocall">> -> 'true';
%%        <<"click2call">> -> 'true';
%%        <<"conference">> -> 'true';
%%        <<"resource">> -> 'true';
%%        <<"sys_info">> ->
%%            timer:sleep(500),
%%            Number = kapps_call:request_user(Call),
%%            (not knm_converters:is_reconcilable(Number));
%%        'undefined' -> 'true';
%%        _Else ->
%%            lager:debug("not responding to calls from auth-type ~s", [_Else]),
%%            'false'
%%    end.

%%------------------------------------------------------------------------------
%% @doc Send a route response for a route request that can be fulfilled by this
%% process.
%% @end
%%------------------------------------------------------------------------------
-spec send_route_response(kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Call) ->
    io:format("\n~p.send_route_response/3: callflows knows how to route the call! sending park response\n",[?MODULE]),
    lager:info("callflows knows how to route the call! sending park response"),
    AccountId = kapps_call:account_id(Call),
    Resp = props:filter_undefined(
             [{?KEY_MSG_ID, kz_api:msg_id(JObj)}
             ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
             ,{<<"Routes">>, []}
             ,{<<"Method">>, <<"park">>}
             ,{<<"Transfer-Media">>, get_transfer_media(Flow, JObj)}
             ,{<<"Ringback-Media">>, get_ringback_media(Flow, JObj)}
%%             ,{<<"Pre-Park">>, pre_park_action(Call)}
             ,{<<"From-Realm">>, kzd_accounts:fetch_realm(AccountId)}
             ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
             ,{<<"Custom-Application-Vars">>, kapps_call:custom_application_vars(Call)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    io:format("AccountId=~p\nResp=~p\nServerId=~p\nPublisher=~p\n",[AccountId,Resp,ServerId,Publisher]),
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            io:format("callflow has received a route win, taking control of the call\nRouteWin=~p\n",[RouteWin]),
            lager:info("callflow has received a route win, taking control of the call"),
            cx_route_win:execute_callflow(RouteWin, kapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec get_transfer_media(kz_json:object(), kz_json:object()) -> kz_term:api_binary().
get_transfer_media(Flow, JObj) ->
    case kz_json:get_value([<<"ringback">>, <<"transfer">>], Flow) of
        'undefined' ->
            kz_json:get_value(<<"Transfer-Media">>, JObj);
        MediaId -> MediaId
    end.

-spec get_ringback_media(kz_json:object(), kz_json:object()) -> kz_term:api_binary().
get_ringback_media(Flow, JObj) ->
    case kz_json:get_value([<<"ringback">>, <<"early">>], Flow) of
        'undefined' ->
            kz_json:get_value(<<"Ringback-Media">>, JObj);
        MediaId -> MediaId
    end.

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec pre_park_action(kapps_call:call()) -> kz_term:ne_binary().
%%pre_park_action(Call) ->
%%    case kapps_config:get_is_true(?CX_CONFIG_CAT, <<"ring_ready_offnet">>, 'true')
%%        andalso kapps_call:inception(Call) =/= 'undefined'
%%        andalso kapps_call:authorizing_type(Call) =:= 'undefined'
%%    of
%%        'false' -> <<"none">>;
%%        'true' -> <<"ring_ready">>
%%    end.


%%------------------------------------------------------------------------------
%% @doc process
%% @end
%%------------------------------------------------------------------------------
-spec update_call(kz_json:object(), boolean(), kz_term:ne_binary(), kapps_call:call()) ->
                         kapps_call:call().
update_call(Flow, NoMatch, ControllerQ, Call) ->
    Props = [{'cf_flow_id', kz_doc:id(Flow)}
            ,{'cf_flow_name', kz_json:get_ne_binary_value(<<"name">>, Flow, kapps_call:request_user(Call))}
            ,{'cf_flow', kz_json:get_value(<<"flow">>, Flow)}
            ,{'cf_capture_group', kz_json:get_ne_value(<<"capture_group">>, Flow)}
            ,{'cf_capture_groups', kz_json:get_value(<<"capture_groups">>, Flow, kz_json:new())}
            ,{'cf_no_match', NoMatch}
%%            ,{'cf_metaflow', kz_json:get_value(<<"metaflows">>, Flow, ?DEFAULT_METAFLOWS(kapps_call:account_id(Call)))}
            ],

    Updaters = [{fun kapps_call:kvs_store_proplist/2, Props}
               ,{fun kapps_call:set_controller_queue/2, ControllerQ}
               ,{fun kapps_call:set_application_name/2, ?APP_NAME}
               ,{fun kapps_call:set_application_version/2, ?APP_VERSION}
               ,{fun kapps_call:insert_custom_channel_var/3, <<"CallFlow-ID">>, kz_doc:id(Flow)}
               ],
    kapps_call:exec(Updaters, Call).

%%%%------------------------------------------------------------------------------
%%%% @doc process
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec maybe_referred_call(kapps_call:call()) -> kapps_call:call().
%%maybe_referred_call(Call) ->
%%    maybe_fix_restrictions(get_referred_by(Call), Call).
%%
%%-spec maybe_device_redirected(kapps_call:call()) -> kapps_call:call().
%%maybe_device_redirected(Call) ->
%%    maybe_fix_restrictions(get_redirected_by(Call), Call).
%%
%%-spec maybe_fix_restrictions(kz_term:api_binary(), kapps_call:call()) -> kapps_call:call().
%%maybe_fix_restrictions('undefined', Call) -> Call;
%%maybe_fix_restrictions(Device, Call) ->
%%    io:format("\n~p.maybe_fix_restrictions/2: \nDevice=~p\nCall_id=~p\n",[?MODULE,Device,kapps_call:call_id(Call)]),
%%    case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Device) of
%%        {'ok', EndpointId} ->
%%            io:format("EndpointId=~p\n",[EndpointId]),
%%            kapps_call:kvs_store(?RESTRICTED_ENDPOINT_KEY, EndpointId, Call);
%%        {'error', 'not_found'} ->
%%            Keys = [<<"Authorizing-ID">>],
%%            io:format("Remove keys from call = ~p\n",[Keys]),
%%            kapps_call:remove_custom_channel_vars(Keys, Call)
%%    end.

%%-spec get_referred_by(kapps_call:call()) -> kz_term:api_binary().
%%get_referred_by(Call) ->
%%    ReferredBy = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
%%    SipUserName = extract_sip_username(ReferredBy),
%%    io:format("\n~p.get_referred_by/1: \nReferredBy=~p\nSipUserName=~p\n",[?MODULE,ReferredBy,SipUserName]),
%%    SipUserName.
%%
%%-spec get_redirected_by(kapps_call:call()) -> kz_term:api_binary().
%%get_redirected_by(Call) ->
%%    RedirectedBy = kapps_call:custom_channel_var(<<"Redirected-By">>, Call),
%%    SipUserName = extract_sip_username(RedirectedBy),
%%    io:format("\n~p.get_redirected_by/1: \nRedirectedBy=~p\nSipUserName=~p\n",[?MODULE,RedirectedBy,SipUserName]),
%%    SipUserName.
%%
%%-spec is_valid_endpoint(kz_term:api_binary(), kapps_call:call()) -> boolean().
%%is_valid_endpoint('undefined', _) -> 'false';
%%is_valid_endpoint(Contact, Call) ->
%%    ReOptions = [{'capture', [1], 'binary'}],
%%    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
%%        {'match', [Match]} ->
%%            case cf_util:endpoint_id_by_sip_username(kapps_call:account_db(Call), Match) of
%%                {'ok', _EndpointId} -> 'true';
%%                {'error', 'not_found'} -> 'false'
%%            end;
%%        _ -> 'false'
%%    end.

%%-spec extract_sip_username(kz_term:api_binary()) -> kz_term:api_binary().
%%extract_sip_username('undefined') -> 'undefined';
%%extract_sip_username(Contact) ->
%%    ReOptions = [{'capture', [1], 'binary'}],
%%    case catch(re:run(Contact, <<".*sip:(.*)@.*">>, ReOptions)) of
%%        {'match', [Match]} -> Match;
%%        _ -> 'undefined'
%%    end.
