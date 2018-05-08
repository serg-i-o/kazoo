-module(cx_flow).

-export([lookup_centrix_account/1, lookup_centrix_account/2
        , lookup_centrix_callflow/1, lookup_centrix_callflow/2
]).

-include("centrix.hrl").


%%------------------------------------------------------------------------------
%% @doc Cache centrix callflow
%% @end
%%------------------------------------------------------------------------------
-spec cache_callflow(kz_term:ne_binary(), kz_term:ne_binary(), kzd_callflow:doc()) -> cx_callflow_ret().
cache_callflow(Number, AccountId, Flow) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [
        {'origin', [{'db', AccountDb, <<"centrix">>}]}
%%        , {'expires', ?MILLISECONDS_IN_HOUR}
    ],
    io:format("\n~p.cache_callflow/3:\nCacheName=~p\nCacheKey=~p\nFlowId=~p\nCacheOptions=~p\n",
        [?MODULE, ?CACHE_NAME, ?CX_CALLFLOW_CACHE_KEY(Number, AccountId), kz_doc:id(Flow), CacheOptions]),
    kz_cache:store_local(?CACHE_NAME, ?CX_CALLFLOW_CACHE_KEY(Number, AccountId), kz_doc:id(Flow), CacheOptions),
    {'ok', Flow}.


%%------------------------------------------------------------------------------
%% @doc Cache centrix (user) account
%% @end
%%------------------------------------------------------------------------------
-spec cache_cx_account(kz_term:ne_binary(), kz_term:ne_binary(), kzd_centrix_accounts:doc()) -> cx_account_lookup_ret().
cache_cx_account(Number, AccountId, CxAccount) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [
        {'origin', [{'db', AccountDb, <<"centrix_accounts">>}]}
%%        , {'expires', ?MILLISECONDS_IN_HOUR}
    ],
    io:format("\n~p.cache_cx_account/3:\nCacheName=~p\nCacheKey=~p\nCxAccountId=~p\nCacheOptions=~p\n",
        [?MODULE, ?CACHE_NAME, ?CX_ACCOUNT_CACHE_KEY(Number, AccountId), kz_doc:id(CxAccount), CacheOptions]),
    kz_cache:store_local(?CACHE_NAME, ?CX_ACCOUNT_CACHE_KEY(Number, AccountId), kz_doc:id(CxAccount), CacheOptions),
    {'ok', CxAccount}.


%%------------------------------------------------------------------------------
%% @doc Get centrix (user) account
%% @end
%%------------------------------------------------------------------------------
-type cx_account_lookup_ret() :: {'ok', kzd_centrix_accounts:doc()} | {'error', any()}.

-spec lookup_centrix_account(kapps_call:call()) -> cx_account_lookup_ret().
lookup_centrix_account(Call) ->
    lookup_centrix_account(kapps_call:request_user(Call), kapps_call:account_id(Call)).

-spec lookup_centrix_account(kz_term:ne_binary(), kz_term:ne_binary()) -> cx_account_lookup_ret().
lookup_centrix_account(Number, AccountId) when not is_binary(Number) ->
    lookup_centrix_account(kz_term:to_binary(Number), AccountId);
lookup_centrix_account(<<>>, _) ->
    {'error', 'invalid_number'};
lookup_centrix_account(Number, AccountId) ->
    io:format("\n~p.lookup_centrix_account/2:\nNumber=~p\nAccountId=~p\n",[?MODULE,Number,AccountId]),
    case kz_cache:fetch_local(?CACHE_NAME, ?CX_ACCOUNT_CACHE_KEY(Number, AccountId)) of
        {'ok', CxAccountId} ->
            io:format("kz_cache:fetch_local CxAccountId=~p\n",[CxAccountId]),
            return_cx_account_doc(CxAccountId, AccountId);
        {'error', 'not_found'} ->
            io:format("kz_cache:fetch_local CxAccountId=~p\n",['not_found']),
            do_cx_account_lookup(Number, AccountId)
    end.

-spec return_cx_account_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> cx_account_lookup_ret().
return_cx_account_doc(CxAccountId, AccountId) ->
    return_cx_account_doc(CxAccountId, AccountId, []).

-spec return_cx_account_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> cx_account_lookup_ret().
return_cx_account_doc(CxAccountId, AccountId, Props) ->
    io:format("\n~p.return_cx_account_doc/3:\nCxAccountId=~p\nAccountId=~p\n",[?MODULE, CxAccountId, AccountId]),
    Db = kz_util:format_account_db(AccountId),
    case kz_datamgr:open_cache_doc(Db, CxAccountId) of
        {'ok', Doc} ->
            io:format("\nkz_datamgr:open_cache_doc Doc=~p\n",[Doc]),
            {'ok', kz_json:set_values(Props, Doc)};
        Error ->
            io:format("\nkz_datamgr:open_cache_doc Doc=~p\n",[Error]),
            Error
    end.

-spec do_cx_account_lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> cx_account_lookup_ret().
do_cx_account_lookup(Number, AccountId) ->
    Db = kz_util:format_account_db(AccountId),
    io:format("\n~p.do_cx_account_lookup/2:\nsearching for centrix account for number ~s in account db '~s'\n",
        [?MODULE,Number, Db]),
    lager:info("searching for centrix account for number ~s in account db '~s'", [Number, Db]),
    Options = [{'key', Number}, 'include_docs'],
    case kz_datamgr:get_results(Db, ?CENTRIX_ACCOUNTS_BY_NUMBER_VIEW, Options) of
        {'error', _}=E ->
            io:format("kz_datamgr:get_results CxAccount=~p\n",[E]),
            E;
        {'ok', []} ->
            io:format("kz_datamgr:get_results CxAccount=~p\n",['not_found']),
            {'error', 'not_found'};
        {'ok', [JObj]} ->
            CxAccount = kz_json:get_value(<<"doc">>, JObj),
            io:format("kz_datamgr:get_results\nJObj=~p\nCxAccountId=~p\n",[JObj,kz_doc:id(CxAccount)]),
            cache_cx_account(Number, AccountId, CxAccount);
        {'ok', [JObj | _Rest]} ->
            io:format("centrix account lookup for number ~s resulted in more than one result, using the first\n",[Number]),
            lager:warning("centrix account lookup for number ~s resulted in more than one result, using the first",[Number]),
            CxAccount = kz_json:get_value(<<"doc">>, JObj),
            io:format("kz_datamgr:get_results CxAccountId=~p\n",[kz_doc:id(CxAccount)]),
            cache_cx_account(Number, AccountId, CxAccount)
    end.

%%------------------------------------------------------------------------------
%% @doc Get centrix callflow
%% @end
%%------------------------------------------------------------------------------
-type cx_callflow_ret() :: {'ok', kzd_callflow:doc(), boolean()} | {'error', any()}.
-type centrix_ret() :: {'ok', kzd_centrix:doc()} | {'error', any()}.

-spec lookup_centrix_callflow(kapps_call:call()) -> cx_callflow_ret().
lookup_centrix_callflow(Call) ->
    lookup_centrix_callflow(kapps_call:request_user(Call), kapps_call:account_id(Call)).

-spec lookup_centrix_callflow(kz_term:ne_binary(), kz_term:ne_binary()) -> cx_callflow_ret().
lookup_centrix_callflow(Number, AccountId) when not is_binary(Number) ->
    lookup_centrix_callflow(kz_term:to_binary(Number), AccountId);
lookup_centrix_callflow(<<>>, _) ->
    {'error', 'invalid_number'};
lookup_centrix_callflow(Number, AccountId) ->
    io:format("\n~p.lookup_centrix_callflow/2:\nNumber=~p\nAccountId=~p\n",[?MODULE,Number,AccountId]),
    case kz_cache:fetch_local(?CACHE_NAME, ?CX_CALLFLOW_CACHE_KEY(Number, AccountId)) of
        {'ok', FlowId} ->
            io:format("kz_cache:fetch_local FlowId=~p\n",[FlowId]),
            return_callflow_doc(Number, AccountId, FlowId);
        {'error', 'not_found'} ->
            io:format("kz_cache:fetch_local FlowId=~p\n",['not_found']),
            do_lookup_centrix_callflow(Number, AccountId)
    end.


-spec do_lookup_centrix_callflow(kz_term:ne_binary(), kz_term:ne_binary()) -> cx_callflow_ret().
do_lookup_centrix_callflow(Number, AccountId) ->
    case lookup_centrix_account(Number, AccountId) of
        {'error', _}=E -> E;
        {'ok', CentrixAccount} ->
            io:format("\n~p.do_lookup_centrix_callflow/2:\nNumber=~p\nAccountId=~p\nCxAccountId=~p\n",
                [?MODULE,Number, AccountId, kzd_centrix_accounts:centrix_id(CentrixAccount)]),
            do_lookup_centrix_callflow(Number, AccountId, kzd_centrix_accounts:centrix_id(CentrixAccount))
    end.

-spec do_lookup_centrix_callflow(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> cx_callflow_ret().
do_lookup_centrix_callflow(_Number, _AccountId, 'undefined') ->
    io:format("\n~p.do_lookup_centrix_callflow/3:  ~p\n",[?MODULE, 'invalid_centrix_account']),
    {'error', 'invalid_centrix_account'};
do_lookup_centrix_callflow(Number, AccountId, CentrixId) ->
    io:format("\n~p.do_lookup_centrix_callflow/3:\nNumber=~p\nAccountId=~p\nCentrixId=~p\n",
        [?MODULE, Number, AccountId, CentrixId]),
    case return_centrix_doc(CentrixId, AccountId) of
        {'error', _}=E -> E;
        {'ok', Centrix} -> return_callflow_doc(Number, AccountId, kzd_centrix:callflow_id(Centrix))
    end.

-spec return_centrix_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> centrix_ret().
return_centrix_doc(CentrixId, AccountId) ->
    return_centrix_doc(CentrixId, AccountId, []).

-spec return_centrix_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> centrix_ret().
return_centrix_doc(CentrixId, AccountId, Props) ->
    Db = kz_util:format_account_db(AccountId),
    io:format("\n~p.return_centrix_doc/3:\nCentrixId=~p\nAccountId=~p\nProps=~p\n",
        [?MODULE, CentrixId, AccountId, Props]),
    case kz_datamgr:open_cache_doc(Db, CentrixId) of
        {'ok', Doc} ->
            io:format("Doc=~p\n",[Doc]),
            {'ok', kz_json:set_values(Props, Doc)};
        Error ->
            io:format("Error=~p\n",[Error]),
            Error
    end.

-spec return_callflow_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> cx_callflow_ret().
return_callflow_doc(_Number, _AccountId, 'undefined') ->
    {'error', 'invalid_centrix_service'};
return_callflow_doc(Number, AccountId, FlowId) ->
    Db = kz_util:format_account_db(AccountId),
    io:format("\n~p.return_callflow_doc/3:\nNumber=~p\nAccountId=~p\nFlowId=~p\n",[?MODULE,Number, AccountId, FlowId]),
    case kz_datamgr:open_cache_doc(Db, FlowId) of
        {'ok', Flow} ->
            cache_callflow(Number, AccountId, Flow),
            refit_callflow(Number, Flow);
        Error -> Error
    end.

-spec refit_callflow(kz_term:ne_binary(), kzd_callflow:doc()) -> cx_callflow_ret().
refit_callflow(_Number, Flow) ->
    io:format("'\n~p.refit_callflow/2: Try refit callflow\n",[?MODULE]),
    {'ok', Flow, contains_no_match(Flow)}.

-spec contains_no_match(kzd_callflow:doc()) -> boolean().
contains_no_match(Doc) ->
    lists:any(fun(Number) when Number =:= ?NO_MATCH ->
        'true';
        (_) ->
            'false'
              end, kzd_callflow:numbers(Doc)).


%%%%------------------------------------------------------------------------------
%%%% @doc Lookup the callflow based on the requested number in the account.
%%%% @end
%%%%------------------------------------------------------------------------------
%%
%%%%-type lookup_ret() :: {'ok', kzd_callflow:doc(), boolean()} | {'error', any()}.
%%
%%-spec lookup(kapps_call:call()) -> lookup_ret().
%%lookup(Call) ->
%%    lookup(kapps_call:request_user(Call), kapps_call:account_id(Call)).
%%
%%-spec lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%%lookup(Number, AccountId) when not is_binary(Number) ->
%%    lookup(kz_term:to_binary(Number), AccountId);
%%lookup(<<>>, _) ->
%%    {'error', 'invalid_number'};
%%lookup(Number, AccountId) ->
%%    case kz_cache:fetch_local(?CACHE_NAME, ?CF_FLOW_CACHE_KEY(Number, AccountId)) of
%%        {'ok', FlowId} -> return_callflow_doc(FlowId, AccountId);
%%        {'error', 'not_found'} -> do_lookup(Number, AccountId)
%%    end.
%%
%%-spec return_callflow_doc(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%%return_callflow_doc(FlowId, AccountId) ->
%%    return_callflow_doc(FlowId, AccountId, []).
%%
%%-spec return_callflow_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> lookup_ret().
%%return_callflow_doc(FlowId, AccountId, Props) ->
%%    Db = kz_util:format_account_db(AccountId),
%%    case kz_datamgr:open_cache_doc(Db, FlowId) of
%%        {'ok', Doc} ->
%%            {'ok', kz_json:set_values(Props, Doc), contains_no_match(Doc)};
%%        Error -> Error
%%    end.
%%
%%
%%
%%-spec do_lookup(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%%do_lookup(Number, AccountId) ->
%%    Db = kz_util:format_account_db(AccountId),
%%    lager:info("searching for callflow in ~s to satisfy '~s'", [Db, Number]),
%%    Options = [{'key', Number}, 'include_docs'],
%%    case kz_datamgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
%%        {'error', _}=E -> E;
%%        {'ok', []} when Number =/= ?NO_MATCH_CF ->
%%            lookup_patterns(Number, AccountId);
%%        {'ok', []} -> {'error', 'not_found'};
%%        {'ok', [JObj]} ->
%%            Flow = kz_json:get_value(<<"doc">>, JObj),
%%            cache_callflow_number(Number, AccountId, Flow);
%%        {'ok', [JObj | _Rest]} ->
%%            lager:info("lookup resulted in more than one result, using the first"),
%%            Flow = kz_json:get_value(<<"doc">>, JObj),
%%            cache_callflow_number(Number, AccountId, Flow)
%%    end.
%%
%%-spec cache_callflow_number(kz_term:ne_binary(), kz_term:ne_binary(), kzd_callflow:doc()) -> lookup_ret().
%%cache_callflow_number(Number, AccountId, Flow) ->
%%    AccountDb = kz_util:format_account_db(AccountId),
%%    CacheOptions = [{'origin', [{'db', AccountDb, <<"callflow">>}]}
%%                   ,{'expires', ?MILLISECONDS_IN_HOUR}
%%                   ],
%%    kz_cache:store_local(?CACHE_NAME, ?CF_FLOW_CACHE_KEY(Number, AccountId), kz_doc:id(Flow), CacheOptions),
%%    {'ok', Flow, contains_no_match(Flow)}.

%%-spec maybe_use_nomatch(kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%%%% only route to nomatch when Number is all digits and/or +
%%maybe_use_nomatch(<<"+", Number/binary>>, AccountId) ->
%%    maybe_use_nomatch(Number, AccountId);
%%maybe_use_nomatch(Number, AccountId) ->
%%    case lists:all(fun is_digit/1, kz_term:to_list(Number)) of
%%        'true' -> lookup(?NO_MATCH_CF, AccountId);
%%        'false' ->
%%            lager:info("can't use no_match: number not all digits: ~s", [Number]),
%%            {'error', 'not_found'}
%%    end.
%%
%%-spec is_digit(char()) -> boolean().
%%is_digit(X) when X >= $0, X =< $9 -> 'true';
%%is_digit(_) -> 'false'.
%%
%%-spec fetch_patterns(kz_term:ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
%%fetch_patterns(AccountId)->
%%    case kz_cache:fetch_local(?CACHE_NAME, ?CF_PATTERN_CACHE_KEY(AccountId)) of
%%        {'ok', _Patterns}= OK -> OK;
%%        {'error', 'not_found'} -> load_patterns(AccountId)
%%    end.
%%
%%-spec load_patterns(kz_term:ne_binary()) -> {'ok', patterns()} | {'error', 'not_found'}.
%%load_patterns(AccountId) ->
%%    Db = kz_util:format_account_db(AccountId),
%%    case kz_datamgr:get_results(Db, ?LIST_BY_PATTERN, ['include_docs']) of
%%        {'ok', []} -> {'error', 'not_found'};
%%        {'ok', JObjs} -> compile_patterns(AccountId, JObjs);
%%        {'error', _}=_E ->
%%            lager:error("error getting callflow patterns for account ~s : ~p", [AccountId, _E]),
%%            {'error', 'not_found'}
%%    end.
%%
%%compile_patterns(AccountId, JObjs) ->
%%    compile_patterns(AccountId, JObjs, []).
%%
%%compile_patterns(AccountId, [], Acc) ->
%%    cache_patterns(AccountId, Acc);
%%compile_patterns(AccountId, [JObj | JObjs], Acc) ->
%%    Regex = kz_json:get_value(<<"key">>, JObj),
%%    FlowId = kz_doc:id(JObj),
%%    case re:compile(Regex) of
%%        {'ok', {'re_pattern', Groups, _, _, _} = MP}
%%          when Groups =:= 0 ->
%%            Pat = #pattern{flow_id=FlowId, regex=MP, has_groups='false'},
%%            compile_patterns(AccountId, JObjs, [Pat | Acc]);
%%        {'ok', MP} ->
%%            {'namelist', Names} = re:inspect(MP, 'namelist'),
%%            Pat = #pattern{flow_id=FlowId, regex=MP, names=Names, has_groups='true'},
%%            compile_patterns(AccountId, JObjs, [Pat | Acc]);
%%        _Err ->
%%            lager:debug("unexpected result compiling regular expression : ~p", [_Err]),
%%            compile_patterns(AccountId, JObjs, Acc)
%%    end.
%%
%%-spec cache_patterns(kz_term:ne_binary(), patterns()) -> {'ok', patterns()}.
%%cache_patterns(AccountId, Patterns) ->
%%    AccountDb = kz_util:format_account_db(AccountId),
%%    CacheOptions = [{'origin', [{'db', AccountDb, <<"callflow">>}]}],
%%    kz_cache:store_local(?CACHE_NAME, ?CF_PATTERN_CACHE_KEY(AccountId), Patterns, CacheOptions),
%%    {'ok', Patterns}.
%%
%%-spec lookup_patterns(kz_term:ne_binary(), kz_term:ne_binary()) ->
%%                             {'ok', {kz_json:object(), kz_term:api_binary()}} |
%%                             {'error', any()}.
%%lookup_patterns(Number, AccountId) ->
%%    case fetch_patterns(AccountId) of
%%        {'ok', Patterns} -> lookup_callflow_patterns(Patterns, Number, AccountId);
%%        _Error -> maybe_use_nomatch(Number, AccountId)
%%    end.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec lookup_callflow_patterns(patterns(), kz_term:ne_binary(), kz_term:ne_binary()) -> lookup_ret().
%%lookup_callflow_patterns(Patterns, Number, AccountId) ->
%%    case test_callflow_patterns(Patterns, Number) of
%%        'no_match' -> maybe_use_nomatch(Number, AccountId);
%%        {Match, #pattern{flow_id=FlowId}=Pattern} ->
%%            NameMap = get_captured_names(Number, Pattern),
%%            Props = [{<<"capture_group">>, Match}
%%                    ,{<<"capture_groups">>, kz_json:from_list(NameMap)}
%%                    ],
%%            return_callflow_doc(FlowId, AccountId, props:filter_empty(Props))
%%    end.
%%
%%-spec get_captured_names(kz_term:ne_binary(), pattern()) -> kz_term:proplist().
%%get_captured_names(_Number, #pattern{names=[]}) -> [];
%%get_captured_names(Number, #pattern{regex=Regex, names=Names}) ->
%%    case re:run(Number, Regex, [{'capture', 'all_names', 'binary'}]) of
%%        {'match', L} -> props:filter_empty(lists:zip(Names,L));
%%        _ -> []
%%    end.
%%
%%-type test_pattern_acc() ::  {binary(), pattern() | 'undefined'}.
%%
%%-spec test_callflow_patterns(patterns(), kz_term:ne_binary()) -> 'no_match' | test_pattern_acc().
%%test_callflow_patterns(Patterns, Number) ->
%%    test_callflow_patterns(Patterns, Number, {<<>>, 'undefined'}).
%%
%%-spec test_callflow_patterns(patterns(), kz_term:ne_binary(), test_pattern_acc()) ->
%%                                    'no_match' | test_pattern_acc().
%%test_callflow_patterns([], _, {_, 'undefined'}) -> 'no_match';
%%test_callflow_patterns([], _, Result) -> Result;
%%test_callflow_patterns([#pattern{regex=Regex}=Pattern |T], Number, {Matched, P}=Result) ->
%%    case re:run(Number, Regex, match_options(Pattern)) of
%%        {'match', Groups} ->
%%            case hd(lists:sort(fun(A, B) -> byte_size(A) >= byte_size(B) end, Groups)) of
%%                Match when P =:= 'undefined'
%%                           orelse byte_size(Match) > byte_size(Matched) ->
%%                    test_callflow_patterns(T, Number, {Match, Pattern});
%%                _ -> test_callflow_patterns(T, Number, Result)
%%            end;
%%        _ ->
%%            test_callflow_patterns(T, Number, Result)
%%    end.
%%
%%-spec match_options(pattern()) -> list().
%%match_options(#pattern{has_groups='true'}) ->
%%    [{'capture', 'all_but_first', 'binary'}];
%%match_options(_) ->
%%    [{'capture', 'all', 'binary'}].
