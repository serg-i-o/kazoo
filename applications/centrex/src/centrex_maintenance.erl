-module(centrex_maintenance).

-export([flush/0]).
-export([is_centrex_account/1]).

-include("centrex.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).

%%------------------------------------------------------------------------------
%% @doc Cache binary flag that account has centrex docs
%% @end
%%------------------------------------------------------------------------------
-spec cache_is_centrex(kz_term:ne_binary(), boolean()) -> boolean().
cache_is_centrex(AccountId, IsCentrex) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"centrex">>}]}, {'expires', ?MILLISECONDS_IN_HOUR}],
    io:format("\n~p.cache_is_centrex/2:\nCacheName=~p\nCacheKey=~p\nIsCentrex=~p\nCacheOptions=~p\n",
        [?MODULE, ?CACHE_NAME, ?IS_CX_ACCOUNT_CACHE_KEY(AccountId), IsCentrex, CacheOptions]),
    kz_cache:store_local(?CACHE_NAME, ?IS_CX_ACCOUNT_CACHE_KEY(AccountId), IsCentrex, CacheOptions),
    IsCentrex.

%%------------------------------------------------------------------------------
%% @doc Check for account is centrex
%% @end
%%------------------------------------------------------------------------------
-spec is_centrex_account(kapps_call:call()) -> boolean().
is_centrex_account(Call) ->
    AccountId = kapps_call:account_id(Call),
    io:format("\n~p.is_centrex_account/1:  AccountId=~p\n",[?MODULE,AccountId]),
    case is_binary(AccountId) andalso account_has_centix(AccountId) of
        'true' -> 'true';
        _ -> 'false'
    end.

-spec account_has_centix(kz_term:ne_binary()) -> boolean().
account_has_centix(AccountId) ->
    io:format("\n~p.account_has_centix/1:  AccountId=~p\n",[?MODULE,AccountId]),
    case kz_cache:fetch_local(?CACHE_NAME, ?IS_CX_ACCOUNT_CACHE_KEY(AccountId)) of
        {'ok', IsCxAccount} -> IsCxAccount;
        {'error', 'not_found'} -> do_lookup(AccountId)
    end.

-spec do_lookup(kz_term:ne_binary()) -> boolean().
do_lookup(AccountId) ->
    Db = kz_util:format_account_db(AccountId),
    lager:info("searching for centrex docs in account ~s", [Db]),
    case kz_datamgr:get_results(Db, ?CENTREX_VIEW) of
        {'error', ErrorType} -> lager:debug("error ~s on find centrex docs in account ~s",[ErrorType, AccountId]), 'false';
        {'ok', []} -> lager:debug("can not find any centrex docs in account ~s", [AccountId]), 'false';
        {'ok', [_JObj]} ->
            cache_is_centrex(AccountId, 'true')
    end.






