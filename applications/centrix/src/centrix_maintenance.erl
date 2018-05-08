-module(centrix_maintenance).

-export([flush/0
        ,cache_is_centrix/2
]).
-export([is_centrix_account/1]).

-include("centrix.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).

%%------------------------------------------------------------------------------
%% @doc Cache binary flag that account has centrix docs
%% @end
%%------------------------------------------------------------------------------
-spec cache_is_centrix(kz_term:ne_binary(), boolean()) -> boolean().
cache_is_centrix(AccountId, IsCentrix) ->
    AccountDb = kz_util:format_account_db(AccountId),
    CacheOptions = [{'origin', [{'db', AccountDb, <<"centrix">>}]}, {'expires', ?MILLISECONDS_IN_HOUR}],
    io:format("\n~p.cache_is_centrix/2:\nCacheName=~p\nCacheKey=~p\nIsCentrix=~p\nCacheOptions=~p\n",
        [?MODULE, ?CACHE_NAME, ?IS_CX_ACCOUNT_CACHE_KEY(AccountId), IsCentrix, CacheOptions]),
    kz_cache:store_local(?CACHE_NAME, ?IS_CX_ACCOUNT_CACHE_KEY(AccountId), IsCentrix, CacheOptions),
    IsCentrix.

%%------------------------------------------------------------------------------
%% @doc Check for account is centrix
%% @end
%%------------------------------------------------------------------------------
-spec is_centrix_account(kapps_call:call()) -> boolean().
is_centrix_account(Call) ->
    AccountId = kapps_call:account_id(Call),
    io:format("\n~p.is_centrix_account/1:  AccountId=~p\n",[?MODULE,AccountId]),
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
    lager:info("searching for centrix docs in account ~s", [Db]),
    case kz_datamgr:get_results(Db, ?CENTRIX_VIEW) of
        {'error', ErrorType} -> lager:debug("error ~s on find centrix docs in account ~s",[ErrorType, AccountId]), 'false';
        {'ok', []} -> lager:debug("can not find any centrix docs in account ~s", [AccountId]), 'false';
        {'ok', [_JObj]} ->
            cache_is_centrix(AccountId, 'true')
    end.






