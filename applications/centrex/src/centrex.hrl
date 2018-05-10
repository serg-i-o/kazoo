-ifndef(TS_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(APP_NAME, <<"centrex">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(NO_MATCH, <<"no_match">>).
-define(CENTREX_VIEW, <<"centrex/crossbar_listing">>).
-define(CENTREX_ACCOUNTS_VIEW,              <<"centrex/cx_accounts_by_centrex_id">>).
-define(CENTREX_ACCOUNTS_BY_NUMBER_VIEW,    <<"centrex/cx_accounts_by_number_offnet">>).

-define(CACHE_NAME, 'centrex_cache').
-define(CX_CALLFLOW_CACHE_KEY(Number, AccountId), {'cx_callflow', Number, AccountId}).
-define(CX_ACCOUNT_CACHE_KEY(Number, AccountId), {'cx_account', Number, AccountId}).
-define(IS_CX_ACCOUNT_CACHE_KEY(AccountId), {'is_cx_account', AccountId}).

%% may be not used
-define(RESTRICTED_ENDPOINT_KEY, <<"Restricted-Endpoint-ID">>).


-define(TS_HRL, 'true').
-endif.
