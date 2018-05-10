%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_centrex_accounts).

-export([new/0]).
-export([centrex_id/1, centrex_id/2, set_centrex_id/2]).
-export([user_id/1, user_id/2, set_user_id/2]).
-export([number_offnet/1, number_offnet/2, set_number_offnet/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"centrex_accounts">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec centrex_id(doc()) -> kz_term:api_ne_binary().
centrex_id(Doc) ->
    centrex_id(Doc, 'undefined').

-spec centrex_id(doc(), Default) -> kz_term:ne_binary() | Default.
centrex_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"centrex_id">>], Doc, Default).

-spec set_centrex_id(doc(), kz_term:ne_binary()) -> doc().
set_centrex_id(Doc, CentrexId) ->
    kz_json:set_value([<<"centrex_id">>], CentrexId, Doc).


-spec user_id(doc()) -> kz_term:api_ne_binary().
user_id(Doc) ->
    user_id(Doc, 'undefined').

-spec user_id(doc(), Default) -> kz_term:ne_binary() | Default.
user_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"user_id">>], Doc, Default).

-spec set_user_id(doc(), kz_term:ne_binary()) -> doc().
set_user_id(Doc, UserId) ->
    kz_json:set_value([<<"user_id">>], UserId, Doc).


-spec number_offnet(doc()) -> kz_term:api_ne_binary().
number_offnet(Doc) ->
    number_offnet(Doc, 'undefined').

-spec number_offnet(doc(), Default) -> kz_term:ne_binary() | Default.
number_offnet(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"number_offnet">>], Doc, Default).

-spec set_number_offnet(doc(), kz_term:ne_binary()) -> doc().
set_number_offnet(Doc, NumberOffnet) ->
    kz_json:set_value([<<"number_offnet">>], NumberOffnet, Doc).