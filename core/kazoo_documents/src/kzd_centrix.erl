%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_centrix).

-export([new/0]).
-export([callflow_id/1, callflow_id/2, set_callflow_id/2]).
-export([description/1, description/2, set_description/2]).
-export([name/1, name/2, set_name/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"centrix">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec callflow_id(doc()) -> kz_term:api_ne_binary().
callflow_id(Doc) ->
    callflow_id(Doc, 'undefined').

-spec callflow_id(doc(), Default) -> kz_term:ne_binary() | Default.
callflow_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"callflow_id">>], Doc, Default).

-spec set_callflow_id(doc(), kz_term:ne_binary()) -> doc().
set_callflow_id(Doc, CallflowId) ->
    kz_json:set_value([<<"callflow_id">>], CallflowId, Doc).


-spec description(doc()) -> kz_term:api_ne_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> kz_term:ne_binary() | Default.
description(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), kz_term:ne_binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).


-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).
