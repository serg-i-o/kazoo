%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% Domains document for white-label
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_domains).

-export([new/0]).
-export([a/1, a/2, set_a/2]).
-export([cname/1, cname/2, set_cname/2]).
-export([mx/1, mx/2, set_mx/2]).
-export([naptr/1, naptr/2, set_naptr/2]).
-export([srv/1, srv/2, set_srv/2]).
-export([txt/1, txt/2, set_txt/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec a(doc()) -> api_object().
-spec a(doc(), Default) -> kz_json:object() | Default.
a(Doc) ->
    a(Doc, 'undefined').
a(Doc, Default) ->
    kz_json:get_json_value([<<"A">>], Doc, Default).

-spec set_a(doc(), kz_json:object()) -> doc().
set_a(Doc, A) ->
    kz_json:set_value([<<"A">>], A, Doc).

-spec cname(doc()) -> kz_term:api_object().
-spec cname(doc(), Default) -> kz_json:object() | Default.
cname(Doc) ->
    cname(Doc, 'undefined').
cname(Doc, Default) ->
    kz_json:get_json_value([<<"CNAME">>], Doc, Default).

-spec set_cname(doc(), kz_json:object()) -> doc().
set_cname(Doc, Cname) ->
    kz_json:set_value([<<"CNAME">>], Cname, Doc).

-spec mx(doc()) -> api_object().
-spec mx(doc(), Default) -> kz_json:object() | Default.
mx(Doc) ->
    mx(Doc, 'undefined').
mx(Doc, Default) ->
    kz_json:get_json_value([<<"MX">>], Doc, Default).

-spec set_mx(doc(), kz_json:object()) -> doc().
set_mx(Doc, Mx) ->
    kz_json:set_value([<<"MX">>], Mx, Doc).

-spec set_a_record(doc(), kz_json:object()) -> doc().
set_a_record(Domains, A_RECORD) ->
    kz_json:set_value(?KEY_A_RECORD, A_RECORD, Domains).

-spec add_a_record_host(doc(), kz_term:ne_binary(), kz_json:object()) -> doc().
add_a_record_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_A_RECORD, Host], Settings, Domains).

-spec naptr(doc()) -> kz_term:api_object().
-spec naptr(doc(), Default) -> kz_json:object() | Default.
naptr(Doc) ->
    naptr(Doc, 'undefined').
naptr(Doc, Default) ->
    kz_json:get_json_value([<<"NAPTR">>], Doc, Default).

-spec set_naptr(doc(), kz_json:object()) -> doc().
set_naptr(Doc, Naptr) ->
    kz_json:set_value([<<"NAPTR">>], Naptr, Doc).

-spec add_naptr_host(doc(), kz_term:ne_binary(), kz_json:object()) -> doc().
add_naptr_host(Domains, Host, Settings) ->
    kz_json:set_value([?KEY_NAPTR, Host], Settings, Domains).

-spec srv(doc()) -> kz_term:api_object().
-spec srv(doc(), Default) -> kz_json:object() | Default.
srv(Doc) ->
    srv(Doc, 'undefined').
srv(Doc, Default) ->
    kz_json:get_json_value([<<"SRV">>], Doc, Default).

-spec set_srv(doc(), kz_json:object()) -> doc().
set_srv(Doc, Srv) ->
    kz_json:set_value([<<"SRV">>], Srv, Doc).

-spec txt(doc()) -> api_object().
-spec txt(doc(), Default) -> kz_json:object() | Default.
txt(Doc) ->
    txt(Doc, 'undefined').
txt(Doc, Default) ->
    kz_json:get_json_value([<<"TXT">>], Doc, Default).

-spec set_txt(doc(), kz_json:object()) -> doc().
set_txt(Doc, Txt) ->
    kz_json:set_value([<<"TXT">>], Txt, Doc).
