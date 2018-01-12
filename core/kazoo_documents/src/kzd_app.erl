%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kzd_app).

-export([fetch/2]).

-export([new/0]).
-export([account_id/1]).
-export([allowed_users/1, allowed_users/2, set_allowed_users/2]).
-export([api_url/1, api_url/2, set_api_url/2]).
-export([author/1, author/2, set_author/2]).
-export([i18n/1, i18n/2, set_i18n/2]).
-export([icon/1, icon/2, set_icon/2]).
-export([license/1, license/2, set_license/2]).
-export([masqueradable/1, masqueradable/2, set_masqueradable/2]).
-export([name/1, name/2, set_name/2]).
-export([phase/1, phase/2, set_phase/2]).
-export([price/1, price/2, set_price/2]).
-export([is_published/1, is_published/2, set_is_published/2
        ,publish/1, unpublish/1
        ]).
-export([screenshots/1, screenshots/2, set_screenshots/2]).
-export([source_url/1, source_url/2, set_source_url/2]).
-export([tags/1, tags/2, set_tags/2]).
-export([urls/1, urls/2, set_urls/2]).
-export([users/1, users/2, set_users/2]).
-export([version/1, version/2, set_version/2]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec account_id(doc()) -> kz_term:api_ne_binary().
account_id(Doc) ->
    kz_doc:account_id(Doc).

-spec allowed_users(doc()) -> kz_term:api_binary().
-spec allowed_users(doc(), Default) -> binary() | Default.
allowed_users(Doc) ->
    allowed_users(Doc, 'undefined').
allowed_users(Doc, Default) ->
    kz_json:get_binary_value([<<"allowed_users">>], Doc, Default).

-spec set_allowed_users(doc(), binary()) -> doc().
set_allowed_users(Doc, AllowedUsers) ->
    kz_json:set_value([<<"allowed_users">>], AllowedUsers, Doc).

-spec api_url(doc()) -> kz_term:api_binary().
-spec api_url(doc(), Default) -> binary() | Default.
api_url(Doc) ->
    api_url(Doc, 'undefined').
api_url(Doc, Default) ->
    kz_json:get_binary_value([<<"api_url">>], Doc, Default).

-spec set_api_url(doc(), binary()) -> doc().
set_api_url(Doc, ApiUrl) ->
    kz_json:set_value([<<"api_url">>], ApiUrl, Doc).

-spec author(doc()) -> kz_term:api_ne_binary().
-spec author(doc(), Default) -> kz_term:ne_binary() | Default.
author(Doc) ->
    author(Doc, 'undefined').
author(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"author">>], Doc, Default).

-spec set_author(doc(), kz_term:ne_binary()) -> doc().
set_author(Doc, Author) ->
    kz_json:set_value([<<"author">>], Author, Doc).

-spec i18n(doc()) -> kz_term:api_object().
-spec i18n(doc(), Default) -> kz_json:object() | Default.
i18n(Doc) ->
    i18n(Doc, 'undefined').
i18n(Doc, Default) ->
    kz_json:get_json_value([<<"i18n">>], Doc, Default).

-spec set_i18n(doc(), kz_json:object()) -> doc().
set_i18n(Doc, I18n) ->
    kz_json:set_value([<<"i18n">>], I18n, Doc).

-spec icon(doc()) -> kz_term:api_binary().
-spec icon(doc(), Default) -> binary() | Default.
icon(Doc) ->
    icon(Doc, 'undefined').
icon(Doc, Default) ->
    kz_json:get_binary_value([<<"icon">>], Doc, Default).

-spec set_icon(doc(), binary()) -> doc().
set_icon(Doc, Icon) ->
    kz_json:set_value([<<"icon">>], Icon, Doc).

-spec license(doc()) -> kz_term:api_binary().
-spec license(doc(), Default) -> binary() | Default.
license(Doc) ->
    license(Doc, 'undefined').
license(Doc, Default) ->
    kz_json:get_binary_value([<<"license">>], Doc, Default).

-spec set_license(doc(), binary()) -> doc().
set_license(Doc, License) ->
    kz_json:set_value([<<"license">>], License, Doc).

-spec masqueradable(doc()) -> boolean().
-spec masqueradable(doc(), Default) -> boolean() | Default.
masqueradable(Doc) ->
    masqueradable(Doc, true).
masqueradable(Doc, Default) ->
    kz_json:get_boolean_value([<<"masqueradable">>], Doc, Default).

-spec set_masqueradable(doc(), boolean()) -> doc().
set_masqueradable(Doc, Masqueradable) ->
    kz_json:set_value([<<"masqueradable">>], Masqueradable, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec phase(doc()) -> kz_term:api_binary().
-spec phase(doc(), Default) -> binary() | Default.
phase(Doc) ->
    phase(Doc, 'undefined').
phase(Doc, Default) ->
    kz_json:get_binary_value([<<"phase">>], Doc, Default).

-spec set_phase(doc(), binary()) -> doc().
set_phase(Doc, Phase) ->
    kz_json:set_value([<<"phase">>], Phase, Doc).

-spec price(doc()) -> kz_term:api_number().
-spec price(doc(), Default) -> number() | Default.
price(Doc) ->
    price(Doc, 'undefined').
price(Doc, Default) ->
    kz_json:get_float_value([<<"price">>], Doc, Default).

-spec set_price(doc(), number()) -> doc().
set_price(Doc, Price) ->
    kz_json:set_value([<<"price">>], Price, Doc).

-spec is_published(doc()) -> kz_term:api_boolean().
-spec is_published(doc(), Default) -> boolean() | Default.
is_published(Doc) ->
    is_published(Doc, 'undefined').
is_published(Doc, Default) ->
    kz_json:get_boolean_value([<<"published">>], Doc, Default).

-spec set_is_published(doc(), boolean()) -> doc().
set_is_published(Doc, Published) ->
    kz_json:set_value([<<"published">>], Published, Doc).

-spec publish(doc()) -> doc().
publish(Doc) ->
    set_is_published(Doc, 'true').

-spec unpublish(doc()) -> doc().
unpublish(Doc) ->
    set_is_published(Doc, 'false').


-spec screenshots(doc()) -> kz_term:api_ne_binaries().
-spec screenshots(doc(), Default) -> kz_term:ne_binaries() | Default.
screenshots(Doc) ->
    screenshots(Doc, 'undefined').
screenshots(Doc, Default) ->
    kz_json:get_list_value([<<"screenshots">>], Doc, Default).

-spec set_screenshots(doc(), kz_term:ne_binaries()) -> doc().
set_screenshots(Doc, Screenshots) ->
    kz_json:set_value([<<"screenshots">>], Screenshots, Doc).

-spec source_url(doc()) -> kz_term:api_binary().
-spec source_url(doc(), Default) -> binary() | Default.
source_url(Doc) ->
    source_url(Doc, 'undefined').
source_url(Doc, Default) ->
    kz_json:get_binary_value([<<"source_url">>], Doc, Default).

-spec set_source_url(doc(), binary()) -> doc().
set_source_url(Doc, SourceUrl) ->
    kz_json:set_value([<<"source_url">>], SourceUrl, Doc).

-spec tags(doc()) -> kz_term:api_ne_binaries().
-spec tags(doc(), Default) -> kz_term:ne_binaries() | Default.
tags(Doc) ->
    tags(Doc, 'undefined').
tags(Doc, Default) ->
    kz_json:get_list_value([<<"tags">>], Doc, Default).

-spec set_tags(doc(), kz_term:ne_binaries()) -> doc().
set_tags(Doc, Tags) ->
    kz_json:set_value([<<"tags">>], Tags, Doc).

-spec urls(doc()) -> kz_term:api_object().
-spec urls(doc(), Default) -> kz_json:object() | Default.
urls(Doc) ->
    urls(Doc, 'undefined').
urls(Doc, Default) ->
    kz_json:get_json_value([<<"urls">>], Doc, Default).

-spec set_urls(doc(), kz_json:object()) -> doc().
set_urls(Doc, Urls) ->
    kz_json:set_value([<<"urls">>], Urls, Doc).

-spec users(doc()) -> kz_term:api_ne_binaries().
-spec users(doc(), Default) -> kz_term:ne_binaries() | Default.
users(Doc) ->
    users(Doc, 'undefined').
users(Doc, Default) ->
    kz_json:get_list_value([<<"users">>], Doc, Default).

-spec set_users(doc(), kz_term:ne_binaries()) -> doc().
set_users(Doc, Users) ->
    kz_json:set_value([<<"users">>], Users, Doc).

-spec version(doc()) -> kz_term:api_binary().
-spec version(doc(), Default) -> binary() | Default.
version(Doc) ->
    version(Doc, 'undefined').
version(Doc, Default) ->
    kz_json:get_binary_value([<<"version">>], Doc, Default).

-spec set_version(doc(), binary()) -> doc().
set_version(Doc, Version) ->
    kz_json:set_value([<<"version">>], Version, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(kz_term:api_binary(), kz_term:api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
fetch('undefined', _) ->
    {'error', 'account_id_undefined'};
fetch(_, 'undefined') ->
    {'error', 'app_id_undefined'};
fetch(Account, Id) ->
    kz_datamgr:open_cache_doc(kz_util:format_account_db(Account), Id).
