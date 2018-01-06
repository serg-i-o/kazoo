-module(kzd_mailbox_message).

-export([new/0]).
-export([call_id/1, call_id/2, set_call_id/2]).
-export([caller_id_name/1, caller_id_name/2, set_caller_id_name/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([folder/1, folder/2, set_folder/2]).
-export([from/1, from/2, set_from/2]).
-export([length/1, length/2, set_length/2]).
-export([media_id/1, media_id/2, set_media_id/2]).
-export([timestamp/1, timestamp/2, set_timestamp/2]).
-export([to/1, to/2, set_to/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec call_id(doc()) -> api_binary().
-spec call_id(doc(), Default) -> binary() | Default.
call_id(Doc) ->
    call_id(Doc, 'undefined').
call_id(Doc, Default) ->
    kz_json:get_binary_value([<<"call_id">>], Doc, Default).

-spec set_call_id(doc(), binary()) -> doc().
set_call_id(Doc, CallId) ->
    kz_json:set_value([<<"call_id">>], CallId, Doc).

-spec caller_id_name(doc()) -> api_binary().
-spec caller_id_name(doc(), Default) -> binary() | Default.
caller_id_name(Doc) ->
    caller_id_name(Doc, 'undefined').
caller_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_name">>], Doc, Default).

-spec set_caller_id_name(doc(), binary()) -> doc().
set_caller_id_name(Doc, CallerIdName) ->
    kz_json:set_value([<<"caller_id_name">>], CallerIdName, Doc).

-spec caller_id_number(doc()) -> api_binary().
-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_number">>], Doc, Default).

-spec set_caller_id_number(doc(), binary()) -> doc().
set_caller_id_number(Doc, CallerIdNumber) ->
    kz_json:set_value([<<"caller_id_number">>], CallerIdNumber, Doc).

-spec folder(doc()) -> api_binary().
-spec folder(doc(), Default) -> binary() | Default.
folder(Doc) ->
    folder(Doc, 'undefined').
folder(Doc, Default) ->
    kz_json:get_binary_value([<<"folder">>], Doc, Default).

-spec set_folder(doc(), binary()) -> doc().
set_folder(Doc, Folder) ->
    kz_json:set_value([<<"folder">>], Folder, Doc).

-spec from(doc()) -> api_binary().
-spec from(doc(), Default) -> binary() | Default.
from(Doc) ->
    from(Doc, 'undefined').
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec length(doc()) -> api_integer().
-spec length(doc(), Default) -> integer() | Default.
length(Doc) ->
    length(Doc, 'undefined').
length(Doc, Default) ->
    kz_json:get_integer_value([<<"length">>], Doc, Default).

-spec set_length(doc(), integer()) -> doc().
set_length(Doc, Length) ->
    kz_json:set_value([<<"length">>], Length, Doc).

-spec media_id(doc()) -> api_ne_binary().
-spec media_id(doc(), Default) -> ne_binary() | Default.
media_id(Doc) ->
    media_id(Doc, 'undefined').
media_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"media_id">>], Doc, Default).

-spec set_media_id(doc(), ne_binary()) -> doc().
set_media_id(Doc, MediaId) ->
    kz_json:set_value([<<"media_id">>], MediaId, Doc).

-spec timestamp(doc()) -> api_integer().
-spec timestamp(doc(), Default) -> integer() | Default.
timestamp(Doc) ->
    timestamp(Doc, 'undefined').
timestamp(Doc, Default) ->
    kz_json:get_integer_value([<<"timestamp">>], Doc, Default).

-spec set_timestamp(doc(), integer()) -> doc().
set_timestamp(Doc, Timestamp) ->
    kz_json:set_value([<<"timestamp">>], Timestamp, Doc).

-spec to(doc()) -> api_binary().
-spec to(doc(), Default) -> binary() | Default.
to(Doc) ->
    to(Doc, 'undefined').
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).
