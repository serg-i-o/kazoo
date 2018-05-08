%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Listing of all expected v1 callbacks
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_centrix).

-export([init/0
%%        ,authenticate/1
%%        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
%%        ,content_types_provided/1
%%        ,content_types_accepted/1
%%        ,languages_provided/1
%%        ,charsets_provided/1
%%        ,encodings_provided/1
        ,validate/1, validate/2, validate/3, validate/4
        ,validate_request/1
%%        ,billing/1
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2, delete/3, delete/4
        ,etag/1
        ,expires/1
        ,finish_request/1
        ,save/1, save/2, save/3, save/4
        ]).

-include("crossbar.hrl").

-define(TYPE_CENTRIX, <<"centrix">>).
-define(TYPE_CENTRIX_USER_ACCOUNT, <<"centrix_account">>).
-define(CENTRIX_SCHEMA_NAME, <<"centrix">>).
-define(CENTRIX_ACCOUNT_SCHEMA_NAME, <<"centrix_accounts">>).

-define(CENTRIX_VIEW, <<"centrix/crossbar_listing">>).
-define(CENTRIX_ACCOUNTS_VIEW, <<"centrix/cx_accounts_by_centrix_id">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
%%    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
%%    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.centrix">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.centrix">>, ?MODULE, 'resource_exists'),
%%    _ = crossbar_bindings:bind(<<"*.content_types_provided.centrix">>, ?MODULE, 'content_types_provided'),
%%    _ = crossbar_bindings:bind(<<"*.content_types_accepted.centrix">>, ?MODULE, 'content_types_accepted'),
%%    _ = crossbar_bindings:bind(<<"*.languages_provided.centrix">>, ?MODULE, 'languages_provided'),
%%    _ = crossbar_bindings:bind(<<"*.charsets_provided.centrix">>, ?MODULE, 'charsets_provided'),
%%    _ = crossbar_bindings:bind(<<"*.encodings_provided.centrix">>, ?MODULE, 'encodings_provided'),
%%    _ = crossbar_bindings:bind(<<"*.validate_resource.centrix">>, ?MODULE, 'validate_resource'),
    _ = crossbar_bindings:bind(<<"*.validate.centrix">>, ?MODULE, 'validate'),
%%    _ = crossbar_bindings:bind(<<"*.billing">>, ?MODULE, 'billing'),
%%    _ = crossbar_bindings:bind(<<"*.execute.get.centrix">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.centrix">>, ?MODULE, 'save'),
    _ = crossbar_bindings:bind(<<"*.execute.post.centrix">>, ?MODULE, 'save'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.centrix">>, ?MODULE, 'save'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.centrix">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.etag.centrix">>, ?MODULE, 'etag'),
    _ = crossbar_bindings:bind(<<"*.expires.centrix">>, ?MODULE, 'expires'),
    _ = crossbar_bindings:bind(<<"*.finish_request">>, ?MODULE, 'finish_request').

%%%%------------------------------------------------------------------------------
%%%% @doc Authenticates the incoming request, returning true if the requestor is
%%%% known, or false if not.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec authenticate(cb_context:context()) -> 'false'.
%%authenticate(_Context) -> 'false'.
%%
%%%%-spec authenticate(cb_context:context()) -> boolean().
%%%%authenticate(Context) ->
%%%%    io:format("\n~p.authenticate/1:\nVerb=~p\nNouns=~p\n",
%%%%        [?MODULE,cb_context:req_verb(Context), cb_context:req_nouns(Context)]),
%%%%    maybe_authenticate(cb_context:req_verb(Context), cb_context:req_nouns(Context)).
%%%%
%%%%-spec maybe_authenticate(http_method(), req_nouns()) -> boolean().
%%%%maybe_authenticate(?HTTP_GET, [{<<"centrix">>, _Tokens}, {?KZ_ACCOUNTS_DB, [_AccountId]}]) ->
%%%%    io:format("authenticate = true\n"), 'true';
%%%%maybe_authenticate(_Verb, _Nouns) ->
%%%%    io:format("authenticate = false\n"),'false'.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc Authorizes the incoming request, returning true if the requestor is
%%%% allowed to access the resource, or false if not.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec authorize(cb_context:context()) -> 'false'.
%%authorize(_Context) -> 'false'.
%%
%%%%-spec authorize(cb_context:context()) -> boolean().
%%%%authorize(Context) ->
%%%%    io:format("\n~p.authorize/1:\nVerb=~p\nNouns=~p\n",
%%%%        [?MODULE,cb_context:req_verb(Context), cb_context:req_nouns(Context)]),
%%%%    maybe_authorize(cb_context:req_verb(Context), cb_context:req_nouns(Context)).
%%%%
%%%%maybe_authorize(?HTTP_GET, [{<<"centrix">>, _Tokens}, {?KZ_ACCOUNTS_DB, [_AccountId]}]) ->
%%%%    io:format("authorize = true\n"),'true';
%%%%maybe_authorize(_Verb, _Nouns) ->
%%%%    io:format("authorize = false\n"),'false'.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    Methods = [?HTTP_GET,?HTTP_PUT],
    io:format("\n~p.allowed_methods/0: (no path tokens) Methods=~p\n",[?MODULE, Methods]),
    Methods.

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(CentrixId) ->
    Methods = [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE],
    io:format("\n~p.allowed_methods/1:\nCentrixId=~p\nMethods=~p\n",[?MODULE, CentrixId, Methods]),
    Methods.

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME) ->
    Methods = [?HTTP_GET, ?HTTP_PUT, ?HTTP_DELETE],
    io:format("\n~p.allowed_methods/2:\nCentrixId=~p\nSecondPathToken=~p\nMethods=~p\n",
        [?MODULE,CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,Methods]),
    Methods.

-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId) ->
    Methods = [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE],
    io:format("\n~p.allowed_methods/2:\nCentrixId=~p\nSecondPathToken=~p\nCxAccountId=~p\nMethods=~p\n",
    [?MODULE,CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,CxAccountId,Methods]),
    Methods.

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /skels => []
%%    /skels/foo => [<<"foo">>]
%%    /skels/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_,_) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.
resource_exists(_,_,_) -> 'true'.

%%%%------------------------------------------------------------------------------
%%%% @doc What content-types will the module be using to respond (matched against
%%%% client's accept header).
%%%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec content_types_provided(cb_context:context()) -> cb_context:context().
%%content_types_provided(Context) ->
%%    Context.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc What content-types will the module be requiring (matched to the client's
%%%% Content-Type header.
%%%% Of the form `{atom(), [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}'
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec content_types_accepted(cb_context:context()) -> cb_context:context().
%%content_types_accepted(Context) ->
%%    Context.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc If you provide alternative languages, return a list of languages and optional
%%%% quality value.
%%%%
%%%% e.g.: `[<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]'
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec languages_provided(cb_context:context()) -> cb_context:context().
%%languages_provided(Context) ->
%%    Context.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc If you provide alternative charsets, return a list of charsets and optional
%%%% quality value.
%%%%  e.g. `[<<"iso-8859-5">>, <<"unicode-1-1;q=0.8">>]'
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec charsets_provided(cb_context:context()) -> cb_context:context().
%%charsets_provided(Context) ->
%%    Context.
%%
%%%%------------------------------------------------------------------------------
%%%% @doc If you provide alternative encodings, return a list of encodings and optional
%%%% quality value.
%%%% e.g. : `[<<"gzip;q=1.0">>, <<"identity;q=0.5">>, <<"*;q=0">>]'
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec encodings_provided(cb_context:context()) -> cb_context:context().
%%encodings_provided(Context) ->
%%    Context.

%%%%------------------------------------------------------------------------------
%%%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%%%% If valid, updates Context with centrixId
%%%%
%%%% Failure here returns `404 Not Found'.
%%%% @end
%%%%------------------------------------------------------------------------------
%%
%%-spec validate_resource(cb_context:context()) -> cb_context:context().
%%validate_resource(Context) -> Context.
%%
%%-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
%%validate_resource(Context, UserId) -> validate_user_id(UserId, Context).
%%
%%-spec validate_resource(cb_context:context(), path_token(), path_token()) -> cb_context:context().
%%validate_resource(Context, UserId, _) -> validate_user_id(UserId, Context).
%%
%%-spec validate_resource(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
%%validate_resource(Context, UserId, _, _) -> validate_user_id(UserId, Context).
%%
%%-spec validate_user_id(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
%%validate_user_id(UserId, Context) ->
%%    case kz_datamgr:open_cache_doc(cb_context:account_db(Context), UserId) of
%%        {'ok', Doc} -> validate_user_id(UserId, Context, Doc);
%%        {'error', 'not_found'} ->
%%            cb_context:add_system_error('bad_identifier'
%%                ,kz_json:from_list([{<<"cause">>, UserId}])
%%                ,Context
%%            );
%%        {'error', _R} -> crossbar_util:response_db_fatal(Context)
%%    end.
%%
%%-spec validate_user_id(kz_term:api_binary(), cb_context:context(), kz_json:object()) -> cb_context:context().
%%validate_user_id(UserId, Context, Doc) ->
%%    case kz_doc:is_soft_deleted(Doc) of
%%        'true' ->
%%            Msg = kz_json:from_list([{<<"cause">>, UserId}]),
%%            cb_context:add_system_error('bad_identifier', Msg, Context);
%%        'false'->
%%            cb_context:setters(Context
%%                ,[{fun cb_context:set_user_id/2, UserId}
%%                    ,{fun cb_context:set_resp_status/2, 'success'}
%%                ])
%%    end.


%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /centrix mights load a list of centrix services
%% /centrix/{CentrixId} might load the centrix service {CentrixId}
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    io:format("\n~p.validate/1:\nVerb=~p\n",[?MODULE,cb_context:req_verb(Context)]),
    validate_centrix(cb_context:req_verb(Context), Context, []).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, CentrixId) ->
    io:format("\n~p.validate/2:\nCentrixId=~p\n",[?MODULE, CentrixId]),
    validate_centrix(cb_context:req_verb(Context), Context, [CentrixId]).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, CentrixId, PathToken) ->
    io:format("\n~p.validate/3:\nCentrixId=~p\nSecondToken=~p\n",[?MODULE, CentrixId, PathToken]),
    validate_centrix(cb_context:req_verb(Context), Context, [CentrixId, PathToken]).

-spec validate(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
validate(Context, CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId) ->
    io:format("\n~p.validate/3:\nCentrixId=~p\nSecondToken=~p\nCxAccountId=~p\n",
        [?MODULE, CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]),
    validate_centrix(cb_context:req_verb(Context), Context,
        [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]
    ).


-spec validate_centrix(http_method(), cb_context:context(), path_tokens()) -> cb_context:context().
validate_centrix(?HTTP_GET, Context, []) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_GET,[]]),
    crossbar_doc:load_view(?CENTRIX_VIEW, [], Context, fun normalize_view_results/2);
validate_centrix(?HTTP_PUT, Context, []) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_PUT,[]]),
    validate_doc('undefined', ?TYPE_CENTRIX, Context);


validate_centrix(?HTTP_GET, Context, [CentrixId]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId]\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_GET,[CentrixId]]),
    crossbar_doc:load(CentrixId, Context, ?TYPE_CHECK_OPTION(?TYPE_CENTRIX));
validate_centrix(?HTTP_DELETE, Context, [CentrixId]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId]\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_DELETE,[CentrixId]]),
    crossbar_doc:load_view(?CENTRIX_ACCOUNTS_VIEW, [{'key', CentrixId}], Context);
validate_centrix(?HTTP_POST, Context, [CentrixId]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId]\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_POST,[CentrixId]]),
    validate_doc(CentrixId, ?TYPE_CENTRIX, Context);
validate_centrix(?HTTP_PATCH, Context, [CentrixId]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId]\nMethod=~p\nPathTokens=~p\n",[?MODULE,?HTTP_PATCH,[CentrixId]]),
    patch_and_validate_doc(CentrixId, ?TYPE_CENTRIX, Context);


validate_centrix(?HTTP_GET, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId, <<\"centrix_accounts\">>]\nMethod=~p\nPathTokens=~p\nOptions=~p\n",
        [?MODULE,?HTTP_GET,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME],[{'key', CentrixId}]]),
    crossbar_doc:load_view(?CENTRIX_ACCOUNTS_VIEW, [{'key', CentrixId}], Context);
validate_centrix(?HTTP_PUT, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId, <<\"centrix_accounts\">>]\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_PUT,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME]]),
    ReqData = kz_json:set_values([{<<"centrix_id">>, CentrixId}], cb_context:req_data(Context)),
    io:format("ReqData=~p\n",[ReqData]),
    validate_doc('undefined', ?TYPE_CENTRIX_USER_ACCOUNT, cb_context:set_req_data(Context, ReqData));
validate_centrix(?HTTP_DELETE, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME]) ->
    io:format("\n~p.validate_centrix/3: [CentrixId, <<\"centrix_accounts\">>]\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_DELETE,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME]]),
    crossbar_doc:load_view(?CENTRIX_ACCOUNT_SCHEMA_NAME, [{'key', CentrixId}], Context);


validate_centrix(?HTTP_GET, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_GET,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,CxAccountId]]),
    crossbar_doc:load(CxAccountId, Context, ?TYPE_CHECK_OPTION(?TYPE_CENTRIX_USER_ACCOUNT));
validate_centrix(?HTTP_DELETE, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_DELETE,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,CxAccountId]]),
    crossbar_doc:load(CxAccountId, Context, ?TYPE_CHECK_OPTION(?TYPE_CENTRIX_USER_ACCOUNT));
validate_centrix(?HTTP_POST, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_POST,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,CxAccountId]]),
    validate_doc(CxAccountId, ?TYPE_CENTRIX_USER_ACCOUNT, Context);
validate_centrix(?HTTP_PATCH, Context, [CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, CxAccountId]) ->
    io:format("\n~p.validate_centrix/3:\nMethod=~p\nPathTokens=~p\n",
        [?MODULE,?HTTP_PATCH,[CentrixId,?CENTRIX_ACCOUNT_SCHEMA_NAME,CxAccountId]]),
    patch_and_validate_doc(CxAccountId, ?TYPE_CENTRIX_USER_ACCOUNT, Context).


-spec validate_doc(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_doc(Id, Type, Context) ->
    OnSuccess = fun(C) -> on_successfull_validation(Id, Type, C) end,
    cb_context:validate_request_data(type_schema_name(Type), Context, OnSuccess).

-spec patch_and_validate_doc(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
patch_and_validate_doc(Id, Type, Context) ->
    ValidateFun = fun(DocId, C) -> validate_doc(DocId, Type, C) end,
    crossbar_doc:patch_and_validate(Id, Context, ValidateFun).

-spec type_schema_name(kz_term:api_binary()) -> kz_term:api_binary().
type_schema_name(?TYPE_CENTRIX) -> ?CENTRIX_SCHEMA_NAME;
type_schema_name(?TYPE_CENTRIX_USER_ACCOUNT) -> ?CENTRIX_ACCOUNT_SCHEMA_NAME;
type_schema_name(_Type) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(cb_context:context()) -> cb_context:context().
validate_request(Context) ->
    cb_context:validate_request_data(?CENTRIX_VIEW, Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT or POST or PATCH save context doc to db.
%% @end
%%------------------------------------------------------------------------------

-spec save(cb_context:context()) -> cb_context:context().
save(Context) ->
    crossbar_doc:save(Context).

-spec save(cb_context:context(), path_token()) -> cb_context:context().
save(Context, _CentrixId) ->
    save(Context).

-spec save(cb_context:context(), path_token(), path_token()) -> cb_context:context().
save(Context, _CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME) ->
    save(Context).

-spec save(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
save(Context, _CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, _CxAccountId) ->
    save(Context).

%%%%------------------------------------------------------------------------------
%%%% @doc If you handle billing-related calls, this callback will allow you to
%%%% execute those.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec billing(cb_context:context()) -> cb_context:context().
%%billing(Context) ->
%%    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, CentrixId) ->
    _ = delete(Context, CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME),
    io:format("\n~p.delete/2:\nContext=~p\nCentrixId=~p\nOptions=~p\n",
        [?MODULE,Context,CentrixId,?TYPE_CHECK_OPTION(?TYPE_CENTRIX)]),
    Context1 = crossbar_doc:load(CentrixId, Context, ?TYPE_CHECK_OPTION(?TYPE_CENTRIX)),
    crossbar_doc:delete(Context1).

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME) ->
    Docs = [kz_json:get_value(<<"id">>, Entry) || Entry <- cb_context:doc(Context)],
    AccountDb = kz_util:format_account_id(cb_context:account_db(Context), 'encoded'),
    %% do we need 'soft' delete as in crossbar_doc?
    kz_datamgr:del_docs(AccountDb, Docs),
    Context.

-spec delete(cb_context:context(), path_token(), path_token(), path_token()) -> cb_context:context().
delete(Context, _CentrixId, ?CENTRIX_ACCOUNT_SCHEMA_NAME, _CxAccountId) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc If you want to manipulate the etag header, change it here in the cb_context{}
%% @end
%%------------------------------------------------------------------------------
-spec etag(cb_context:context()) -> cb_context:context().
etag(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc Set the expires header
%% @end
%%------------------------------------------------------------------------------
-spec expires(cb_context:context()) -> cb_context:context().
expires(Context) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc The response has gone out, do some cleanup of your own here.
%% @end
%%------------------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> cb_context:context().
finish_request(Context) ->
    Context.

%%%%------------------------------------------------------------------------------
%%%% @doc Create a new instance with the data provided, if it is valid
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec create(cb_context:context()) -> cb_context:context().
%%create(Context) ->
%%    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
%%    cb_context:validate_request_data(DocType, Context, OnSuccess).

%%%%------------------------------------------------------------------------------
%%%% @doc Load an instance from the database
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
%%read(Id, Context) ->
%%    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"skel">>)).

%%%%------------------------------------------------------------------------------
%%%% @doc Update an existing menu document with the data provided, if it is
%%%% valid
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
%%update(Id, Context) ->
%%    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
%%    cb_context:validate_request_data(<<"skels">>, Context, OnSuccess).

%%%%------------------------------------------------------------------------------
%%%% @doc Update-merge an existing menu document with the data provided, if it is
%%%% valid
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
%%validate_patch(Id, Context) ->
%%    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%%%------------------------------------------------------------------------------
%%%% @doc Attempt to load a summarized listing of all instances of this
%%%% resource.
%%%% @end
%%%%------------------------------------------------------------------------------
%%-spec summary(cb_context:context()) -> cb_context:context().
%%summary(Context) ->
%%    io:format("\n~p.summary/1: Try load centrix list\n",[?MODULE]),
%%    crossbar_doc:load_view(?CENTRIX_VIEW, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successfull_validation(kz_term:api_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
on_successfull_validation('undefined', Type, Context) ->
    io:format("\n~p.on_successfull_validation/3: DocType=~p, DocId=~p\n",[?MODULE,Type,""]),
    Doc = kz_json:set_values([{<<"pvt_type">>, Type}], cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successfull_validation(Id, Type, Context) ->
    io:format("\n~p.on_successfull_validation/3: DocType=~p, DocId=~p\n",[?MODULE,Type,Id]),
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(Type)).

%%-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
%%on_successful_validation('undefined', Context) ->
%%    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"centrix">>));
%%on_successful_validation(Id, Context) ->
%%    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"centrix">>)).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].
