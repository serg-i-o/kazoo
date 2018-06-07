%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_conference_command).

-export([init/0
        , bindings_and_responders/0
        ]).

-include("webhooks.hrl").

-define(CONFERENCE_ACTIONS, ['play','mute','deaf','kick']).

-define(CONF_ID_MODIFIER
       ,kz_json:from_list(
          [
           {<<"type">>, <<"string">>}
          ,{<<"description">>, <<"Conferences id to handle">>}
          ]
         )
       ).

-define(ACTIONS_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of conference commands to handle">>}
          ,{<<"items">>, ?CONFERENCE_ACTIONS}
          ]
         )
       ).

-define(MODIFIERS
       ,kz_json:from_list(
          [
           {<<"action">>, ?ACTIONS_MODIFIER},
           {<<"conference_id">>, ?CONF_ID_MODIFIER}
          ]
         )
       ).

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"Conference command">>).
-define(DESC, <<"This webhook is triggered on command in the specified conference">>).

-define(METADATA
       ,kz_json:from_list(
          [{<<"_id">>, ?ID}
          ,{<<"name">>, ?NAME}
          ,{<<"description">>, ?DESC}
          ,{<<"modifiers">>, ?MODIFIERS}
          ]
         )
       ).


-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {
     [{'conference', [{'restrict_to',[ {'command', <<"*">>}]}]}],
     [{{'webhooks_conference_util', 'handle_event'},[{<<"conference">>, <<"command">>}]}]
    }.
