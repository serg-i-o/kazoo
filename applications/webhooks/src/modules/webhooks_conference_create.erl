%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_conference_create).

-export([init/0
        , bindings_and_responders/0
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"Conference create">>).
-define(DESC, <<"This webhook is triggered when the conference created by first participant">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ])
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {
     [{'conference',
       [{'restrict_to',
         [{'event',
           [
            {'event', <<"conference-create">>},
            {'account_id', <<"*">>},
            {'conference_id', <<"*">>},
            {'call_id', <<"*">>}
           ]
          }]
        }]
      }],
     [{{'webhooks_conference_util', 'handle_event'},[{<<"conference">>, <<"event">>}]}]
    }.
