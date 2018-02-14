-module(webhooks_conference_enter).
-vsn("0.0.2").

-export([init/0
    , bindings_and_responders/0

    , test_start_link/0, load_module_fold/2, load_module_bindings_and_responders/0]
).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(NAME, <<"Conference Enter">>).
-define(DESC, <<"This webhook is triggered when a participant entered in the conference">>).
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
    {[{'call', [{'restrict_to', ['CHANNEL_ANSWER']}
               ]
      }
     ]
    ,[{{'webhooks_channel_util', 'handle_event'}
      ,[{<<"call_event">>, <<"CHANNEL_ANSWER">>}]
      }
     ]
    }.


-define(SERVER, ?MODULE).
-define(QUEUE_NAME, <<"webhooks_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).
-define(BINDINGS, [{'conf', [{'restrict_to', ['doc_type_updates']}
    ,{'type', kzd_webhook:type()}
]
}
]).

-define(RESPONDERS, [{{?MODULE, 'handle_doc_type_update'}
    ,[{<<"configuration">>, <<"doc_type_update">>}]
}
]).
-type load_acc() :: {gen_listener:bindings()
    ,gen_listener:responders()
}.

-spec test_start_link() -> kz_types:startlink_ret().
test_start_link() ->
    {Bindings, Responders} = load_module_bindings_and_responders(),
    lager:debug("bindings: ~p", [Bindings]),
    io:format("Bindings=~p\nResponders=~p\n", [Bindings,Responders]).
%%    gen_listener:start_link({'local', ?SERVER}
%%    ,?MODULE
%%    ,[{'bindings', Bindings}
%%    ,{'responders', Responders}
%%    ,{'queue_name', ?QUEUE_NAME}
%%    ,{'queue_options', ?QUEUE_OPTIONS}
%%    ,{'consume_options', ?CONSUME_OPTIONS}
%%    ]
%%    ,[]
%%    ).

-spec load_module_bindings_and_responders() -> load_acc().
load_module_bindings_and_responders() ->
    lists:foldl(fun load_module_fold/2
        ,{?BINDINGS, ?RESPONDERS}
        ,webhooks_init:existing_modules()
    ).

-spec load_module_fold(atom(), load_acc()) -> load_acc().
load_module_fold(Module, {_Bindings, _Responders}=Acc) ->
%%    io:format("Module=~p\nBindings=~p\nResponders=~p\n",[Module, Bindings, Responders]),
    io:format("Module=~p\nAcc=~p\n",[Module, Acc]),
    Acc.

%%    try Module:bindings_and_responders() of
%%        {ModBindings, ModResponders} ->
%%            lager:debug("added ~s bindings and responders", [Module]),
%%            {ModBindings ++ Bindings
%%                ,ModResponders ++ Responders
%%            }
%%    catch
%%        'error':'undef' ->
%%            lager:debug("~s doesn't supply bindings or responders", [Module]),
%%            Acc;
%%        _E:_R ->
%%            lager:debug("~s failed to load bindings or responders: ~s: ~p"
%%                ,[Module, _E, _R]
%%            ),
%%            Acc
%%    end.