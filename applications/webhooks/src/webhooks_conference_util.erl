-module(webhooks_conference_util).

-export([handle_event/2]).

-define(CONFERENCE_COMMAND_HOOK_EVENT, <<"conference_command">>).
-define(CONFERENCE_DESTROY_EVENT, <<"conference_destroy">>).

-include("webhooks.hrl").

-spec handle_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    BaseEventName = erlang:binary_to_atom(kz_json:get_value(<<"Event-Name">>, JObj), 'utf8'),
    case BaseEventName of
        'event'   ->
            HookEvent = hook_event_name(kz_json:get_value(<<"Event">>, JObj)),
            handle_conf_event(JObj, HookEvent);
        'command' -> handle_conf_command(JObj, ?CONFERENCE_COMMAND_HOOK_EVENT);
        _ -> lager:debug("failed to determine event-name")
    end.

-spec handle_conf_event(kz_json:object(), kz_term:ne_binary()) -> any().
handle_conf_event(JObj, ?CONFERENCE_DESTROY_EVENT) ->
    case kz_json:get_value(<<"Account-ID">>, JObj) of
        'undefined' ->
            lager:debug("failed to determine account id for conference_destroy event");
        AccountId ->
            maybe_handle_conf_event(AccountId, ?CONFERENCE_DESTROY_EVENT, JObj)
    end;
handle_conf_event(JObj, HookEvent) ->
    case kz_hooks_util:lookup_account_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            maybe_handle_conf_event(AccountId, HookEvent, JObj)
    end.

-spec handle_conf_command(kz_json:object(), kz_term:ne_binary()) -> any().
handle_conf_command(JObj, HookEvent) ->
    CommandName = conf_command_name(kz_json:get_value(<<"Application-Name">>, JObj)),
    handle_conf_command(JObj, HookEvent, CommandName).
handle_conf_command(_JObj, _HookEvent, 'undefined') ->
    lager:debug("failed to determine command for the event");
handle_conf_command(JObj, HookEvent, CommandName) ->
    case kz_hooks_util:lookup_conference_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine conference id for the event");
        {'ok', ConferenceId} ->
            maybe_handle_conf_command(HookEvent, ConferenceId, CommandName, JObj)
    end.

-spec maybe_handle_conf_command(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_handle_conf_command(HookEvent, ConferenceId, CommandName, JObj) ->
    case webhooks_util:find_conference_webhooks(HookEvent, ConferenceId, CommandName) of
        [] -> lager:debug("no hooks to handle conference with id ~s", [ConferenceId]);
        Hooks ->
            webhooks_util:fire_hooks(JObj, Hooks)
    end.

%% @public
-spec maybe_handle_conf_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_handle_conf_event(AccountId, HookEvent, JObj) ->
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks ->
            webhooks_util:fire_hooks(JObj, Hooks)
    end.

-spec hook_event_name(kz_term:ne_binary()) -> kz_term:ne_binary().
hook_event_name(<<"conference-create">>) -> <<"conference_create">>;
hook_event_name(<<"conference-destroy">>) -> <<"conference_destroy">>;
hook_event_name(<<"add-member">>) -> <<"conference_add_member">>;
hook_event_name(<<"del-member">>) -> <<"conference_del_member">>;
hook_event_name(Event) -> Event.

-spec conf_command_name(kz_term:ne_binary()) -> kz_term:ne_binary().
conf_command_name(<<"mute_participant">>) -> <<"mute">>;
conf_command_name(<<"unmute_participant">>) -> <<"mute">>;
conf_command_name(<<"deaf_participant">>) -> <<"deaf">>;
conf_command_name(<<"undeaf_participant">>) -> <<"deaf">>;
conf_command_name(Command) -> Command.
