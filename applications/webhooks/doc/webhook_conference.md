
# Conference webhooks

### Hooks

* Create conference
    * hook_event: conference_create
* Destroy conference
    * hook_event: conference_destroy
* Add member to the conference
    * hook_event: conference_add_member
* Delete member from the conference
    * hook_event: conference_ del_member
* Conference command
    * hook_event: conference_command
    * action modifiers: 'play','mute','deaf','kick'
    * conference_id modifier: <<"your_conference_id">>

#### Hook Specific Custom Data for *conference_command*

To set hook for the command in the conference you must specify conference id from your account and action:
```json
{
   "conference_id": "your_conference_id",
   "action": "mute"
}
```
#### Example conference_command webhook

> GET /v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/webhooks/{WEBHOOK_ID}
```
```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "test-kick",
        "hook": "conference_command",
        "include_subaccounts": false,
        "http_verb": "get",
        "retries": 1,
        "uri": "http://127.0.0.1:8080",
        "custom_data": {
            "conference_id": "{CONFERENCE_ID}",
            "action": "kick"
        },
        "enabled": true,
        "include_internal_legs": true,
        "id": "{WEBHOOK_ID}"
    },
    "revision": "{REVISION_ID}",
    "timestamp": "2018-03-12T12:33:38",
    "version": "4.0.0",
    "node": "{NODE_ID}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```