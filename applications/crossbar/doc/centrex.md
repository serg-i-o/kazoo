### Centrex

#### About Centrex app

[Centrex](../../centrex/doc/README.md) is the module for lightweight services pattern management.


#### Schema

### Centrex service

#### Fetch centrex list

> GET /v2/accounts/{ACCOUNT_ID}/centrex

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex
```

```json
{
    "page_size": 2,
    "data": [
        {
            "id": "{CENTREX_ID_1}",
            "name": "centrex_name_1",
            "description": "cetnrex service 1",
            "callflow_id": "{CALLFLOW_ID_1}"
        },
        {
            "id": "{CENTREX_ID_2}",
            "name": "centrex_name_2",
            "description": "cetnrex service 2",
            "callflow_id": "{CALLFLOW_ID_2}"
        }
    ],
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Get centrex

> GET /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}
```

```json
{
    "data": {
        "name": "centrex_main_service",
        "description": "Centrex main service",
        "callflow_id": "{CALLFLOW_ID}",
        "id": "{CENTREX_ID}"
    },
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Create a centrex

> PUT /v2/accounts/{ACCOUNT_ID}/centrex

Setting *"id" = "{CENTREX_ID}"* is optional

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "id": "{CENTREX_ID}",
        "name": "centrex_main_service",
        "description": "Centrex main service",
        "callflow_id": "{CALLFLOW_ID}"
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex
```

```json
{
    "data": {
        "id": "{CENTREX_ID}",
        "name": "centrex_main_service",
        "description": "Centrex main service",
        "callflow_id": "{CALLFLOW_ID}"
    }
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Delete a centrex

> DELETE /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENЕREX_ID}
```

```json
 {
    "page_size": 0,
    "data": {
        "name": "centrex_main_service",
        "description": "Centrex main service",
        "callflow_id": "{CALLFLOW_ID}",
        "id": "{CENTREX_ID}",
        "_read_only": {
            "deleted": true
        }
    },
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Update a centrex

> POST /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}

> PATCH /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}


```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "name": "centrex_main_service",
        "description": "Centrex main service",
        "callflow_id": "{CALLFLOW_ID}"
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}
```

```json
{
    "data": {
        "name": "centrex_main_service_fix",
        "description": "Centrex main service fix",
        "callflow_id": "{CALLFLOW_ID}",
        "id": "{CENTREX_ID}"
    }
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

### Centrex accounts

#### Fetch centrex accounts list

> GET /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts
```

```json
{
    "page_size": 2,
    "data": [
        {
            "id": "{CENTREX_ACCOUNT_ID_1}",
            "key": "{CENTREX_ID}",
            "value": {
                "id": "{CENTREX_ACCOUNT_ID_1}",
                "centrex_id": "{CENTREX_ID}",
                "user_id": "{USER_ID_1}",
                "number": "{NUMBER_OFFNET_1}"
            }
        },
        {
            "id": "{CENTREX_ACCOUNT_ID_2}",
            "key": "{CENTREX_ID}",
            "value": {
                "id": "{CENTREX_ACCOUNT_ID_2}",
                "centrex_id": "{CENTREX_ID}",
                "user_id": "{USER_ID_1}",
                "number": "{NUMBER_OFFNET_2}"
            }
        }
    ],
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Get centrex

> GET /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}
```

```json
{
    "data": {
        "centrex_id": "{CENTREX_ID}",
        "name": "centrex_account_name_test",
        "user_id": "{USER_ID}",
        "number_offnet": "{NUMBER_OFFNET}",
        "description": "centrex test user account",
        "id": "{CENTREX_ACCOUNT_ID}"
    },
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Create a centrex

> PUT /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts

Setting *"id" = "{CENTREX_ACCOUNT_ID}"* is optional

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
		"id": "{CENTREX_ACCOUNT_ID}",
		"centrex_id": "{CENTREX_ID}",
		"name": "centrex_account_name_test",
		"user_id": "{USER_ID}",
		"number_offnet": "{NUMBER_OFFNET}",
		"description": "centrex test user account"
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts
```

```json
{
    "data": {
		"id": "{CENTREX_ACCOUNT_ID}",
		"centrex_id": "{CENTREX_ID}",
		"name": "centrex_account_name_test",
		"user_id": "{USER_ID}",
		"number_offnet": "{NUMBER_OFFNET}",
		"description": "centrex test user account"
    }
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Delete a centrex

> DELETE /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}
```

```json
 {
    "page_size": 0,
    "data": {
		"id": "{CENTREX_ACCOUNT_ID}",
		"centrex_id": "{CENTREX_ID}",
		"name": "centrex_account_name_test",
		"user_id": "{USER_ID}",
		"number_offnet": "{NUMBER_OFFNET}",
		"description": "centrex test user account"
        "_read_only": {
            "deleted": true
        }
    },
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```

#### Update a centrex

> POST /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}

> PATCH /v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}


```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
		"centrex_id": "{CENTREX_ID}",
		"name": "centrex_account_name_test_fix",
		"user_id": "{USER_ID}",
		"number_offnet": "{NUMBER_OFFNET_NEW}",
		"description": "centrex test user account_fix"
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/centrex/{CENTREX_ID}/centrex_accounts/{CENTREX_ACCOUNT_ID}
```

```json
{
    "data": {
    	"id": "{CENTREX_ACCOUNT_ID}",
		"centrex_id": "{CENTREX_ID}",
		"name": "centrex_account_name_test_fix",
		"user_id": "{USER_ID}",
		"number_offnet": "{NUMBER_OFFNET_NEW}",
		"description": "centrex test user account_fix"
    }
    "auth_token": "{AUTH_TOKEN}",
    ...
}
```
