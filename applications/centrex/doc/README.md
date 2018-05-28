
# Centrex *lightweight services pattern management*
*Less is better*

## Description

This application is designed to simplify the management of pseudo-accounts with standardized flows.

*For example*: we have 1000 users, where each user represents one customer (tenant), and all thouse users have the same call flow pattern. To change the service  delivered to thouse users, we can change oly one callflow pattern.

## Base centrex entities
Centrex - base centrex service description.

Schema for centrex

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`callflow_id` | The ID of a callflow pattern for this centrex service | `string()` |   | `true`
`description` | A friendly centrex service description | `string()` |   | `false`
`name` | A friendly centrex service name | `string()` |  | `true`

Centrex account - customer account for specified centrix service.

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`centrex_id` | Centrex service id | `string()` |   | `true`
`number_offnet` | Offnet number for user account | `string()` |   | `true`
`user_id` | he ID of single user for the centrex account | `string()` |  | `true`


Callflow patterns - set of callflow patterns which describe centrex service.
Callflow pattern schema is the same as for a typical callflow.


