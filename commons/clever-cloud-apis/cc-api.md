---
title: Clever Cloud API
position: 1
---
# Clever Cloud API

Clever Cloud console is built on top of an API that allow you to manage your
account and products. This article will explain how to connect to this API and
use it.

## Make a request on the API

### Authentication

Clever Cloud API works with an OAuth based authentication. First, you need to
[create some tokens](https://console.clever-cloud.com/users/me/tokens) in
the Clever Cloud console. Use these tokens to make an OAuth authentication.

### API request links

All the API links are referenced in a swagger documentation.

 * [https://www.clever-cloud.com/doc/api/](https://www.clever-cloud.com/doc/api/)

The base URL for the API is :

 * `https://api.clever-cloud.com/v2/`

## Logs API

You have two ways to get logs of an application, you can get them via http or
open a stream via websocket.

### Http method

You can request the logs of an application on this URL :

 * `https://api.clever-cloud.com/v2/logs/<app_id>?limit=<limit>`
 * `<app_id>` is the id of the app you request the logs.
 * `<limit>` is the number of log lines you want to have.
 * **Note :** For using HMAC authentication you need to sign your request with
 this URL :
 * `https://api.clever-cloud.com/v2/logs/<app_id>`

### Websocket Method

You can request the logs in a websocket stream, to get real time logs.

 * `wss://logs-api.clever-cloud.com/logs-socket/<app_id>?since=<timestamp>`
 * `<app_id>` is the id of the app you request the logs.
 * `<timestamp>` the timestamp of the first log you will recieve in `iso 8601`.
 * **Note :** For using HMAC authentication you need to sign your request with
 this URL :
 * `https://api.clever-cloud.com/v2/logs/logs-socket/<app_id>`

When the websocket connection is opened, you need to send the OAuth header in
this format :
```json
{
	"message_type": "oauth",
	"authorization": "<oauth_header>"
}
```
You need to replace `<oauth_header>` by a signed OAuth header.

When you are connected to the websocket logs-api, the server will send you a
json each time there is a log.

## Event API

You can stream events on your account via websocket. This is used by the console
for the notifications.

 * `wss://event-api-clever-cloud.cleverapps.io/event-socket/`
 * **Note :** For using HMAC authentication you need to sign your request with
 this URL :
 * `https://api.clever-cloud.com/v2/events/event-socket`

The authentication is the same as logs via websocket.