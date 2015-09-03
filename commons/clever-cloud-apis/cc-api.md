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

Clever Cloud API works with an OAuth1 based authentication.

 * [Create some tokens](https://console.clever-cloud.com/users/me/tokens) in
the Clever Cloud console.
 * [Get a request token](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_request_token_post)
 * [Get the authorisation URL](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_authorize_get)
 * Connect with your account on the url
 * Get the `oauth_verifier` in the callback url
 * [Generate the access token](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_access_token_post)
 with your request token and the verifier.
 * You can use this access token to make OAuth1 signed requests.

### API request links

All the API links are referenced in a swagger documentation.

 * [https://www.clever-cloud.com/doc/api/](https://www.clever-cloud.com/doc/api/)

The base URL for the API is :

 * `https://api.clever-cloud.com/v2/`

## WebSocket API requests

### Connection protocol

Clever Cloud API can handle WebSocket-Security requests for the logs or events.
To connect to a WebSocket API URL follow this guide.

 * Take a URL in the API for the WebSocket.
 * Ex : `https://api.clever-cloud.com/v2/events/event-socket`
 * Sign the OAuth request with this url.
 * Replace `https://` by `wss://`
 * `wss://api.clever-cloud.com/v2/events/event-socket`
 * Connect to this url in WebSocket
 * When the WebSocket connection is opened, you need to send the OAuth1 header in
this format :
```json
{
	"message_type": "oauth",
	"authorization": "<oauth_header>"
}
```
You need to replace `<oauth_header>` by the signed OAuth1 header.

### Avaible WebSocket

 * [Logs](https://www.clever-cloud.com/doc/api/#!/logs/logs_logs-socket_appId_get)
 * [Events](https://www.clever-cloud.com/doc/api/#!/events/events_event-socket_get)