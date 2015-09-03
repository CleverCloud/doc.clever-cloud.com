---
title: Clever Cloud API
position: 1
---
# Clever Cloud API

The Clever Cloud console is built on top of an API that allow you to manage your
account and products. This article will explain how to connect to this API and
use it.

## HTTP calls

### **Authentication**

Clever Cloud API works with an OAuth1 based authentication.

#### **Create consumers tokens**

You need to [create some consumers tokens](https://console.clever-cloud.com/users/me/tokens)
in the Clever Cloud console. Theses tokens allow you to select the permissions
you ask to the user which permissions you need for your application. You need
to set a callback URL, it will send you to this URL after authentication.

#### **Get a request token**

You have to make a `POST`request to get a
[request token](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_request_token_post)
to the API.

#### **Get the authorisation URL**

Ask the API for the [authorisation URL](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_authorize_get)
and go to this URL with a browser. Log in with your account and it will send you
to the callback URL.

#### **Get the verifier token**

In the callback URL you have the verifier token :

`http://www.example.com/callback?oauth_verifier=<verifierToken>`

Where `<verifierToken>` is your token.

#### **Get the access token**
Make a `POST`  request to get the
[access token](https://www.clever-cloud.com/doc/api/#!/oauth/oauth_access_token_post)
 with your request token and the verifier.
You can use this access token to make OAuth1 signed requests.

More information about [OAuth dance](http://oauth.net/core/1.0/#anchor9).

### **API request links**

All the API links are referenced in a swagger documentation.

 * [https://www.clever-cloud.com/doc/api/](https://www.clever-cloud.com/doc/api/)

The base URL for the API is :

 * `https://api.clever-cloud.com/v2/`

## WebSocket API requests

### **Connection protocol**

Clever Cloud API can handle WebSocket-Security requests for the logs or events.
To connect to a WebSocket API URL follow this guide.

 * Take a URL in the API for the WebSocket.
 * Ex : `https://api.clever-cloud.com/v2/events/event-socket`
 * Sign the OAuth request with this URL.
 * Replace `https://` by `wss://`
 * `wss://api.clever-cloud.com/v2/events/event-socket`
 * Connect to this URL in WebSocket
 * When the WebSocket connection is opened, you need to send the OAuth1 header in
this format :
```json
{
	"message_type": "oauth",
	"authorization": "<oauth_header>"
}
```
You need to replace `<oauth_header>` by the signed OAuth1 header.

### **Available WebSocket endpoints**

#### **Logs-Socket**

[This endpoint](https://www.clever-cloud.com/doc/api/#!/logs/logs_logs-socket_appId_get)
allow you to receive real-time logs of an application via WebSocket.

#### **Events**
[This endpoint](https://www.clever-cloud.com/doc/api/#!/events/events_event-socket_get)
allow you to receive a stream of events on you account. It can be application
deployments that success or fail, application creation or deletion.
