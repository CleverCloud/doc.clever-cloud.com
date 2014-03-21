---
title: Add-on API
position: 1
---
# Clever Cloud add-on API

Clever Cloud offers you to sell your service as an add-on through the dashboard.
This article will document how you can technically plug into the platform to provide add-on provision, deprovision, and configuration via the dashboard.

There are two faces here:

* [The addon provider API](#addon-provider-api) - The API *you* need to provide and document to allow the Clever Cloud's backend to request provision and deprovision of add-ons.

* [The addon infos API](#addon-infos-api) - The API Clever Cloud provides to allow you to get informations about provisioned add-ons and their owners.

## Preamble

In the PaaS world, an add-on system is preceding the others, therefore is well known by a lot of developers: the Heroku one. To ease up the transition between Heroku and Clever Cloud, we decided to support (then iterate over) the Heroku standard for add-on provisioning API.

So, if you already have been integrated as an add-on provider for the Heroku platform, you will have no trouble integrating in the Clever Cloud's add-on platform. This explains why we handle "heroku\_id" fields instead of "clevercloud\_id" or equivalent.

## Addon Provider API

This is the API *you* need to provide to allow Clever Cloud to provision an add-on for a customer.

### How it works

When a Clever Cloud customer uses the marketplace to provision an add-on, Clever Cloud sends a request to your service which provides a new add-on for this app.

### Build an add-on

You can write your service in any language.

You need to create a manifest file, this file is a JSON that describes your add-on.

#### Example manifest

```json
{
  "id": "addon-name",
  "api": {
    "config_vars": [ "ADDON_NAME_MY_VAR" ],
    "regions": [ "eu" ],
    "password": "44ca82ddf8d4e74d52494ce2895152ee",
    "sso_salt": "fcb5b3add85d65e1dddda87a115b429f",
    "production": {
      "base_url": "https://yourservice.com/clevercloud/resources",
      "sso_url": "https://yourservice.com/clevercloud/sso/login"
    },
    "test": {
      "base_url": "http://localhost:9000/clevercloud/resources",
      "sso_url": "http://localhost:9000/clevercloud/sso/login"
    }
  }
}
```

Fields

* `id` - An id for your add-on. All lower case, no spaces or punctuation. Underscores and dashes are allowed. This can’t be changed after the first push. It is also used for HTTP basic auth when making provisioning calls.

* `api/config_vars` - A list of config vars that will be returned on provisioning calls. Each config_var name *MUST* start with the capitalized, underscorized addon id, as in the example.

* `api/password` - Password that Clever Cloud will send in HTTP basic auth when making provisioning calls. You should generate a long random string for that field.

* `api/sso_salt` - Shared secret used in single sign-on between the Clever Cloud admin panel and your service’s admin panel. You should generate a long random string for that field.

* `api/regions` - The list of geographical zones supported by your add-on. It cannot be empty. As for now, it *MUST* contain the element "eu". More will be supported.

* `api/production/base_url` - The production endpoint for Clever Cloud api actions (provision, deprovision, and plan change).

* `api/production/sso_url` - The production endpoint for single sign-on.

* `api/test/base_url` - The test endpoint for Clever Cloud api actions. Used to test your service on your own machine. You can skip this field.

* `api/test/sso_url` - The test endpoint for single sign-on. Used to test your service on your own machine. You can skip this field.

### Authentication

Your service is expected to authenticate all calls with the add-on id and password found as described in the add-on manifest. A failed authentication should return 401 Not Authorized.

#### Sample with Flask in Python

```python
from functools import wraps
from flask import request, Response


def check_auth(username, password):
    """This function is called to check if a username /
    password combination is valid.
    """
    return password == '44ca82ddf8d4e74d52494ce2895152ee'

def authenticate():
    """Sends a 401 response that enables basic auth"""
    return Response(
    'Could not verify your access level for that URL.\n'
    'You have to login with proper credentials', 401,
    {'WWW-Authenticate': 'Basic realm="Poney required"'})

def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            return authenticate()
        return f(*args, **kwargs)
    return decorated
```


### Provisioning

When a customer installs your add-on, Clever Cloud issues a POST request to your service to provision a ressource for his app.

The request will be the following:

```json
Request: POST {base_url}
Request Body: {
  "heroku_id": "addon_xxx",
  "plan": "basic",
  "region": "eu",
  "callback_url": "https://ccapi.cleverapps.io/vendor/apps/addon_xxx",
  "logplex_token": "logtoken_yyy",
  "options": {}
}
Response Body: {
	"id": "myaddon_id",
	"config": {
		"ADDON_NAME_MY_VAR": "some value"
	},
	"message": "Some provisioning message"
}
```

The request body contains the following fields:

* `heroku_id` - The id we give to your addon to identify it on our side.

* `plan` - The plan name the user chose.

* `region` - The region to provision the addon. As for now, only "eu" will be sent.

* `callback_url` - The URL you can use to get informations about the addon and the user. This URL is available as soon as the provisioning is done. You can't use this URL during the POST call.

* `logplex_token` - Token used to identify what you send to our log aggregator. [Need link to logplex doc]

* `options` - String -> String map with options. We don't currently support this, so it will be empty for now.

The response body contains the following fields:

* `id` - The addon id as seen from your side. The difference with the heroku api is that the id *MUST* be a String.

* `config` (Optional) - A String -> String map with value for each config\_var defined in your manifest. A key that is not in your config\_vars will be ignored.

* `message` (Optional) - A creation message we will display in the dashboard.

### Deprovisioning

When a customer deletes your add-on, Clever Cloud issues a DELETE request to your service to deprovision a ressource for his app.

The request will be the following:

```json
Request: DELETE {base_url}/{addon_id}
Request Body: none
Response Status: 200
```

* `addon_id` - This is the same as the `id` field set in the response to the provisioning call.

### Plan change

When a customer wants to change it's addon's plan, Clever Cloud issues a PUT request to your service.

The request will be the following:

```json
Request: PUT {base_url}/{addon_id}
Request Body: {
	"heroku_id": "addon_xxx",
	"plan": "premium"
}
Response Body: {
	"config": { ... },
	"message": "your message here"
}
```

* `addon_id` - This is the same as the `id` field set in the response to the provisioning call.

The request body contains:

* `heroku_id` - The addon's id as seen from our side.

* `plan` - The name of the new plan.

The response body contains:

* `config` - The value for the new config map. Same constraints as in the provisioning response.

* `message` - A message displayed in our dashboard.

### SSO

Your service probably has a web UI admin panel that your users log into to manage and view their resources.

Clever Cloud customers will be able to access the admin panel for their resource if you implement single sign-on.

Clever Cloud will generate a single sign-on token by combining the salt (a shared secret), timestamp, and resource ID. The user’s browser will be redirected to your site with this token. Your site can confirm the authenticity of the token, then set a cookie for the user session and redirect them to the admin panel for their resource.

When the user clicks your add-on in their add-on menu, they will be directed via HTTP POST to a URL defined in your manifest.

```http
POST <production/sso_url>
id=<id>&token=<token>&timestamp=<timestamp>&nav-data=<nav-data>&email=<email>
```

* The hostname or sso_url comes from your add-on manifest.
* The id is the ID for the previously provisioned resource.
* The timestamp is a millisecond timestamp. You *SHOULD* verify that the timestamp is not older than 15 minutes.
* the token is computed using the formula below.
* Nav data contains information like the current app name and installed add-ons for Clever Cloud Dashboard.

#### Token

The token field in the sso call, is created as follow:

```javascript
sha1sum(id + ':' + sso_salt + ':' + timestamp)
```

Where:

* `id` - The id of the connecting addon. This is the id you returned on
the provision call.

* `sso_salt` - The sso_salt field defined in your manifest.

* `timestamp` - The timestamp field of the sso request.

#### Sample in Python

```python
from hashlib import sha1
import time

id = "1234"
salt = "fcb5b3add85d65e1dddda87a115b429f"
timestamp = str(time.time())
token = sha1(id + ':' + salt + ':' + timestamp).hexdigest()
print token
```

This will return:

```
'aca601ba464437cbaa12b2fedd7db755c32ddb5e'
```

## Addon Infos API

This API is part of the Clever Cloud API. The base URL for the Clever Cloud API is:

```http
https://ccapi.cleverapps.io/v2
```

You should prefix your calls by this base URL.

### Endpoints

```http
GET /vendor/apps
Response Body: [
	{
		"provider_id": "addon-name",
		"heroku_id": "addon_xxx",
		"callback_url": "https://ccapi.cleverapps.io/vendor/apps/addon_xxx",
		"plan": "test"
	}, {
		"provider_id": "addon-name",
		"heroku_id": "addon_yyy",
		"callback_url": "https://ccapi.cleverapps.io/vendor/apps/addon_yyy",
		"plan": "premium"
	}
]
```

List all add-ons provided by you.

* `provider_id` - Should be the same as the "id" field of your uploaded manifest.

* `heroku_id` - The add-on's id from Clever Cloud's POV.

* `callback_url` - URL to call to get more informations about this add-on.

* `plan` - The current plan of this add-on.

Now, you can get more informations about a specific add-on:

```http
GET /vendor/apps/{addonId}
Response Body: {
	"id": "addon_xxx",
	"name": "My addon-name instance",
	"config": {"MYADDON_URL": "http://myaddon.com/52e82f5d73"},
	"callback_url": "https://ccapi.cleverapps.io/vendor/apps/addon_xxx",
	"owner_email": "user@example.com",
	"region": "eu",
	"domains": []
}
```

* `id` - The addon id from Clever Cloud's POV.

* `name` - The name the user gave to this add-on in the Clever Cloud's dashboard.

* `config` - Config vars as you defined during the provision call.

* `callback_url` - The URL you just called.

* `owner_email` - One of the owner's email address.

* `region` - The region this add-on is located in. As for now, we only support "eu".

* `domains` - Originally the domains names for the application owning the add-on. We return an empty list.

```http
PUT /vendor/apps/{addonId}
```
Modify your config vars, you should update your manifest too.

## Sample code

### Add-on Skeleton in Python

```python
#!/usr/bin/env/python
# -*- coding:utf-8 -*-

"""
api.py
"""
from flask import Flask, redirect, Response, jsonify, request
import auth
import provision

app = Flask(__name__)

@app.route('/')
def index():
  """
  Render the home template
  """
  return redirect("https://google.com/")


@app.route('/clevercloud/resources', methods=['POST'])
@auth.requires_auth
def clevercloud_create_resource():
  data = request.json
  msg = provision.add(**data)
  return jsonify(msg)


@app.route('/clevercloud/resources/<string:id>', methods=['DELETE','PUT'])
@auth.requires_auth
def clevercloud_action_resource(id):
  data = request.json
  if request.method == 'POST':
    msg = provision.del(id,**data)

  if request.method == 'PUT':
    msg = provision.update(id,**data)

  return jsonify(msg)

@app.route('/clevercloud/sso/login')
def clevercloud_sso_login():
  return Response(status=200)


if __name__ == "__main__":
  app.run(host='0.0.0.0',port=9000,debug=True)
```

