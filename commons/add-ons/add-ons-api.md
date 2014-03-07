---
title: Add-on API
position: 1
---
## Clever Cloud add-on API

Clever Cloud provides an API to add-on providers. This document is a handbook for this API.

### How it works

When a Clever Cloud customer use the marketplace to provision an add-on, Clever Cloud sends a request to your service which provides a new add-on for this app.

### Build an add-on

You can write your service in any language. 

You need to create a manifest file, this file is a JSON that describes your add-on.

##### Example manifest

```json
{
  "id": "addonname",
  "api": {
    "config_vars": [ "MY_VAR" ],
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

* id - An id for your add-on. All lower case, no spaces or punctuation. This can’t be changed after the first push. It is also used for HTTP basic auth when making provisioning calls.

* api/config_vars - A list of config vars that will be returned on provisioning calls.

* api/password - Password that Clever Cloud will send in HTTP basic auth when making provisioning calls.

* api/sso_salt - Shared secret used in single sign-on between the Clever Cloud admin panel and your service’s admin panel.

* api/regions - The list of geographical zones supported by your add-on. It cannot be empty. It must either contain the elements "eu"

* api/production/base_url - The production endpoint for Clever Cloud api actions (provision, deprovision, and plan change). 

* api/production/sso_url - The production endpoint for single sign-on

* api/test - The root URL of your development host, typically local, or a map of URLs.

* api/test/base_url - The test endpoint for Clever Cloud api actions

* api/test/sso_url - The test endpoint for single sign-on

### Authenfication

Your service is expected to authenticate all calls with the add-on id and password found as described in the add-on manifest. A failed authentication should return 401 Not Authorized.

##### Sample with Flask in Python

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


#### Provisioning

When a customer installs your add-on, Clever Cloud makes a REST call to your service to provision a ressource for his app.

#### Deprovisioning

When a customer deletes your add-on, Clever Cloud makes a REST call to your service to deprovision a ressource for his app.

#### SSO

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
* The timestamp is a timestamp ;), 
* the token is computed using the formula above. 
* Nav data contains information like the current app name and installed add-ons for Clever Cloud Dashboard.

##### Token

You should use data as described in manifest

###### Sample in Python

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

### API Documentation

#### Endpoints

##### Resources

```http
POST /clevercloud/resources 
```

Provision a new resource on your add-on.

```http
PUT /clevercloud/resources/{id}
```

Update a resource on your add-on.

```http
DELETE /clevercloud/resources/{id}
```

Deprovision a resource on your add-on.
##### SSO



##### Vendor

```http
GET /vendor/apps
```
List all add-ons

```http
GET /vendor/apps/{addonId}
```
Get information for one specific add-on.

```http
PUT /vendor/apps/{addonId}
```
Modify your config vars, you should update your manifest too.

### Sample code

##### Add-on Skeleton in Python

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
 