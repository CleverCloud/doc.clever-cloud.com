---
title: Deploy Go apps
shortdesc: Go, otherwise known as Golang, is an open source, compiled, garbage-collected, concurrent system programming language.
---

# Deploy Go applications

Clever Cloud allows you to deploy any Go application. This page will
explain you how to set up your application to run it on our service.

You do not need to change a lot in your application, the *requirements* will help you configure your applications with some mandatory files to add, and properties to setup.

## Overview

Go, otherwise known as Golang, is an open source, compiled, garbage-collected, concurrent system programming language. It was first designed and developed at Google Inc. beginning in September 2007 by Robert Griesemer, Rob Pike, and Ken Thompson.

## Create an application

1. Create a new app by clicking on the **Add an application** button, in the headbar.
2. Select the language/framework: <figure class="cc-content-img"><img src="/assets/images/select-lang.png"/></figure>
3. Select the scalability options: <figure class="cc-content-img"><img src="/assets/images/select-scalab.png"/></figure>
4. Enter your application's name and description, choose your deployment zone and click "Create".
<figure class="cc-content-img"><img src="/assets/images/choose-name.png"/></figure>
5. *Optional*: <a href="/addons/add-an-addon/">add an add-on</a>

## Necessary information

Be sure that:

* you push in <strong>master branch</strong>

* your application listens to the wild network <strong>0.0.0.0</strong>,
  not only localhost or 127.0.0.1

* your application listens on port <strong>8080</strong>

## Requirements

Apart from <strong>listening on port 8080</strong>, there is nothing to
change on your application.

We automatically build and run your application, as `go get myapp` would
build it. The executable is run with the application's root as its
current path. So if your application has a "static" folder at its root, it will be
accessible via "./static" in your application.

We currently support single module applications only. That means that
your sources files just need to be at the project's root, and you can't
have multiple modules running.

## More configuration

By default, we consider that your repository contains a single
application. We put your application in ${GOPATH}/src/{app_id} and then
run `go get {app_id}`.

If you want to configure more precisely your dependencies (e.g. have
private libraries, or specific versions of some libraries), the
following will explain you how to do it:

1. Make your repository have a GOPATH structure:

	```
	./
	   src/
	      myapp/
	      foo/
	         module1/
	         module2/
	      module3/
	```
<br />

	Here you have the modules `myapp`, `foo/module1`, `foo/module2` and `module3`.

2. Create a *clevercloud/go.json* file at the top of your repository:

	```
	./
	   clevercloud/
	      go.json
	   src/
	      myapp/
	      ...
	```


3. In the go.json file, put the following:

	```javascript
	{
	    "deploy": {
	        "appIsGoPath": true,
	        "main": "myapp"
	    }
	}
	```
<br />

	If `appIsGoPath` is present and equals `true`, then we consider that
	your repo root is the *GOPATH*. the `main` field then becomes mandatory
	and must be the name of the module you want to run. e.g. if you want
	to run `module1`, `main` must be `foo/module1`.

4. (Optional) Add a "execDir" field to the "deploy" object:

	```javascript
	{
	    "deploy": {
	        "appIsGoPath": true,
	        "main": "myapp"
	        "execDir": "src/myapp"
	    }
	}
	```
<br />


	The `execDir` value must be relative to the root of your repo. In the
	example above, we will run the application in the src/myapp directory.

## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To access your variable in your application, nothing simpler! Just get
it from your environment, like you would with `PATH`:
`os.Getenv("MY_VARIABLE")`.

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
