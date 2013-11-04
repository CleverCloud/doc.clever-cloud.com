---
title: Deploy Go apps
shortdesc: Go, otherwise known as Golang, is an open source, compiled, garbage-collected, concurrent system programming language.
---

## Deploy Go applications

Clever Cloud allows you to deploy any Go application. This page will
explain you how to set up your application to run it on our service.

You do not need to change a lot in your application, the *requirements* will help you configure your applications with some mandatory files to add, and properties to setup.

### Overview

Go, otherwise known as Golang, is an open source, compiled, garbage-collected, concurrent system programming language. It was first designed and developed at Google Inc. beginning in September 2007 by Robert Griesemer, Rob Pike, and Ken Thompson.

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/go/go_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/go/go_validation.png"/></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

Be sure that:

* you push in <strong>master branch</strong>

* your application listens on port <strong>8080</strong>

### Requirements

Appart from <strong>listening on port 8080</strong>, there is nothing to
change on your application.

We automatically build and run your application, as `go get myapp` would
build it. The executable is run with the application's root as its
current path. So if your application has a "static" folder at its root, it will be
accessible via "./static" in your application.

We currently support single module applications only. That means that
your sources files just need to be at the project's root, and you can't
have multiple modules running.

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
