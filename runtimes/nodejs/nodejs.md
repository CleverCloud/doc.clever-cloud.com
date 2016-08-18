---
title: Deploy Node.js apps
shortdesc: Node.js is a platform built on Chrome's JavaScript runtime for building fast, scalable network applications.
tags:
- nodejs	
---

# Deploy Node.js apps

Clever Cloud allows you to deploy any Node.js application. This page will
explain you how to set up your application to run it on our service.

You do not need to change a lot in your application, the *requirements* will help you to configure your apps with some
mandatory files to add, and properties to setup.

## Overview

Node.js is a platform built on Chrome's JavaScript runtime for building fast, scalable network applications. Node.js uses an event-driven, non-blocking I/O model that makes it lightweight and efficient, pretty good for data-intensive real-time applications that run across distributed devices.

## Create an application

Refer to the page [Deploy an application on Clever Cloud](/doc/clever-cloud-overview/add-application/).

## Necessary information

Be sure that:

* you have pushed in <b>master branch</b>
* you listen on <b>port 8080</b>
* you have added application <b>name</b> in package.json
* you have added application <b>version</b> in package.json
* you have added application <b>start script</b> in package.json

## Requirements

First, your application must be set to listen on the 8080 port, for worldwide
connections. The following code describes a Hello world application listening on
that port:

```javascript
// Load the http module to create an http server.
var http = require('http');

// Configure our HTTP server to respond with Hello World to all requests.
var server = http.createServer(function (request, response) {
  response.writeHead(200, {"Content-Type": "text/plain"});
  response.end("Hello World\n");
});

// Last, but not least, listen on port 8080
server.listen(8080);
```
Then, a *package.json* file is mandatory to initiate your app deployment on Clever Cloud. The next section will detail this point.

### Describing package.json  

Even if you have no dependencies, you have to provide a `package.json` file at the root of your project’s directory.

<div class="alert">
  <h5>About dependencies:</h5>
  <ul>
  <li>For every Node.js project you **HAVE TO** provide a package.json file at the root of your project’s
directory.</li>
  <li>Additionally, make sure that the folder "/node_modules" is in your .gitignore file before pushing your app.</li>
  </ul>
</div>  

If you already are a Node.js guru, you probably won’t have to change anything to that
file. Just check the required fields below.

The `package.json` file should look like the following:

```javascript
{
  "name" : "myapp",
  "version" : "0.1.0",
  "main" : "myapp.js",
  "scripts" : {
    "start" : "node myapp.js"
  },
  "engines" : {
    "node" : "~4.0"
  }
}
```

### The json fields

The following table describes each of the fields formerly mentioned.

<table id="nodedeps" class="table table-bordered table-striped">
<thead>
<tr>
<th>Usage</th>
<th>Field</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><span class="label label-important">Required</span></td>
<td>name</td>
<td>Name of your application. You need to fill this field.</td>
</tr>
<tr>
<td><span class="label label-important">Required</span></td>
<td>version</td>
<td>Version of you application. You need to fill this field.</td>
</tr>
<tr>
<td class="cc-depusage" rowspan="2"><span class="label label-important">At least one</span></td>
<td>main</td>
<td>This field allows you to specify the file you want to run. It should
be the relative path of the file starting at the project's root. It's used prior to the next one.</td>
</tr>
<tr>
<td>scripts.start</td>
<td>This field provides a command line to run. It is required if the <code>main</code> one is missing. You need one
of <code>scripts.start</code> and <code>main</code>. If both exist, we use the <code>main</code> one.</td>
</tr>
<tr>
<td class="cc-depusage" ><span class="label label-inverse">Optional</span></td>
<td>engines.node</td>
<td>Sets the node engine version you app runs with. Any "A.B.x" or "^A.B.C" or "~A.B" version will lead
to run the application with le latest "A.B" local version. If this field is
missing, we use the greatest default local version (0.10 until 2015-10-11). If you want to use <a
href="http://iojs.org/">iojs</a>, put either "iojs", "iojs-v1.0.3", or any truncated
version number (e.g. "iojs-1" will work).<br />If you want to ensure that your app will always run, please put something of the form "^A.B.C" and avoid setting only ">=A.B.C".</td>
</tr>
</tbody>
</table>

### NPM modules dependencies

If you need some modules you can easily add some with the *dependencies* field.

Here is an example :

```javascript
{
  "name" : "myapp",
  "version" : "0.1.0",
  "main" : "myapp.js",
  "scripts" : {
    "start" : "node myapp.js"
  },
  "engines": {
    "node": "0.8.15"
 },
"dependencies": {
  "express": "3.x",
  "socket.io": "0.9.x",
  "underscore": "1.4.3"
  }
}
```

#### Deploy with privates dependencies

If your application got privates dependencies, you can add a [Private SSH Key](https://www.clever-cloud.com/doc/clever-cloud-overview/common-application-configuration/#private\
-ssh-key).


## Node.js supported versions

The Clever Cloud can virtually run any version of node >= 0.6 and any
module. Lesser (pre-npm) versions are not officially supported. Unstable
versions are not supported either. We support the io.js runtime.

### Pre-installed modules

We currently host the following Node.js versions, with these modules already installed:

**You can use any version of node you need and any modules you need. Preinstall version is only for faster deployment process.**

<div class="row">
<div class="span4">
<table class="table table-bordered table-striped span1">
<thead>
<tr>
<th>Pre-installed modules</th>
</tr>
</thead>
<tbody>
<tr><td>socket.io</td></tr>
<tr><td>express  </td></tr>
<tr><td>async    </td></tr>
<tr><td>mysql    </td></tr>
<tr><td>pg       </td></tr>
</tbody>
</table>
</div>
</div>


New versions will be added as they are released.

### Defining *pre-installed*

The above table describes the modules pre-installed.
These modules are available at deploy time, without the need to download and
install them.

If you use modules that are not pre-installed, we will just get them with npm
(provided they are in the npm repository), and install it before we start your
application. The deploy will then be a little longer, due to probable
compilation of some of these modules.

Some modules require system dependencies to build, require to rebuild node… so
we installed the most used ones.

If you think more modules are commonly used and should be pre-installed, do not
hesitate to contact us at <mailto:support@clever-cloud.com>

## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To access your variable in your application, nothing simpler! Just get
it from your environment, like you would with `PATH`:
`process.env["MY_VARIABLE"]`.

You can, for example, inject the NODE_ENV variable to define it at "production". It is not
currently set on our machines, because we need to build the project first. Beware,
devDependencies do not get fetched if NODE_ENV=production.

### Special env NPM_TOKEN

Since April 2015, npmjs.com allows you to have private repositories. The bad news is,
you have to write a specific line in your `.npmrc` for that. The good news is, on Clever
Cloud, you only need to provide the *token* part, and we set up everything for you!

So to register your npm auth token, you need to add to your application the `NPM_TOKEN`
environment variable.

``` javascript
Example:
NPM_TOKEN=00000000-0000-0000-0000-000000000000
```

As you can see, only the `authToken` value is needed, not the full url you can find in
your .npmrc.

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/doc/clever-cloud-overview/add-application/) to deploy your application.

## Deployment video

More info on <a target="_blank" href="http://nodejs.org/">nodejs.org</a>.