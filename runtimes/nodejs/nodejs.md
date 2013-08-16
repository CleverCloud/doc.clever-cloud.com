---
title: Deploying Node.js apps
---

## Deploying Node.js apps

Clever Cloud allows you to deploy any Node.js application. This page will
explain you how to set up your application to run it on our service.

You do not need to change a lot in your application, the *requierments* will help you to configure your apps with some mandatory files to add, and properties to setup.

### Overview

Node.js is a platform built on Chrome's JavaScript runtime for building fast, scalable network applications. Node.js uses an event-driven, non-blocking I/O model that makes it lightweight and efficient, pretty good for data-intensive real-time applications that run across distributed devices.


### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/appjavawar.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
3. *Optional:* in case of PHP or static applications, you can choose between FTP and Git deployment
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/appcreationreviewjavawar.png"></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

Be sure that:
* you have pushed in <b>master branch</b>
* you listen on <p>port 8080</p>
* you have added application <b>name</b> in package.json
* you have added application <b>version</b> in package.json
* you have added application <b>start script</b> in package.json

### Requirements

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
Then, a *package.json* file is mandatory to initiate your app deployement on Clever Cloud. The next section will detail this point.

#### Describing package.json  

Even if you have no dependencies, you have to provide a `package.json` file at the root of your project’s directory.

<div class="alert">
  <h5>About depedencies:</h5>
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
    "node" : ">=0.6"
  }
}
```

#### The required fields

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
			<td>Sets the node engine version you app runs with. Any ">=" version will lead to
			run the application with the latest local version. Any "A.B.x" version will lead
			to run the application with le latest "A.B" local version. If this field is
			missing, we use the greatest local version.</td>
		</tr>
	</tbody>
</table>

#### NPM modules dependencies

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
### Node.js supported versions

The Clever Cloud can virtually run any version of node >= 0.6 and any module. Lesser (pre-npm) versions are not officially supported.

#### Pre-installed versions and modules

We currently host the following Node.js versions, with these modules already installed:

**You can use any version of node you need and any modules you need. Preinstall version is only for faster deployment process.**

<div class="row">
  <div class="span3">
    <table class="table table-bordered table-striped">
    	<thead>
    		<tr>
    			<th>Available Node Versions</th>
    		</tr>
    	</thead>
    	<tbody>
    		<tr><td>v0.10.3 </td></tr>
    		<tr><td>v0.10.2 </td></tr>
    		<tr><td>v0.10.1 </td></tr>
    		<tr><td>v0.10.0 </td></tr>
    		<tr><td>v0.9.3  </td></tr>
    		<tr><td>v0.9.2  </td></tr>
    		<tr><td>v0.9.1  </td></tr>
    		<tr><td>v0.9.0  </td></tr>
    		<tr><td>v0.8.9  </td></tr>
    		<tr><td>v0.8.8  </td></tr>
    		<tr><td>v0.8.16 </td></tr>
    		<tr><td>v0.8.15 </td></tr>
    		<tr><td>v0.8.14 </td></tr>
    		<tr><td>v0.8.13 </td></tr>
    		<tr><td>v0.8.12 </td></tr>
    		<tr><td>v0.8.11 </td></tr>
    		<tr><td>v0.8.10 </td></tr>
    		<tr><td>v0.7.12 </td></tr>
    		<tr><td>v0.6.21 </td></tr>
    	</tbody>
    </table>
  </div>
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


New versions will be added as they go out.

#### Defining *pre-installed*

The above table describes the versions and modules pre-installed for these versions.
These modules are available at deploy time, without the need to download and
install them.

If you use modules that are not pre-installed, we will just get them with npm
(provided they are in the npm repository), and install it before we start your
application. The deploy will then be a little longer, due to probable
compilation of some of these modules.

If you think more modules are commonly used and should be pre-installed, do not
hesitate to contact us at <mailto:support@clever-cloud.com>

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.

### Tutorial - Node.js application deployment

<p>
  <iframe width="560" height="315" src="http://www.youtube.com/embed/pTCcminQwaI" frameborder="0" allowfullscreen></iframe>
</p>

More info on <a target="_blank" http://nodejs.org/">nodejs.org</a>.