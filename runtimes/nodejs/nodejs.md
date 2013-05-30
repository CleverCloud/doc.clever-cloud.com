---
title: Node.js apps
---

## Deploying Node.js apps

The Clever Cloud allows you to deploy any Node.js application. This page will
explain you how to set up your app to run it on our service.

You do not need to change a lot in your application, the *requierments* will help you to configure your apps with some mandatory files to add, and properties to setup.

### Overview

Node.js is a platform built on Chrome's JavaScript runtime for building fast, scalable network applications. Node.js uses an event-driven, non-blocking I/O model that makes it lightweight and efficient, pretty good for data-intensive real-time applications that run across distributed devices.


### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavamaven.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Node.js":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavamaven.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>

### Requirements

First, your application must be set to listen on the 8080 port, for worldwide
connections. The following code describe a Hello world application listening on
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

  

For every Node.js application you want to deploy on the Clever Cloud, you **HAVE TO**
provide a `package.json` file at the root of your project’s directory, even if your app has no dependencies.

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

### Git Deployment

*You will need git on your computer to deploy via this tool. Here is the official website of Git to get more informations&nbsp;: <a href="http://git-scm.com">git-scm.com</a>*

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following message:

<figure class="cc-content-imglarge">
  <img src="/assets/images/newgitapp.png"/></a>
</figure>

#### Setting up your remotes

1. The "Information" page of your app gives you your git deployment URL.  
It looks like this:  ``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``.  
Copy it in your clipboard.
2. On your computer, go into your application repository. 
If you didn't already track your app with git, start by typing:

    ```bash
    $ git init
    ```
3. Then, use the "git remote" command to add the deploy URL:

    ```bash
    $ git remote add <name> <your-git-deployment-url>
    ```

4. The last step is to push your application:

    ```bash
    $ git push <name> master
    ```

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>The remote branch on Clever Cloud is <strong>ALWAYS</strong> master. If your local branch is not "master", use this syntax:</p>
  <pre>git push < name > yourbranch:master</pre>

</div>