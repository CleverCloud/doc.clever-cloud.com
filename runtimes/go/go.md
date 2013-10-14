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
  <img src="/assets/images/appjavawar.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/appcreationreviewjavawar.png"></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

Be sure that:

* you push in <b>master branch</b>

* you commit a <strong>Makefile</strong>

* you have a <strong>clevercloud/go.json</strong> file containing the informations listed in <a href="#the-go.json-file">This section</a>.

### Requirements

#### The Makefile

This file should contain the actions needed to build the application. It is a standard Makefile. You might probably already have one.
More documentation about make can be found here: <a href="https://www.gnu.org/software/make/">https://www.gnu.org/software/make/</a>.

#### The go.json file

You **HAVE TO** provide a clevercloud/go.json file. (That is a go.json file in the clevercloud folder at the root of your application)
This file should look like the following:

```javascript
{
	"build": {
		"makefile": "somefile",
		"target": "sometarget"
	},
	"deploy": {
		"exec": "path-to-executable"
	}
}
```

##### Required fields

<table id="godeps" class="table table-bordered table-striped">
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
		<td>deploy.exec</td>
		<td>This must be the path (relatively to the application root) of the generated executable file. This field is required.</td>
		</tr>
		<tr>
		<td><span class="label label-inverse">Optional</span></td>
		<td>build.makefile</td>
		<td>This is the path (relatively to the application root) of the makefile to use. If none given, the file named `Makefile` will be used.</td>
		</tr>
		<tr>
		<td><span class="label label-inverse">Optional</span></td>
		<td>build.target</td>
		<td>This is the target to use when calling make. If none given, the make command will use the first target of the file. (Standard make behaviour.)</td>
		</tr>
	</tbody>
</table>



### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
