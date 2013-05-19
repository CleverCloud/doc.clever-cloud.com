---
title: Play Framework 2
position: 2
---

## Deploying Play Framework 2 Java

The Clever Cloud supports Play 2 applications natively. The present guide explains how to set up your application to run on the Clever Cloud.  
This guide will show you how to deploy applications for Play Framework 2 only.

### Overview
Play! is a framework created by Guillaume Bort. It allows you to quickly create ready-to-use web application with Java or Scala. There are currently two major versions of this framework: 1.2 and 2. They are really different from each other. This guide will show you how to deploy applications for both versions of the Play! Framework.

More infos on<a target="_blank" href="http://www.playframework.org">Play Framework.com</a>. 

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavaplay.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Java + Play! 2" or "Scala + Play!2":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavaplay.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>



### Configure your application
The code of your application and the `clevercloud` folder containing the `play.json` file (if you use one) must be placed at the root of your git repository.

To configure your Play! 2 application, you might need a file named `clevercloud/play.json` that is a `play.json` file placed in a `clevercloud` folder at the root of your application.

<div class="alert alert-hot-problems">
	<h4>Please note:</h4>
	<p>That file is optional and is used to set more
configuration elements to the start command.</p>
</div>

The file must contain the
following fields:

```javascript
{
   "deploy":{
	   "goal":<string>
	}
}
```

**goal** : That field should contain additional configuration like
`"-Dconfig.resource=clevercloud.conf"`.

<div class="alert alert-hot-problems">
	<h4>Tip:</h4>
	<p>do not forget the double quotes
	around the "goal"’s value.</p>
</div>

### Known problems with Play! 2

Please read the following if your project fails with this error:  

	sbt.ResolveException: unresolved dependency: play#sbt-plugin;2.0: not found

Some versions of Play2 try to retrieve a nonexistent version of
"sbt-plugin" which is required by the framework to work.
You have two options to fix this problem:

You can set the "play.version" environment variable in the
`clevercloud/play.json` file.  
For example, for Play 2.0.4:

``` javascript
{
	"deploy": {
		"goal": "-Dplay.version=2.0.4"
	}
}
```

Otherwise, you can modify plugins.sbt in the project folder of your
app like the following:

``` scala
// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.4") // The important part of the configuration
```

The two solutions do the job, you can pick your favorite.

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
