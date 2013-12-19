---
title: Play Framework 2
position: 2
shortdesc: Play is an open source web application framework, written in Scala and Java, which follows the model–view–controller (MVC) architectural pattern.
---

## Deploy Play Framework 2 Java

Clever Cloud supports Play! 2 applications natively. The following guide explains how to set up your application to run the Clever Cloud.

### Overview

Play is an open source web application framework, written in Scala and Java, which follows the model–view–controller (MVC) architectural pattern. It aims to optimize developer productivity by using convention over configuration, hot code reloading and display of errors in the browser.

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar.
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/javaplay2/javaplay2_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/javaplay2/javaplay2_validation.png"/></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

* the application must be located at the **root** of the git repository

### Configuration file

You can configure your application start command by adding a
`./clevercloud/sbt.json` file with the following fields:

```javascript
{
  "deploy":{
    "goal": "yourgoal"
  },
  "hooks": {
     "postDeploy": "pathtoyourscript"
  }
}
```

**goal**: can for example contain additional configuration like
`"-Dconfig.resource=clevercloud.conf"` or `"-Dplay.version=2.0.4"`.


**postDeploy**: execute a custom script after the deploy. Some frameworks or custom applications might require bootstrapping before the application may run.
You can achieve this by creating a custom script with your commands and
adding the associated file name in `clevercloud/sbt.json`.



### Known problems with Play! 2

Please read the following if your project fails with this error:

`sbt.ResolveException: unresolved dependency: play#sbt-plugin;2.0: not found`

Some versions of Play2 try to retrieve a nonexistent version of
"sbt-plugin" which is required by the framework to work.
You have two options to fix this problem:

You can set the "play.version" environment variable in the
`clevercloud/sbt.json` file.
For example, for Play 2.0.4:

``` javascript
{
  "deploy": {
    "goal": "-Dplay.version=2.0.4"
  }
}
```

Otherwise, you can modify `plugins.sbt` in the project folder of your
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

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.


### Tutorial - Play! application deployment

<p>
  <iframe width="640"; height="360" src="http://www.youtube.com/embed/HL366BhWFMw" frameborder="0" allowfullscreen></iframe>
</p>


More info on <a target="_blank" href="http://www.playframework.com">playframework.com</a>.
