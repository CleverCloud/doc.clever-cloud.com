---
title: Scala with SBT
---

## Deploying Scala apps

The Clever Cloud allows you to deploy Scala and Java applications built with <acronym title=" Simple Build Tool">SBT</acronym>.  
This document will explain you how to set up your app to run on our service.


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

### Requirements

First, your application must be set to listen on the 8080 port, for worldwide
connections.

And for now, you need to add the sbt-start-script plugin to your project.

That plugin will add the "stage" goal that we will use to create a target/start
script we will execute.

In project/plugins.sbt:

```scala
addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.7.0")
```

You can also use newer (or older) versions of the plugin.

Prepend to build.sbt:

```scala
import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)
```

That should be enough for a project with a main method.

For more configuration, please go to <a href="https://github.com/sbt/sbt-start-script" target="_blank">https://github.com/sbt/sbt-start-script</a>.

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
