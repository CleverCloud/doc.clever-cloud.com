---
title: Scala with SBT
shortdesc: Scala is an object-functional programming and scripting language that runs on the Java platform…
---

## Deploy Scala apps

The Clever Cloud allows you to deploy Scala and Java applications built with <acronym title=" Simple Build Tool">SBT</acronym>.  
This document will explain you how to set up your app to run on our service.

### Overview

Scala is an object-functional programming and scripting language that runs on the Java platform for general software applications, statically typed, designed to concisely express solutions in an elegant, type-safe and lightweight manner. Scala includes full support for functional programming (including currying, pattern matching, algebraic data types, lazy evaluation, tail recursion, immutability, etc.).

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/scalasbt/scalasbt_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
3. *Optional:* in case of PHP applications, you can choose between FTP and Git deployment
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/scalasbt/scalasbt_validation.png"/></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Requirements

First, your application must be set to listen on the 8080 port, for worldwide
connections.

We currently support both sbt-start-script and sbt-native-packager. So
you need to add one of them to your project.

These plugins will add the "stage" goal that we will use to create a
start script we will execute.

In project/plugins.sbt:

```scala
addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")
// Or 0.9.0 if you use sbt ≤0.12
```

or

```scala
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.6.4")
```

You can also use newer (or older) versions of the plugin.

Prepend to build.sbt:

```scala
import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)
```

or

```scala
packageArchetype.java_application
```

That should be enough for a project with a main method.

Please note that the sbt-start-script plugin is going toward deprecation.

For more configuration, please go to <a href="https://github.com/sbt/sbt-start-script" target="_blank">https://github.com/sbt/sbt-start-script</a>.

### Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To get an env variable from your code, you can use the `System.getenv("MY_VARIABLE")` method. Be aware that it can return null.

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
