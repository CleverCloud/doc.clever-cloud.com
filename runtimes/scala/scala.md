---
title: Scala with SBT
shortdesc: Scala is an object-functional programming and scripting language that runs on the Java platform…
tags:
- scala
---

# Deploy Scala apps

The Clever Cloud allows you to deploy Scala and Java applications built with <acronym title=" Simple Build Tool">SBT</acronym>.  
This document will explain you how to set up your app to run on our service.

## Overview

Scala is an object-functional programming and scripting language that runs on the Java platform for general software
applications, statically typed, designed to concisely express solutions in an elegant, type-safe and lightweight manner.
Scala includes full support for functional programming (including currying, pattern matching, algebraic data types, lazy
evaluation, tail recursion, immutability, etc.).

## Create an application

Refer to the page [Deploy an application on Clever Cloud](/clever-cloud-overview/add-application/).

## Requirements

First, your application must be set to listen on the 8080 port, for worldwide
connections.

We currently support both sbt-native-packager and sbt-start-script. So
you need to add one of them to your project. (Please use sbt-native-packager if you can,
sbt-start-script has some bugs involving environment variables)

These plugins will add the "stage" goal that we will use to create a
start script we will execute.

In project/plugins.sbt:

```scala
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.8.0")
```

or

```scala
addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")
// Or 0.9.0 if you use sbt ≤0.12
```


You can also use newer (or older) versions of the plugin.

Prepend to build.sbt:

```scala
packageArchetype.java_application
```

or

```scala
import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)
```

That should be enough for a project with a main method.

Please note that the sbt-start-script plugin is going toward deprecation. Also, please
note that sbt-start-script fails to handle environment variables in a correct way.

For more configuration, please go to <a href="https://github.com/sbt/sbt-start-script" target="_blank">https://github.com/sbt/sbt-start-script</a>.

## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To get an env variable from your code, you can use the `System.getenv("MY_VARIABLE")` method. Be aware that it can return null.

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
