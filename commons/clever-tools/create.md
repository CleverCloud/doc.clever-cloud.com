---
title: Clever Cloud CLI create
position: 1
shortdesc: Create a new application using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Create

`clever create` allows you to create a new application.

## Know your application's type

First of all, select the type of your application in the list below :

* docker (Docker)
* go (Go)
* gradle (Java or Groovy + Gradle)
* jar (Java + JAR)
* maven (Java + Maven)
* node (Node)
* php (PHP)
* play1 (Java + Play! 1)
* play2 (Java or Scala + Play! 2)
* python (Python)
* ruby (Ruby)
* sbt (Scala + SBT)
* static (Static)
* war (Java + WAR)

## Create your application

In order to create your app in your personal space, use :
    `clever create --type TYPE APP-NAME`

### Organisation

If you want to create an application for one of your organisation, just add the flag `--org` followed by the organisation ID (or the name, if unambiguous).

### Alias

You might want to use an alias instead of the complete name. Add the flag `--alias ALIAS` when you create your application and use `--alias ALIAS` when you use others commands to make reference to this application.

### Region

Choose your region by adding `--region ZONE` where ZONE can be 'par' for Paris or 'mtl' for Montreal. The default zone is Paris.

### Github