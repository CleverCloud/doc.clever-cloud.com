---
title: Intro to Apps
position: 1
---

## Introduction to Applications

### Application's definition

An application is defined on Clever Cloud by the following elements:

* a dedicated language/framework
* a deployment method (FTP and/or Git)
* RAM and CPU consumption, depending on the language or framework used
* an optional configuration file you've added to your project

If one of these elements is missing, Clever Cloud can't deploy your application properly (except the configuration file, optional in some cases).

Available languages are listed below: 

* [Go](/go)
* [Java](/java) [Play Framework 1 & 2, Maven, War files… ]
* [Node.js](/nodejs)
* [PHP](/php)
* [Python](/python)
* [Ruby](/ruby)
* [Scala](/scala)


### How it works

When an application is pushed, the platform receives it. It then checks the resources’ requirements. If they are complete, the deployment is launched. When finished and successfull, the application is up and running.

The log system retrieves every traces from the application and display theme in your admin console.
