---
layout: page

id: cc_conf
parent: app_configuration
next: java_and_scala
---
Deployment configuration
============

For every java or scala project using Maven, Ant or SBT

This json is the configuration file that you will need for some deployment/build. Here is it's syntax:

    {
      "jarName": "<string>",
      "build": "<string>",
      "goal": "<string>"
    }

* "jarName" is a string containing the name of the main jar used to launch your application.
* "build" is a string with the builder you want to use. We currently support Maven, SBT and Ant.
* "goal" is a string with a list of the parameters and/or the goals that Maven/Ant have to execute. It is mandatory if you upload a Maven or Ant-managed application. 
