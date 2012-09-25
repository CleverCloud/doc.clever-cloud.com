---
layout: page

id: cc_conf
parent: app_configuration
next: java_and_scala
---
Deployment configuration
============

For every java or scala project using Maven, Ant or SBT, you need to write a small specific JSON file, copied at the root of your project.

This JSON is the configuration file that you will need for some deployments and builds. Here is the syntax:

    {
      "jarName": "<string>",
      "build": "<string>",
      "goal": "<string>"
    }


* "jarName" is a string containing the name of the main jar used to launch your application.
* "build" is a string with the builder you want to use. We currently support Maven, SBT and Ant.
* "goal" is a string with a list of the parameters and/or the goals that Maven/Ant have to execute. If you build via Maven and don't fill this field, the "package" command will be applied.
