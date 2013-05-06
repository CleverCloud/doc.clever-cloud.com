---
layout: page
title: Deploying scala apps
---

## Deploying Scala apps

The Clever Cloud allows you to deploy Scala and Java applications built with <acronym title=" Simple Build Tool">SBT</acronym>.  
This document will explain you how to set up your app to run on our service.

### Scala overview

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavamaven.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Java or Scala + SBT":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavamaven.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>

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