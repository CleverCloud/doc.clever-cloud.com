---
title: Play Framework 1
position: 1
shortdesc: Play is an open source web application framework, written in Scala and Java, which follows the model–view–controller (MVC) architectural pattern.
---

# Deploy Play Framework 1.x

The Clever Cloud supports Play 1.x applications natively. The present guide explains how to set up your application to run on the Clever Cloud.  

## Overview

Play is an open source web application framework, written in Scala and Java, which follows the model–view–controller (MVC) architectural pattern. It aims to optimize developer productivity by using convention over configuration, hot code reloading and display of errors in the browser.

## Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/scalaplay1/scalaplay1_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
4. Check that the application information is correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/scalaplay1/scalaplay1_validation.png"/></figure>
5. *Optional*: <a href="/addons/add-an-addon/">add an add-on</a>

## Necessary information

* the application must be located at the **root** of the git repository

## Play! configuration

By default, your application will run on Clever Cloud with the option `--%clevercloud`.  
It means that you can define special keys in your `application.conf` file that will be used only on Clever Cloud.

You can for example:

* set production mode so the files are compiled at startup time and the errors are logged in a file:

    ```bash
    %clevercloud.application.mode=prod
    ```

* set up a mysql database

    ```bash
    %clevercloud.db.url=jdbc:mysql://{yourcleverdbhost}/{dbname}
    %clevercloud.db.driver=com.mysql.jdbc.Driver
    %clevercloud.db.user={yourcleveruser}
    %clevercloud.db.pass={yourcleverpass}
    ```


More information on <a target="_blank" href="http://www.playframework.com">playframework.com</a>.

## Configuration file

You can configure your application start command by adding a `./clevercloud/play.json` file with the following fields:

```javascript
{
  "hooks": {
     "postDeploy": "pathtoyourscript"
  }
}
```

**postDeploy**: execute a custom script after the deploy. Some frameworks or custom applications might require bootstrapping before the application may run.
You can achieve this by creating a custom script with your commands and adding the associated file name in `clevercloud/play.json`.



## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.

