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

1. Create a new app by clicking on the **Add an application** button, in the headbar.
2. Select the language/framework: <figure class="cc-content-img"><img src="/assets/images/select-lang.png"/></figure>
3. Select the scalability options: <figure class="cc-content-img"><img src="/assets/images/select-scalab.png"/></figure>
4. Enter your application's name and description, choose your deployment zone and click "Create".
<figure class="cc-content-img"><img src="/assets/images/choose-name.png"/></figure>
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


## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To access the environment variables from your application, you need to
reference them in your application.conf file:
you just have to put `my.option=${MY_VARIABLE}` in your application.conf file, and then use
the configuration item `my.option` in your application.

So for an application using the MySQL add-on, you can set:

```bash
%clevercloud.db.url="jdbc:mysql://"${MYSQL_ADDON_HOST}"/"${MYSQL_ADDON_DB}
%clevercloud.db.driver=com.mysql.jdbc.Driver
%clevercloud.db.user=${MYSQL_ADDON_USER}
%clevercloud.db.pass=${MYSQL_ADDON_PASSWORD}
```

## HTTPS support

HTTPS is handled by Clever Cloud ahead of your application, your application
retrieves the traffic in plain http. To be able to use `request.secure`, you
have to add `XForwardedSupport=all` in `application.conf`.

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.

