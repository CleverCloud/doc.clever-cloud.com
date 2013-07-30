---
title: Play Framework 1
position: 1
---

## Deploying Play Framework 1.x

The Clever Cloud supports Play 1.x applications natively. The present guide explains how to set up your application to run on the Clever Cloud.  

Play! is a framework created by Guillaume Bort. It allows you to quickly create ready-to-use web applications with Java or Scala (only Play! 2).  

The following information are dedicated to Play! 1 applications:


### Configuration

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