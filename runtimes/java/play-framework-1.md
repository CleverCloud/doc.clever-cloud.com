---
title: Play Framework 1
position: 1
---

## Deploying Play Framework 1.x

The Clever Cloud supports Play 1.x applications natively. The present guide explains how to set up your application to run on the Clever Cloud.  

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/appjavawar.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
3. *Optional:* in case of PHP or static applications, you can choose between FTP and Git deployment
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/appcreationreviewjavawar.png"></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

* the application must be located at the **root** of the git repository

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

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.

