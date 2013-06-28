---
title: Deploying Python
position: 1
---

## Deploying Python apps

Python is a programming language that lets you work more quickly and integrate your systems more effectively. You can learn to use Python and see almost immediate gains in productivity and lower maintenance costs.

### Overview

Python is available on our platform with the version 2.7.3. You can use Git to deploy your application.

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/apppython.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Python":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewpython.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>

### Available extensions and modules

You are granted to install external libs. As you can do on your workstation you can easily use pip and requirements.txt

For example to install Flask and various libs you have to create a file `/requirements.txt` :

```javascript
SQLAlchemy==0.7.8
Flask==0.9
Flask-Admin==1.0.2
Flask-Assets==0.8
Flask-DebugToolbar==0.7.1
Flask-KVSession==0.3.2
Flask-Mail==0.8.2
Flask-SQLAlchemy==0.16
Flask-Script==0.5.3
Flask-WTF==0.8.3
webassets==0.8
pytz==2012d
psycopg2==2.4.5
```

If you have any question, please refer to Support section.


### Configuration file

The configuration file for your Python application must be `clevercloud/python.json`.

#### Select your module

To select which module you want to start, use the key `module` in `deploy` like the following:

```haskell
   {
      "deploy": {
         "module": "myapplication"
      }
   }
```

The module (without .py) must be importable, i.e. be in `PYTHONPATH`. For example with Django: "module":"cc_django_wsgi".


#### Manage your static files

You are able to use a Filesystem Bucket to store your static files. Please refer to the [File System Buckets](/databases-and-services/fs-buckets/) section for creating it.

When your bucket is available, you must set your public folder in `clevercloud/python.json` like below to enable Nginx to serve your static resources:

```haskell
   {
      "deploy": {
         "static": "/mypublicfolder"
      }
   }
```

*Note: the path of your must be absolute regarding the root of your application.*


### CRON configuration file

The configuration file used for crontab is `clevercloud/cron.json`.

Here is the general syntax:

```haskell
  [
    "<string>",
    "<string>"
 ]
```

The string `<string>` must use the cron format\*:
<pre>M H d m Y command</pre>

There are two restrictions about the usage of crontab on our platform:

* The special date `@reboot` is not available since the crontab is added after the startup of the instance
* You must use the absolute path of commands

You can use the special variable `$ROOT` to refer to the root folder of your application.


#### Note about crontab clustering

We do not currently support the clustering of crontab, you must manage it yourself if your application requires more than one instance.

_* For more information about the syntax, you can check <a href="http://en.wikipedia.org/wiki/Cron">this page</a>_

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

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>The remote branch on Clever Cloud is <strong>ALWAYS</strong> master. If your local branch is not "master", use this syntax:</p>
  <pre>git push < name > yourbranch:master</pre>

</div>