---
title: Deploy Your App
position: 1
shortdesc: Python 2.7.8 and 3.4.1 are available on our platform. You can use Git to deploy your application.
tags:
- python
---

# Deploy Python apps

Python is a programming language that lets you work more quickly and integrate your systems more effectively.
You can learn how to use Python and see almost immediate gains in productivity and lower maintenance costs.


## Overview

Python 2.7 and 3.4 are available on our platform. You can use Git to deploy your application.


## Create an application

Refer to the page [Deploy an application on Clever Cloud](/doc/clever-cloud-overview/add-application/).

## Available extensions and modules

You are granted to install external libs. As you can do on your workstation you can easily use **pip** and **requirements.txt**.

For example to install *Flask* and various libs you have to create a file `/requirements.txt` :

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

If you have any question, feel free to [contact our support](https://www.clever-cloud.com/doc/get-help/support/).


## Configuration file

The configuration file for your Python application must be `/clevercloud/python.json`.

You can find a [example of Flask application](/doc/python/python-flask-sample-app/) who use concept detailed below.


### Select your module

To select which module you want to start, use the key `module` in `deploy` like the following:

```haskell
   {
      "deploy": {
         "module": "mymodule:app"
      }
   }
```

The module (without .py) must be importable, i.e. be in `PYTHONPATH`. For example with *Flask*, it's gonna be the name of
your main server file followed by your Flask object: `server:app` for example if you have a `server.py` file at the root
of your project with a Flask `app` object inside.

Basically, you should just point to a WSGI capable object.


### Manage your static files

You are able to use a Filesystem Bucket to store your static files. Please refer to the
[File System Buckets](/doc/addons/clever-cloud-addons/#fs-buckets-file-system-with-persistance/) section.

When your bucket is available, to enable Nginx to serve your static resources you must set your public folder in
`clevercloud/python.json` like below:

```haskell
   {
      "deploy": {
         "static": "/mypublicfolder"
      }
   }
```

**Note**: the path of your folder must be absolute regarding the root of your application.


### Use Python 3

The default version of python on Clever Cloud is **2.7**, if you want to use python **3.4** instead, use the file
`/clevercloud/python_version` and put `3` in it.

**Note**: the version is an integer, do not use quotes. values allowed are `2` and `3`.


## Environment injection

Clever Cloud can inject environment variables that are defined in the dashboard and by add-ons linked to your application.

The access to these variables is simple: just get them as you would with any environment variable:

```python
import os
os.getenv("MY_VARIABLE")
```


## Git Deployment

*You will need git on your computer to deploy via this tool. Here is the official website of Git to get more
information: [git-scm.com](http://git-scm.com)*

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following message:

<figure class="cc-content-img">
  <img src="/doc/assets/images/newgitapp.png"/>
</figure>


### Setting up your remotes

1. The "Information" page of your app gives you your git deployment URL. It looks like this:
``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``. Copy it in your clipboard.

2. On your computer, go into your application repository. 
If you didn't already track your app with **git**, start by typing:

    ```bash
    $ git init
    ```

3. Then, use the `git remote` command to add the deploy URL:

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
  <pre>git push < name > yourbranch:master</pre>
</div>
