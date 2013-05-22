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

You can install module via pip.

If you have any question, please refer to Support section.

### Configuration file

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

### Manage your static files

You are able to use a Filesystem Bucket to store your static files.

To mount and use a static folder. You have to subscribe to a Filesystem Bucket in Clever-Coud Console. And then you can put theses following lines in `clevercloud/bucket.json`:

```javascript
[
  {
    "bucket" : "YOUR BUCKET ID",
    "folder" : "/YOUR_FOLDER"
  }
]
```
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
