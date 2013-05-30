---
title: Java Maven
position: 3
---

## Deploying Java Maven projects

The Clever Cloud offers you to run your Java Maven projects. You can deploy this kind of project without changing your code, but running it on Clever Cloud needs some configuration files, to add parameters like your targeted container for instance.


### Overview
Maven is essentially a project management and comprehension tool and as such provides a way to help with managing:

* Builds
* Documentation
* Reporting
* Dependencies
* SCMs
* Releases
* Distribution


### About Cargo
To run your app, you can, for example, use plugins like cargo
(<a href="http://cargo.codehaus.org/Maven2+plugin">Find it here</a>).
Your application must be set to listen on the port 8080.

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavamaven.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Java with Maven":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavamaven.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>

### Configuration file

For every Java project using Maven, you **HAVE TO** to write a small specific JSON
file, named *maven.json* in the *clevercloud* folder placed at the
root of your application.

The `maven.json` has to contain at least the following value:

```haskell
  {
    "deploy": {
    "goal": <string>
    }
  }
```

<div class="alert">
<h5>About the field *goal*</h5>
That field must contain the maven goal you want to execute on deploy.
</div>

An example of what can be found as a goal value is:  

```haskell
"-Dtest.active=false -Dexec.mainClass=\"com.example.Main\" assembly:jar-with-dependencies exec:java"
```

*Note that the goal field must be double-quoted.*

#### Optional configuration

The full configuration can look like the following:

```haskell
{
  "build": {
    "type": "<string>",
    "goal": "<string>"
  },
  "deploy": {
    "javaVersion": <integer>,
    "goal": "<string>"
  }
}
```
You can use the following properties: 

* ``"build"`` is an object with the goal to execute.
  * ``"type"`` can be ``"maven"`` or ``"ant"``.
  * ``"goal"`` is the target you want to use to build your project.
* ``"deploy"`` is an object containing the type of deploy (Maven, Ant or SBT) and the goal to execute.
  * ``"goal"``: the goal/target and options you want to execute to deploy/run you project.
  * ``"javaVersion"``: the version of java you want to use to run your app.  
Values can be 6 or 7. Default is 7.

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