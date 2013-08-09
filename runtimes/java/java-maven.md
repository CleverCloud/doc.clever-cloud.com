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

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.