---
title: Play Framework 2
position: 2
---

## Deploying Play Framework 2 Java

Clever Cloud supports Play! 2 applications natively. The following guide explains how to set up your application to run the Clever Cloud.  

### Necessary information

* the application must be located at the **root** of the git repository

### Configure your application

You can configure your application start command by adding a `./clevercloud/play.json` file with the following fields:

```javascript
{
  "deploy":{
    "goal":<string>
  }
}
```

**goal** can for example contain additional configuration like
`"-Dconfig.resource=clevercloud.conf"` or `"-Dplay.version=2.0.4"`.


### Known problems with Play! 2

Please read the following if your project fails with this error:  

`sbt.ResolveException: unresolved dependency: play#sbt-plugin;2.0: not found`

Some versions of Play2 try to retrieve a nonexistent version of
"sbt-plugin" which is required by the framework to work.
You have two options to fix this problem:

You can set the "play.version" environment variable in the
`clevercloud/play.json` file.  
For example, for Play 2.0.4:

``` javascript
{
  "deploy": {
    "goal": "-Dplay.version=2.0.4"
  }
}
```

Otherwise, you can modify `plugins.sbt` in the project folder of your
app like the following:

``` scala
// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("play" % "sbt-plugin" % "2.0.4") // The important part of the configuration
```

The two solutions do the job, you can pick your favorite.


### Tutorial - Play! application deployment

<p>
  <iframe width="640"; height="360" src="http://www.youtube.com/embed/HL366BhWFMw" frameborder="0" allowfullscreen></iframe>
</p>


More info on <a target="_blank" href="http://www.playframework.com">playframework.com</a>.