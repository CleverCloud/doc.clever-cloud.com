---
layout: page
title: Play! 2 Java
tags: playframework, scala
---

## Play Framework 2 Java
####Table of Contents
<ul id="cc-tableofcontent__list">
	<li>
		<a href="#introducing-play">
			Introducing Play
		</a>
	</li>
	<li>
		<a href="#play-2-on-clever-cloud">
			Play! 2 on Clever Cloud
		</a>
	</li>
	<li>
		<a href="#configure-your-application">
		Configure your application
		</a>
	</li>
	<li>
		<a href="#known-problems-with-play-2">
		Known problems with Play! 2
		</a>
	</li>
	<li>
		<a href="#deployment-via-git">
		Deployment via Git
		</a>
	</li>
</ul>
### Introducing Play
Play! is a framework created by Guillaume Bort. It allows you to quickly create ready-to-use web application with Java or Scala. There are currently two major versions of this framework: 1.2 and 2. They are really different from each other. This guide will show you how to deploy application for both versions of the Play! Framework.

<span>More infos: <a href="http://www.playframework.org">Play!Framework</a></span>

### Play! 2 on Clever Cloud

The Clever Cloud supports Play 2.0.x applications natively. The present guide explains how to set up your application to run on the Clever Cloud.
To [create an accout](/create-an-account), [an application](/create-an-app) or [manage your databases](/services), please read the dedicated sections.
The code of your application and, if you use one, the
`clevercloud` folder containing the `play.json` file must be placed at the root of your git repository.

### Configure your application
To configure you Play! 2 application, you might need a file named
`clevercloud/play.json` that is a `play.json` file placed in a
`clevercloud` folder at the root of your application.

<div class="alert alert-hot-problems">
	<h4>Please note:</h4>
	<p>That file is optional and is used to set more
configuration elements to the start command.</p>
</div>

The file must contain the
following fields:

```javascript
{
   "deploy":{
	   "goal":<string>
	}
}
```

**goal**
: That field should contain additional configuration like
`"-Dconfig.resource=clevercloud.conf"`.

<div class="alert alert-hot-problems">
	<h4>Tip:</h4>
	<p>do not forget the double quotes
	around the "goal"’s value.</p>
</div>

### Known problems with Play! 2

If your project fails with error **sbt.ResolveException: unresolved
dependency: play#sbt-plugin;2.0: not found** read the following:

Some versions of Play2 try to retrieve a nonexistent version of
"sbt-plugin" which is required by the framework to work.
You have two options to fix this problem:

(Best version) You can set the "play.version" environment variable in the
`clevercloud/play.json` file. For example, for Play 2.0.4:

``` javascript
{
	"deploy": {
		"goal": "-Dplay.version=2.0.4"
	}
}
```

Otherwise, you can modify plugins.sbt in the project folder of your
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

### Deployment via Git

Like all java-based applications, Play apps have to be deployed *via* Git.
To deploy via Git, see details here: <a href="/git-deploy-java">Git deploy</a>.

<script type="text/javascript">
$('.cc-content__text ul li a').click(function(){
    $('html, body').animate({
        scrollTop: $( $(this).attr('href') ).offset().top - 1
    }, 500);
    return false;
});
</script>
