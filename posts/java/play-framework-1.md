---
layout: page
title: Play! 1
tags: playframework, scala
---

## Play Framework 1
####Table of Contents
<ul id="cc-tableofcontent__list">
	<li>
		<a href="#introducing-play">
			Introducing Play
		</a>
	</li>
	<li>
		<a href="#play-1-on-clever-cloud">
			Play! 1 on Clever Cloud
		</a>
	</li>
	<li>
		<a href="#configure-your-application">
		Configure your application
		</a>
	</li>
	<li>
		<a href="#example-set-up-a-mysql-database">
		Example: set up a mysql database
		</a>
	</li>
	<li>
		<a href="#deployment-via-git">
		Configure your application
		</a>
	</li>
</ul>
### Introducing Play
Play! is a framework created by Guillaume Bort. It allows you to quickly create ready-to-use web application with Java or Scala. There are currently two major versions of this framework: 1.2 and 2. They are really different from each other. This guide will show you how to deploy application for both versions of the Play! Framework.

<span>More infos: <a href="http://www.playframework.org">Play!Framework</a></span>

### Play! 1 on Clever Cloud

The Clever Cloud supports Play 1.2 applications natively. The present guide explains how to set up your application to run on the Clever Cloud.  
To [create an accout](/create-an-account), [an application](/create-an-app) or [manage your databases](/services), please read the dedicated sections.

### Configure your application

The only file you have to modify is your application.conf in the conf directory.
Your application will be run with the option `--%clevercloud`.  
It means that you can define special keys in your application configuration file that will be used only on the Clever Cloud.

Production mode: Set `application.mode` to `PROD` so the files are compiled at startup time and the errors are logged in a file.

```xml
%clevercloud.application.mode=PROD
```

### Example: set up a mysql database
```xml
%clevercloud.db.url=jdbc:mysql://{yourcleverdbhost}/{dbname}
%clevercloud.db.driver=com.mysql.jdbc.Driver
%clevercloud.db.user={yourcleveruser}
%clevercloud.db.pass={yourcleverpass}
```

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
