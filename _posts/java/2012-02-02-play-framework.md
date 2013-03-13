---
layout: page

title: Play! Framework
tags:
- Java

---

# <span class="label">Beta</span> Play Framework 
####Table of Contents
<ul style="list-style:none">
	<li>
		<a href="#introducing_play">
			<div class="unselectable " >Introducing Play<span class="unselectable caret pointer-table-content"></span></div>
		</a>
	</li>
	<li>
		<a href="#play_12x">
			<div class="unselectable " >Play 1.2.x<span class="unselectable caret pointer-table-content"></span></div>
		</a>
	</li>
	<li>
		<a href="#play_2">
		<div class="unselectable " >Play 2<span class="unselectable caret pointer-table-content"></span></div>
		</a>
	</li>
</ul>
## Introducing Play
Play! is a framework created by Guillaume Bort. It allows you to quickly create ready-to-use web application with Java or Scala. There are currently two major versions of this framework: 1.2 and 2. They are really different from each other. This guide will show you how to deploy application for both versions of the Play! Framework.  

<div class="alert alert-hot-problems">
<h4>Please note:</h4>
<p>This framework is still in beta.</p>
</div>

<small>More infos: <a href="http://www.playframework.org">Play!Framework</a></small>

## Play! 1.2.x

The Clever Cloud supports Play 1.2 applications natively. The present guide explains how to set up your application to run on the Clever Cloud.
To [create an accout](/create-an-account), [an application](/create-an-app) or [manage your databases](/services), please read the dedicated sections.


### Configure your application

The only file you have to modify is your application.conf in the conf directory.
Your application will be run with the option `--%clevercloud`. It means that you can define special keys in your application configuration file that will be used only on the Clever Cloud.

Production mode: Set `application.mode` to `PROD` so the files are compiled at startup time and the errors are logged in a file.

{% highlight properties %}
%clevercloud.application.mode=PROD

### Example: set up a mysql database
%clevercloud.db.url=jdbc:mysql://{yourcleverdbhost}/{dbname}
%clevercloud.db.driver=com.mysql.jdbc.Driver
%clevercloud.db.user={yourcleveruser}
%clevercloud.db.pass={yourcleverpass}
{% endhighlight %}

## Play! 2

The Clever Cloud supports Play 2.0.x applications natively. The present guide explains how to set up your application to run on the Clever Cloud.
To [create an accout](/create-an-account), [an application](/create-an-app) or [manage your databases](/services), please read the dedicated sections.
The code of your application and, if you use one, the cc_conf.json must be at the root of your git repository.

## Configure your application
To configure you Play! 2 application, you might need a file called `cc_conf.json` at
the root of your application.

<div class="alert alert-hot-problems">
	<h4>Please note:</h4>
	<p>That file is optional and is used to set more
configuration elements to the start command.</p>
</div>

The file must contains the
following fields:

{% highlight javascript %}
{
   "deploy":{
	   "goal":<string>
	}
}
{% endhighlight %}

**goal**
: That field should contain additional configuration like
`"-Dconfig.resource=clevercloud.conf"`. 

<div class="alert alert-hot-problems">
	Tip: do not forget the double quotes
	around the "goal"’s value.
</div>

### Deployment via Git

Like all java-based applications, Play apps have to be deployed *via* Git.
To deploy via Git, see details here: <a href="/git-deploy-java">Git deploy</a>.

<script type="text/javascript">
$('#center a').click(function(){
    $('html, body').animate({
        scrollTop: $( $(this).attr('href') ).offset().top - 0
    }, 500);
    return false;
});
</script>
