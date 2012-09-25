---
layout: page

id: cc_conf
parent: app_configuration
next: java_and_scala
---
Deployment configuration
============

For every java or scala project using Maven, Ant or SBT, you need to write a small specific JSON file, copied at the root of your project. Make sure this file is called
**cc_conf.json**

This JSON is the configuration file that you will need for some deployments and builds. Here is the syntax:
{% highlight javascript%}
    {
      "jarName": "<string>",
      "build": "<string>",
      "goal": "<string>"
    }
{% endhighlight %}


* "jarName" is a string containing the name of the main jar used to launch your application, with the extension.
* "build" is a string with the builder you want to use. We currently support Maven, SBT and Ant.
* "goal" is a string with a list of the parameters and/or the goals that Maven/Ant have to execute. If you build via Maven and don't fill this field, the "package" command will be applied.

Example of cc_conf.json for a Maven build:

{% highlight javascript%}
    {
      "build": "maven",
      "goal": "-Dtest.active=false assembly:jar-with-dependencies"
    }

{% endhighlight %}

Example of cc_conf.json for Ant build:

{% highlight javascript%}
    {
      "build": "ant",
      "goal": "exterminate -Ddoctor.version=11"
    }
{% endhighlight %}

Example of cc_conf.json for jar deploy:  

{% highlight javascript%}
    {
      "jarName": "Muad-dib.jar"	
    }  
{% endhighlight %}
