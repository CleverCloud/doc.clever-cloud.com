---
layout: page

title: Java+Maven
tags:
- Java
---

#Java basics

For now, Java can be deployed on the Clever Cloud using maven.

To run your app, you can, for example, use plugins like cargo
(<a href="http://cargo.codehaus.org/Maven2+plugin">Find it here</a>).

## Informations for all the projects

Your application must be set to listen on the port 8080.

## Configuration file

### Mandatory configuration
For every Java project using Maven, you have to add the
`clevercloud/maven.json` file, that is a `maven.json` file in a
`clevercloud` folder placed at the root of your application.

The `maven.json` has to contain at least the following value:

{% highlight javascript%}
    {
      "deploy": {
        "goal": <string>
      }
    }
{% endhighlight %}

**goal**
: that field must contain the maven goal you want to execute on deploy.
An example of what can be found as a `goal` value is
"-Dtest.active=false -Dexec.mainClass=\"com.example.Main\" assembly:jar-with-dependencies exec:java".
Note that the `goal` field must be double-quoted.

### Optional configuration

The full configuration can look like the following:
{% highlight javascript%}
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
{% endhighlight %}

* ``"build"`` is an object with the goal to execute.
  * ``"type"`` can be ``"maven"`` or ``"ant"``.
  * ``"goal"`` is the target you want to use to build your project.
* ``"deploy"`` is an object containing the goal to execute.
  * ``"goal"``: the goal/target and options you want to execute to deploy/run you project.
  * ``"javaVersion"``: the version of java you want to use to run your app. Values: 6, 7. Default : 7.
## Deployment via Git
To deploy via Git, see details here: <a href="/git-deploy-java">Git Deploy</a>.
