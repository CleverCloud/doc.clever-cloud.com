---
layout: page

title: Node dependencies
tags:
- Javascript
- Node.js

---

# Describing package.json

For every Node.js application you want to deploy on the Clever Cloud, you need
to provide a `package.json` file at the root of your project’s directory, even if your app has no dependencies.

If you already are a Node.js guru, you probably won’t have to change anything to that
file. Just check the following documented fields.

The `package.json` file should _at least_ contain the following fields:

{% highlight javascript%}
    {
        "main" : "myapp.js",
        "scripts" : {
            "start" : "node myapp.js"
        },
        "engines" : {
            "node" : ">=0.6"
        }
    }
{% endhighlight %}

**main**
: Should be the relative path to the main file we will start using node.

**scripts.start**
: If you prefer something more complicated, just provide a `script.start` field *instead of* the `main` one.

**engines.node**
: Sets the node engine version you app runs with. Any ">=" version will lead to
run the application with the latest local version. See
[here](/node-version-and-modules) to learn more about supported versions and
modules.

You can find more help about the `package.json` file at <a href="http://package.json.nodejitsu.com/">http://package.json.nodejitsu.com/</a>.
