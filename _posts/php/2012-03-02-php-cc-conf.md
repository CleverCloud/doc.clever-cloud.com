---
layout: page

title: Configuration file
tags:
- PHP
---

#Configuration files for PHP applications

For more information about the general usage of configuration files, please refer to <a href="/cc-conf/">this page</a>.

## Change the webroot

Since one of the best practices of PHP development is to take the libraries and core files outside the webroot, you may want to set another webroot than the default one (*the root of your application*).

To change the webroot, just set the key `webroot` in the `deploy` part of the main configuration file **cc_conf.json** with the absolute path (*from the root of your application*) of your new public folder.

In the following example we want to set the webroot to the folder `/public`:

{% highlight javascript %}
   {
      "deploy": {
                  "webroot": "/public"
                }
   }
{% endhighlight %}

### Limitation

The change of the webroot will be rejected during the deployment if the target directory does not exist or is not a directory.
