---
layout: page

title: Configuration file
tags:
- PHP
---

#Configuration files for PHP applications

The configuration file for your PHP application must be
`clevercloud/php.json`, that is a `php.json` file in a `clevercloud`
folder at the root of your application.

## Change the webroot

Since one of the best practices of PHP development is to take the libraries and core files outside the webroot, you may want to set another webroot than the default one (*the root of your application*).

To change the webroot, just set the key `webroot` in the `deploy` part
of the configuration file **cleverclou/php.json** with the absolute path (*from the root of your application*) of your new public folder.

In the following example we want to set the webroot to the folder `/public`:

{% highlight javascript %}
   {
      "deploy": {
                  "webroot": "/public"
                }
   }
{% endhighlight %}

Please note the absolute path style: `/public`.

### Limitation

The change of the webroot will be rejected during the deployment if the target directory does not exist or is not a directory.
