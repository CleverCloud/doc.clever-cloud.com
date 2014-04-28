---
title: Deploy PHP applications
shortdesc: PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.
---

## Deploy PHP apps

PHP is available on our platform with the version 5.5.11. You can use FTP or Git to deploy your applications.

### Overview

PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/php5.5/php5.5_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
3. You can choose between FTP and Git deployment
<div class="alert alert-hot-problems">
  <h4>Warning:</h4>
  <p>An FTP application is automatically started once the application is created, even if no code has been sent.</p>
</div>

4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/php5.5/php5.5_create.png"/></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### CRON configuration file

The configuration file used for crontab is **clevercloud/cron.json**. It
is only available for <strong>PHP</strong> applications at this time.

Here is the general syntax:

```haskell
  [
    "<string>",
    "<string>"
  ]
```

The string `<string>` must use the cron format\*:
<pre>M H d m Y command</pre>

There are two restrictions about the usage of crontab on our platform:

* The special date `@reboot` is not available since the crontab is added after the startup of the instance
* You must use the absolute path of commands

You can use the special variable `$ROOT` to refer to the root folder of your application.

Example of `clevercloud/cron.json` which executes the file `cron.php` every 5 minutes:

```haskell
  [
    "*/5 * * * * /usr/bin/php $ROOT/cron.php"
  ]
```

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>We do not currently support the clustering of cron tasks, you must manage it yourself if your application requires more than one instance.</p>
</div>


_* For more information about the syntax, you can check <a href="http://en.wikipedia.org/wiki/Cron">this page</a>_


###Configuration files for PHP applications

The configuration file for your PHP application must be
`clevercloud/php.json`, that is a *php.json* file in a *clevercloud*
folder at the root of your application.

#### Change the webroot

Since one of the best practices of PHP development is to take the libraries and core files outside the webroot, you may want to set another webroot than the default one (*the root of your application*).

To change the webroot, just set the key `webroot` in the `deploy` part
of the configuration file *clevercloud/php.json* with the absolute path (*from the root of your application*) of your new public folder.

In the following example we want to set the webroot to the folder `/public`:

```javascript
  {
    "deploy": {
      "webroot": "/public"
    }
  }
```

Please note the absolute path style: `/public`.

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
 <p>The change of the webroot will be rejected during the deployment if the target directory does not exist or is not a directory.</p>
</div>

#### Execute a custom script after the deploy

Some frameworks or custom applications might require bootstrapping before the application may run (_e.g. Composer_).
You can achieve this by creating a custom script with your commands and adding the following line in `clevercloud/php.json`:

```javascript
   {
      "hooks": {
         "postDeploy": "pathtoyourscript"
      }
   }
```

##### Example

You use Composer to manage dependencies of your project and you want to execute _composer.phar install_ before running your app.

First, add a file `ccbuild.sh` at the root of your project with these lines:

```bash
#!/bin/bash

php composer.phar install
```

Then add these lines in `clevercloud/php.json`:

```javascript
   {
      "hooks": {
         "postDeploy": "ccbuild.sh"
      }
   }
```

<strong>Note: </strong>You must add the _execute_ permission to your file (`chmod u+x yourfile`) before pushing it.



### Frameworks and CMS

The following is the list of tested CMS by our team.

It's quite not exhaustive, so it doesn't mean that other CMS can't work on the Clever Cloud platform.  

<div class="">
<table class="table table-bordered">
  <tbody>
    <tr>
      <td>Wordpress</td>
      <td>Prestashop</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td>Dokuwiki</td>
      <td>Joomla</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td>SugarCRM</td>
      <td>Drupal</td>
    </tr>
    <tr>
      <td>Magento</td>
      <td>Status.net</td>
  </tr>
    <tr>
      <td>Symfony</td>
      <td> </td>
  </tr>
  </tbody>
</table>
</div>

### Available extensions and modules

You can check enabled extensions and versions by viewing our <a href="http://phpinfo.cleverapps.io" target="_blank">phpinfo() example</a>.

If you have a request about modules, feel free to ask on <a href="https://groups.google.com/forum/?fromgroups#!forum/clever-cloud-users" target="_blank">Clever Cloud user group</a>.


### Deploy on Clever Cloud

Application deployment on Clever Cloud is via **Git or FTP**. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
