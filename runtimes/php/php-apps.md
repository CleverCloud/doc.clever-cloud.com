---
title: Deploy PHP applications
shortdesc: PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.
---

# Deploy PHP apps

PHP is available on our platform with the branches 5.4 and 5.5. You can use FTP or Git to deploy your applications.

## Overview

PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded
into HTML.

## Create an application

1. Create a new app by clicking on the **Add an application** button, in the headbar.
2. Select the language/framework: <figure class="cc-content-img"><img src="/assets/images/select-lang.png"/></figure>
3. Select between Git and FTP deployments.<figure class="cc-content-img"><img src="/assets/images/select-depl.png"/></figure>
3. Select the scalability options: <figure class="cc-content-img"><img src="/assets/images/select-scalab.png"/></figure>
3. Enter your application's name and description, choose your deployment zone and click "Create".
<figure class="cc-content-img"><img src="/assets/images/choose-name.png"/></figure>
5. *Optional*: [add an add-on](/addons/clever-cloud-addons/).

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
<p>An FTP application is automatically started once the application is created, even if no code has been sent.</p>
<p>
 When you create a FTP application, a free [FS Bucket](/addons/fs_buckets/) add-on is
 provisioned, named after the application. You will find the FTP
 credentials in the configuration tab of this add-on.
</p>
</div>

## Configuration files for PHP applications

The configuration file for your PHP application must be `/clevercloud/php.json`, that is a *php.json* file in a
`/clevercloud` folder at the root of your application.

### Change the webroot

Since one of the best practices of PHP development is to take the libraries and core files outside the webroot, you may
want to set another webroot than the default one (*the root of your application*).

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

### Change PHP settings

Most of PHP settings can be changed using a `.user.ini` file. Please refer to the
[official documentation](http://www.php.net/manual/en/configuration.file.per-user.php) for more information.

Other settings can be changed by adding the following line in `clevercloud/php.json`:

```javascript
   {
      "configuration": {
         "mbstring.func_overload": 2,
         "another.setting": "value"
      }
   }
```

Here is the list of available settings:

* `mbstring.func_overload`

<strong>Note: </strong>You can send a request to the support if you need to change a setting which is not in this list.

#### Maximum PHP Children per instance

You can fix the maximum number of PHP running processes per instance by adding the following line in `clevercloud/php.json`:

```javascript
   {
      "configuration": {
         "pm.max_children": 32
      }
   }
```

This setting is usefull if you need to limit the number of running processes according to the maximum connections limit 
of your MySQL or PostgreSQL database.

By default, `pm.max_children` is set to **10**.

### Prevent Apache to redirect HTTPS calls to HTTP when adding a trailing slash

`DirectorySlash` is enabled by default on the PHP scalers, therefore Apache will add a trailing slash to a resource when
 it detects that it is a directory.

eg. if foobar is a directory, Apache will automatically redirect http://example.com/foobar to http://example.com/foobar/

Unfortunately the module is unable to detect if the request comes from a secure connection or not. As a result it will
force an HTTPS call to be redirected to HTTP.

In order to prevent this behavior, you can add the following statements in a `.htaccess` file:

```apache
DirectorySlash Off
RewriteEngine On
RewriteCond %{REQUEST_FILENAME} -d
RewriteRule ^(.+[^/])$          %{HTTP:X-Forwarded-Proto}://%{HTTP_HOST}/$1/  [R=301,L,QSA]
```

These statements will keep the former protocol of the request when issuying the redirect. Assuming that the header
X-Forwarded-Proto is always filled (which is the case on our platform).

If you want to force all redirections to HTTPS, you can replace `%{HTTP:X-Forwarded-Proto}` with `https`.


### Composer

We support Composer build out of the box. You just need to provide a `composer.json` file in the root of
your repository and we will run `composer.phar install` for you.

The PHP instances embed the latest release of Composer. You can check it on the following pages:

* [php54info.cleverapps.io/composer](https://php54info.cleverapps.io/composer) for PHP 5.4
* [php55info.cleverapps.io/composer](https://php55info.cleverapps.io/composer) for PHP 5.5

<div class="alert alert-hot-problems">
 <h4>Note:</h4>
 <p>Add your own `composer.phar` file in the root of your repository if you need to override our version for the build phase.</p>
</div>


Example of a `composer.json` file:

```javascript
{
   "require": {
      "laravel/framework": "4.1.*",
      "ruflin/Elastica": "dev-master",
      "shift31/laravel-elasticsearch": "dev-master",
      "natxet/CssMin": "dev-master"
   },
   "repositories": [
      {
         "type": "vcs",
         "url": "https://github.com/timothylhuillier/laravel-elasticsearch.git"
      }
   ],
   "autoload": {
      "classmap": [
         "app/controllers",
         "app/models",
         "app/database/migrations",
         "app/database/seeds"
      ],
      "psr-0": {
         "SomeApp": "app"
      }
   },
   "config": {
      "preferred-install": "dist"
   },
   "minimum-stability": "dev"
}
```

#### GitHub rate limit

To prevent download dependencies's fails like 

```
Failed to download symfony/symfony from dist: Could not authenticate against github.com
```

that is often caused by rate limit of GitHub API while deploying your apps, we recommend you to add `oauth` token in
your composer configuration file or in separate file nammed as describe in
[this](https://getcomposer.org/doc/articles/troubleshooting.md#api-rate-limit-and-oauth-tokens) FAQ entry.

You can find more documentation about composer configuration at [getcomposer.com](https://getcomposer.org/doc/04-schema.md).



### Execute a custom script after the deploy

Some frameworks or custom applications might require bootstrapping before the application may run (_e.g. Composer_).
You can achieve this by creating a custom script with your commands and adding the following line in `clevercloud/php.json`:

```javascript
   {
      "hooks": {
         "postDeploy": "pathtoyourscript"
      }
   }
```

#### Example

You use Artisan to manage your project and you want to execute _artisan migrate_ before running your app.

First, add a file `ccbuild.sh` at the root of your project with these lines:

```bash
#!/bin/bash

php artisan migrate
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


## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

To access the variables, use the `getenv` function. So, for example, if
your application has a postgresql add-on linked:

```php
<?php

$dbh = new PDO(
	'postgresql:host='.getenv("POSTGRESQL_ADDON_HOST").';dbname='.getenv("POSTGRESQL_ADDON_DB"),
	getenv("POSTGRESQL_ADDON_USER"),
	getenv("POSTGRESQL_ADDON_PASSWORD")
);
```

## Frameworks and CMS

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

## Available extensions and modules

You can check enabled extensions and versions by viewing our `phpinfo()` example for
[PHP 5.4](https://php54info.cleverapps.io) and [PHP 5.5](https://php55info.cleverapps.io).

If you have a request about modules, feel free to ask on
[Clever Cloud user group](https://groups.google.com/forum/?fromgroups#!forum/kclever-cloud-users).


## Use Redis to store PHP Sessions

We provide the possibility to store the PHP sessions in a [Redis database](/addons/redis/) to improve the performances of
your application.

To enable this feature, you need a Redis add-on and you have to create an
[environment variable](/admin-console/environment-variables/) named `SESSION_TYPE` with the value `redis`.

<div class="alert alert-hot-problems">
 <h4>Warning:</h4>
 <p>You must have a <a href="/addons/redis/">Redis</a> add-on
 <a href="/addons/clever-cloud-addons/#link-an-add-on-to-your-applicaiton">linked with your application</a>
 to enable PHP session storage in Redis.<br />
 If no Redis add-on is linked with your application, the deployment will fail.</p>
</div>


## Deploy on Clever Cloud

Application deployment on Clever Cloud is via **Git or FTP**. Follow
[these steps](/clever-cloud-overview/add-application/) to deploy your application.
