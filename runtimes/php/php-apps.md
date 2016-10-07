---
title: Deploy PHP applications
shortdesc: PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.
tags:
- php
---

# Deploy PHP apps

PHP is available on our platform with the branches 5.4 and 5.5. You can use FTP or Git to deploy your applications.

The HTTP server is [Apache 2](https://httpd.apache.org/), and the PHP code is executed by [PHP-FPM](http://php-fpm.org/).

## Overview

PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded
into HTML.

## Create an application

Refer to the page [Deploy an application on Clever Cloud](/doc/clever-cloud-overview/add-application/).

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>An FTP application is automatically started once the application is created, even if no code has been sent.</p>
    <p>
     When you create a FTP application, a free [FS Bucket](/doc/addons/fs_buckets/) add-on is
     provisioned, named after the application. You will find the FTP
     credentials in the configuration tab of this add-on.
    </p>
  </div>
</div>

## Choose your PHP version

Since January 2016, choosing a PHP version has gotten easier: just set the PHP_VERSION environment
variable to one of the following values:

- 5.4
- 5.5
- 5.6
- 7.0

By default, all new PHP applications are created with a default PHP_VERSION, set to 5.6.
You can of course change it whenever you want then redeploy your application to use the
version you want.

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>PHP5.5 is, since July 10, 2016, End Of Life. It's no longer supported and may be exposed to
    unpatched security vulnerabilities.
    PHP5.4 is already unsupported since September 3, 2015. We strongly urge you to update to at least PHP 5.6.
    Support for PHP5.4 will be discontinued in the next months, with PHP5.5 to follow.</p>
  </div>
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

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>The change of the webroot will be rejected during the deployment if the target directory does not exist or is not a directory.</p>
  </div>
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

**Note**: You can send a request to the support if you need to change a setting which is not in this list.

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

### Configure Apache

We use Apache 2 as HTTP Server. In order to configure it, you can create a `.htaccess` file and set directives inside
this file.

The `.htaccess` file can be created everywhere in you app, depending of the part of the application covered by directives.
However, directives who applies to the entire application must be declared in a `.htaccess` file to the application root.

#### Define a custom HTTP timeout

You can define the timeout of an HTTP request in Apache using the `HTTP_TIMEOUT`
[environment variable](/doc/admin-console/environment-variables/).

By default, the HTTP timeout is se to 3 minutes (180 seconds).

#### Prevent Apache to redirect HTTPS calls to HTTP when adding a trailing slash

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

If you want to force all redirects to HTTPS, you can replace `%{HTTP:X-Forwarded-Proto}` with `https`.

## Composer

We support Composer build out of the box. You just need to provide a `composer.json` file in the root of
your repository and we will run `composer.phar install` for you.

You can check out our current version of composer on these pages:

* [php56info.cleverapps.io/composer](https://php56info.cleverapps.io/composer) for PHP 5.6
* [php70info.cleverapps.io/composer](https://php70info.cleverapps.io/composer) for PHP 7.0

<div class="panel panel-warning">
  <div class="panel-heading">
   <h4>Note:</h4>
  </div>
  <div class="panel-body">
    <p>Add your own `composer.phar` file in the root of your repository if you need to override our version for the build phase.</p>
  </div>
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

Example of a minimalist PHP application using composer and custom scripts: [php-composer-demo](https://github.com/CleverCloud/php-composer-demo)

### GitHub rate limit

Sometimes, you can encounter the following error when downloading dependencies:

```
Failed to download symfony/symfony from dist: Could not authenticate against github.com
```

To prevent this download dependencies's fails that is often caused by rate limit of GitHub API while deploying your apps,
we recommend you to add `oauth` token in your composer configuration file or in separate file named as described in
[composer FAQ (API rate limit and OAuth tokens)](https://getcomposer.org/doc/articles/troubleshooting.md#api-rate-limit-and-oauth-tokens).

You can find more documentation about composer configuration at [getcomposer.com](https://getcomposer.org/doc/04-schema.md).

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

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>Environment variables are displayed in the default output of `phpinfo()`.
    If you want to use `phpinfo()` without exposing environment variables, you have to call it this way:
    </p>
 ```php
 phpinfo(INFO_GENERAL | INFO_CREDITS | INFO_CONFIGURATION | INFO_MODULES | INFO_VARIABLES | INFO_LICENSE)
 ```
 </div>
</div>


## Frameworks and CMS

The following is the list of tested CMS by our team.

It's quite not exhaustive, so it does not mean that other CMS can't work on the Clever Cloud platform.

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
<td>Thelia</td>
</tr>
</tbody>
</table>
</div>

## Available extensions and modules

You can check enabled extensions and versions by viewing our `phpinfo()` example for
[PHP 5.6](https://php56info.cleverapps.io) and [PHP 7.0](https://php70info.cleverapps.io).

**Warning**: some extensions need to be [enabled explicitely](#enable-specific-extensions)

The following extensions are enabled by default: `couchbase`, `imagick`, `memcached`,
`memcache`, `mongodb`, `opcache`, `redis`, `solr`, `ssh2`.

You can add `DISABLE_<extension_name>: true` in your [environment variable](/doc/admin-console/environment-variables/)
to disable them.

If you have a request about modules, feel free to contact our support at <support@clever-cloud.com>.

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>memcache and memcached extensions are not yet available for PHP 7</p>
  </div>
</div>

### Enable specific extensions

Some extensions need to be enabled explicitly. To enable these extensions, you'll need to set the corresponding
[environment variable](/doc/admin-console/environment-variables/):

* APC: set `ENABLE_APC` to `true`.

    APC is a framework for caching and optimizing PHP intermediate code.
    **Warning**: APC is only available for PHP 5.4.

* APCu: set `ENABLE_APCU` to `true`.

    APCu is an in-memory key-value store for PHP. Keys are of type string and values can be any PHP variables.

* IonCube: set `ENABLE_IONCUBE` to `true`.

    IonCube is a tool to obfuscate PHP code. It's often used by paying Prestashop and Wordpress plugins.

* Mongo: set `ENABLE_MONGO` to `true`.

    MongoDB is a NoSQL Database. This extension allows to use it from PHP.
    **Warning**: this extension is now superseded by the `mongodb` extension. We provide it for backward compatibility.

* NewRelic: set `ENABLE_NEWRELIC` to `true`.

    Newrelic Agent for PHP. Newrelic is a software analytics tool.

* OAuth: set `ENABLE_OAUTH` to `true`.

    OAuth consumer extension. OAuth is an authorization protocol built on top of HTTP.

* XDebug: set `ENABLE_XDEBUG` to `true`.

    XDebug is a debugger and profiler tool for PHP.

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>These extensions (except `APC`) are only available for PHP >= 5.5.</p>
  </div>
</div>

## Use Redis to store PHP Sessions

We provide the possibility to store the PHP sessions in a [Redis database](/doc/addons/redis/) to improve the performances of
your application.

To enable this feature, you need to:

 - enable Redis support on the application (create an [environment variable](/doc/admin-console/environment-variables/) named `ENABLE_REDIS` with the value `true`.)
 - create and link a Redis add-on 
 - create an [environment variable](/doc/admin-console/environment-variables/) named `SESSION_TYPE` with the value `redis`.

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4>Warning:</h4>
  </div>
  <div class="panel-body">
    <p>You must have a <a href="/addons/redis/">Redis</a> add-on
    <a href="/addons/clever-cloud-addons/#link-an-add-on-to-your-applicaiton">linked with your application</a>
    to enable PHP session storage in Redis.<br />
    If no Redis add-on is linked with your application, the deployment will fail.</p>
    </div>
</div>

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via **Git or FTP**. Follow
[these steps](/doc/clever-cloud-overview/add-application/) to deploy your application.
