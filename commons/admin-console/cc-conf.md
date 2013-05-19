---
title: Configuration
---

##Configuration files format

This page describes the Clever Cloud configuration files format for your applications.

All of the configuration files must be placed in a folder named
`clevercloud` at the root of your project and use the JSON syntax if not
told otherwise. Please note that even if you use FTP to deploy your application, you must request a redeploy to apply the changes made on these files.

### General configuration file syntax

The configuration file for a specific instance is described in that
specific instance documentation. In general, the configuration file is
named after the instance type (e.g. clevercloud/php.json,
clevercloud/maven.json…).

Here is the general syntax:
```haskell
{
  "build": object,
  "deploy": object
    }
```

For more information about the available and mandatory keys for each language, please refer to the specific pages:
 * [Configuration files for PHP](/php-cc-conf/)
 * [Configuration files for Java+war](/java-war/#configuration_file)
 * [Configuration files for Java+maven](/java-maven/)


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

Example of cc_cron.json which executes the file `cron.php` every 5 minutes:
```haskell
  [
    "*/5 * * * * /usr/bin/php $ROOT/cron.php"
  ]
```


#### Note about crontab clustering

We do not currently support the clustering of crontab, you must manage it yourself if your application requires more than one instance.

_* For more information about the syntax, you can check <a href="http://en.wikipedia.org/wiki/Cron">this page</a>_
