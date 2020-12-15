---
title: Deploy a Symfony application
shortdesc: This article shows you how to deploy a Symfony application on Clever Cloud.
tags:
- deploy
keywords:
- php
- symfony
str_replace_dict:
  "@application-type@": "PHP"
---

## Overview

This tutorial assumes that your application is based on Symfony >= 3.4 and Symfony Flex.
Symfony applications almost work out of the box on Clever Cloud, you just have a few adjustments to make.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

## Configure your Symfony application
### Configure `DocumentRoot`

Add a new [environment variable](#setting-up-environment-variables-on-clever-cloud) called `CC_WEBROOT` and set `/public` as its value `CC_WEBROOT=/public`.

### Configure your application secret

`APP_SECRET` [environment variable](#setting-up-environment-variables-on-clever-cloud) is required to generate CSRF tokens. By default for [symfony/framework-bundle](https://GitHub.com/symfony/framework-bundle) generates one when it's installed via [Symfony Flex](https://GitHub.com/symfony/flex). 
If you do not use Flex, make sure to change your `APP_SECRET`. The default value is `ThisTokenIsNotSoSecretChangeIt`, **change it**.

### Configure the Symfony environment

If you're using [`.env` file](https://symfony.com/blog/improvements-to-the-handling-of-env-files-for-all-symfony-versions) in your application, please don't commit productions credentials in this file or in a `.env.production` file. They are not meant to be committed alongside your applications. Clever Cloud allows you to inject environment in your app, so you can dynamically link databases and have separate environments with the same code base.

From the console, you can edit the application's environment variables. Click on "expert mode", you'll be able to directly paste the contents of the `.env` file.

From the CLI, it's even simpler: `clever env import < .env`.

You will also need to set the environment variable `APP_ENV` to one of:

-  dev
-  test
-  prod

You can anyway add your environment with any of the methods mentionned in [Setting up environment variables on Clever Cloud](#setting-up-environment-variables-on-clever-cloud).

### Configure monolog to get application logs

For your application logs to be collected and available in the console and CLI, you need to configure monolog to use its `error_log` output.
That does not mean that it will only output error level logs, you can set it to use any level, here is an exemple with the info level (and above):

```
monolog:
    handlers:
        filter_for_errors:
            type: fingers_crossed
            action_level: error
            handler: error_log_handler
            excluded_404s:
                 # regex: exclude all 404 errors from the logs
                 - ^/

        error_log_handler:
            type: error_log
            level: info
```


### Configure Symfony to work behind Clever Cloud reverse proxies

You can use the `CC_REVERSE_PROXY_IPS` [environment variable](#setting-up-environment-variables-on-clever-cloud) that contains a list of trusted IP addresses, separated by commas.

```
if ($trustedProxies = $request->server->get('CC_REVERSE_PROXY_IPS')) {
    // trust *all* requests
    Request::setTrustedProxies(array_merge(['127.0.0.1'], explode(',', $trustedProxies)),

    // trust *all* "X-Forwarded-*" headers
    Request::HEADER_X_FORWARDED_ALL);
}
```

For more information on configuring symfony behind a reverse proxy, you can read the [official documentation](https://symfony.com/doc/current/deployment/proxies.html).

### Apache 404 error after deployment

If everything looks fine and you still get 404 errors, remember that CleverCloud works with an Apache server, so you'll need an htaccess in the  `/public` directory.
Symfony got your back on this: just run `composer require symfony/apache-pack`. See [the official documentation of Symfony](https://symfony.com/doc/current/setup/web_server_configuration.html) for more information.

{{< readfile "/content/partials/new-relic.md" >}}

{{< readfile "/content/partials/env-injection.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

## Configure your database

Make sure you have created a database add-on in the Clever Cloud console, and that it's linked to your application. When it's done, you will be able to access all of your add-on [environment variables](#setting-up-environment-variables-on-clever-cloud) from the application. You can use them as  `DATABASE_URL=$MYSQL_ADDON_URI`.

### Optional: run tasks after build step

If you want to have database migrations automatically run during each deployment, or frontend assets which must be built, you can write all these commands in `clevercloud/post_build.sh` like this one:

```
# Database migrations
./bin/console doctrine:migrations:migrate --no-interaction

# Frontend build
 yarn install && yarn run build
```

Make sure this file is executable:

```
chmod +x clevercloud/post_build.sh
```

Then, add this to the application's environment variables `CC_POST_BUILD_HOOK=./clevercloud/post_build.sh`.


{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/deploy-ftp.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
