---
title: Deploy PHP applications
shortdesc: PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.
tags:
- php
str_replace_dict:
  "@application-type@": "PHP"
---

## Overview

PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded
into HTML.

PHP is available on our platform with the branches 5.6, 7.2, 7.3, 7.4 and 8.0. You can use FTP or Git to deploy your applications.

The HTTP server is [Apache 2](https://httpd.apache.org/), and the PHP code is executed by [PHP-FPM](https://php-fpm.org/).

## Memory Limit

A smaller part of your application's memory is defined as the memory limit to allow workers to run efficiently, here is the memory limit for each flavor:

{{<table "table table- bordered" "text-align:center" >}}
 | <center>Flavor</center> | <center>Memory Limit</center> |
 |-----------------------|------------------------------|
 |Pico | 64M |
 |Nano | 64M |
 |XS | 128M |
 |S | 256M |
 |M | 384M |
 |L | 512M |
 |XL | 768M |
 |2XL | 1024M |
 |3XL | 1536M |
 |4XL+ | 2048M |
 {{< /table >}}

To change this limit you can define `MEMORY_LIMIT` [environment variable]({{< ref "reference/reference-environment-variables.md#php" >}}).

If you define a limit exceeding the application memory it will use the default one.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

{{< readfile "/content/partials/language-specific-deploy/php.md" >}}

{{< readfile "/content/partials/new-relic.md" >}}

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via **Git or FTP**.

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/deploy-ftp.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
