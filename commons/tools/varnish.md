---
title: Varnish as HTTP Cache
position: 3
---

# Varnish

[Varnish](https://www.varnish-cache.org/) is a HTTP proxy-cache, which works as a reverse proxy between your application
and the client. Following rules defined by the user, Varnish will caches the data of an application to reduce the load.

## Enable Varnish for your application

Varnish is provided by default in PHP >= 5.5 instances. To enable it, you just have to create a `varnish.vcl` file
in the `/clevercloud` folder.

This file describes how Varnish caches your applications and how it decides to return a cached resource or not.

<div class="alert alert-hot-problems">
<h5>Warning:</h5>
<div>The <code>vcl 4.0;</code> and backend section of the `varnish.vcl` configuration file are not necessary as they
are already handled by Clever Cloud.
</div>
</div>

## Varnish version / version migration

We use **Varnish 4**. If you already have a configuration file written for varnish 3, you can use
[varnish3to4](https://github.com/fgsch/varnish3to4) to convert it to varnish 4. Don't forget to add `vcl 4.0;`
at the beginning of the config file.

## Example files

We provide some [examples of Varnish configuration files](https://github.com/CleverCloud/varnish-examples) that you can
use for your application. Just create the `/clevercloud` folder if it does not exist, rename the file to `varnish.vcl`
and place in the `/clevercloud` folder.
