---
title: Running crons on Clever Cloud
position: 1
---

# CRON configuration file

The configuration file used for crontab is **clevercloud/cron.json**.

Here is the general syntax:

```haskell
  [
    "<string>",
    "<string>"
  ]
```

The string `<string>` must use the cron format:
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


