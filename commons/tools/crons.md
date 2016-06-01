---
title: Running crons
position: 1
tags:
- developer
---

# CRON configuration file

The configuration file used for crontab is `/clevercloud/cron.json`.

Here is the general syntax:

```haskell
  [
    "<string>",
    "<string>"
  ]
```

The string `<string>` must use the cron format:

```javascript
M H d m Y command
```

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
  <p>All the servers are configured to use Coordinated Universal Time (UTC), please keep it in mind when configuring cron tasks to run at a specific hour.</p>
</div>

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>We do not currently support the clustering of cron tasks, you must manage it yourself if your application requires more than one instance.</p>
  <p>Also, please note that environment variables like `INSTANCE_NUMBER` are not yet injected into crons. You have to add this line at the beginning of your script:
  ```bash
    source /home/bas/applicationrc
  ```
  </p>
  <p>Once you added this line at the top of your script, you can access the directory in which your applicaiton is using `$APP_HOME`</p>
</div>


_* For more information about the syntax, you can check <a href="http://en.wikipedia.org/wiki/Cron">this page</a>_


