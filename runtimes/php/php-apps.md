---
title: PHP apps
---

## Deploying PHP apps

PHP is available on our platform with the version 5.3.23. You can use FTP or Git to deploy your applications.

### Overview

PHP is a widely-used general-purpose scripting language that is especially suited for Web development and can be embedded into HTML.

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavamaven.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "PHP":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. This is where you choose your deployment process. You can choose FTP or Git. FTP is only available for PHP apps.
5. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavamaven.png"></figure>
6. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
7. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>

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

#### Limitation

The change of the webroot will be rejected during the deployment if the target directory does not exist or is not a directory.

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

If you have a request about modules or if you have any question, feel free to ask on <a href="https://groups.google.com/forum/?fromgroups#!forum/clever-cloud-users" target="_blank">Clever Cloud user group</a>.

### FTP deployment
After you created your PHP app in the console and choosed FTP to deploy, your FTP credentials are sent by email within minutes.

1. Use a FTP client with FTPS compliance, able to negotiate using the AUTH TLS method.  
If your client does not handle this secure mode, the server will refuse the connection.
2. The credentials are like:  

```bash
host = ftpes://upload.clever-cloud.com/
username = userxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
password = ***************
```
<div class="alert alert-hot-problems">
  <h5>Important:</h5>
  <p>Unlike git, FTP deployment starts a instance **as soon as** you create it.</p>
  <p>So drops consumption will be effective immediately after your FTP credentials will be sent.</p>
</div> 


### Git deployment
*You will need git on your computer to deploy via this tool. Here is the official website of Git to get more informations&nbsp;: <a href="http://git-scm.com">git-scm.com</a>*

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following message:

<figure class="cc-content-imglarge">
  <img src="/assets/images/newgitapp.png"/></a>
</figure>

#### Setting up your remotes

1. The "Information" page of your app gives you your git deployment URL.  
It looks like this:  ``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``.  
Copy it in your clipboard.
2. On your computer, go into your application repository. 
If you didn't already track your app with git, start by typing:

    ```bash
    $ git init
    ```
3. Then, use the "git remote" command to add the deploy URL:

    ```bash
    $ git remote add <name> <your-git-deployment-url>
    ```

4. The last step is to push your application:

    ```bash
    $ git push <name> master
    ```


<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>The remote branch on Clever Cloud is <strong>ALWAYS</strong> master. If your local branch is not "master", use this syntax:</p>
  <pre>git push < name > yourbranch:master</pre>

</div>