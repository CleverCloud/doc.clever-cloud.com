---
layout: page

id: php
parent: app_configuration
prev: java_and_scala
next: ruby
---
PHP
===

There are two ways to deploy your PHP app. The git way and the sftp one.

The git way
-----------

Use this method if you want to deploy you app once, or do not fear to lose all
sessions and other created files on each new deploy.

For security reason, the current git deployment restart an all new app. So if it
creates files during it's lifetime, the files will be deleted.

To deploy through git, just follow the [deployment tutorial](/app-deployment.html)

The sFTP way
------------

Use this way of doing if you need to redeploy your app on a regular way. To use
it, just push your files on the following url:


PHP modules
-----------

For now, a minimum of modules are installed. We will add them on demand. Do not
hesitate to contact us if you need a particular module.

