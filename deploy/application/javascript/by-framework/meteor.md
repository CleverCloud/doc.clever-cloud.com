---
title: Deploy Meteor.js applications
shortdesc: Meteor is an open source platform for web, mobile, and desktop.
tags:
- deploy
keywords:
- meteor
- meteorjs
- nodejs
str_replace_dict:
  "@application-type@": "Meteor.js"
---

## Overview

Clever Cloud allows you to deploy any Meteor.js application. This page will explain how to set up your application to run it on our services.

Be sure your `.meteor` folder is in your git repository.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}


## Configure your Meteor.js based application
### Mandatory configuration

#### Set up environment variables

To run your Meteor.js application you need a few mandatory [environment variables](#setting-up-environment-variables-on-clever-cloud):

* `MONGO_URL`: this is the mongo uri you can find in your mongodb's dashboard, it has to start with `mongodb://`
* `ROOT_URL`: this is your application's root url. It has to start with `http://` or `https://`
* `PORT`: keep the current value of 8080

##### Optional Meteor settings as environment variable

Some application require a `settings.json` file to [store api keys and other various private and public Meteor variables](https://docs.meteor.com/api/core.html#Meteor-settings).
You can declare a `METEOR_SETTINGS` environment variable and past the content of the json file as the value of this key.

## Specify required Node.js version

Since building with the latest version of Node.js might fail, you should specify in the `package.json` file of the application the version needed for your Meteor application ; as shown in [Deploying Node.js apps](https://www.clever-cloud.com/doc/nodejs/nodejs/#requirements) documentation.
To find out which version is required by your application type the following command `meteor node -v` inside root folder.

Currently with Meteor *2.4* the Node.js version is 14.17.6** so you should add the following inside the `package.json` file.

```
  ...
  "engines": {
    "node": "14.17.6"
  },
  ...
```
You may change the node version value according to the application requirements.



#### Activate the dedicated build

Meteor.js uses a lot of memory during the build. You most likely will need to activate the [dedicated build feature](https://www.clever-cloud.com/doc/admin-console/apps-management/#dedicated-build) and set your vertical scaling size at least to **M**.

If your build still fails after you enabled the feature, please contact us on the support so we can set you a bigger scaler.

### Automatic HTTPS redirection

You can use the [force-ssl](https://atmospherejs.com/meteor/force-ssl) atmosphere package to redirect your users to the HTTPS version of your application.

### Custom start parameters

If you want to set custom start parameters, the main entry point of built meteor applications is: `.build/bundle/main.js`.
Add in the `scripts.start` field of the package.json: `node .build/bundle/main.js <parameters>`

### Deployment process on Clever Cloud

At each deployment, the needed Meteor.js version will be read from `.meteor/release` and installed. 
Your application will then be built using `meteor build --server-only` and deployed from the files created by this command.


{{< readfile "/content/partials/env-injection.md" >}}

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

### Oplog Tailing
Oplog tailing is supported on dedicated databases (starting from Hazelnut size). Contact the support to enable oplog. Once enabled, set the following [environment variable](#setting-up-environment-variables-on-clever-cloud):
- `MONGO_OPLOG_URL`: `mongodb://[…]mongodb.services.clever-cloud.com:{DB_PORT}/local?authSource={DB_NAME}`

## Migrate from a Node.js instance

If you want to migrate from your classic node.js app to a meteor application, contact our support in the console or at
support@clever-cloud.com with the application id.

{{< readfile "/content/partials/more-config.md" >}}
