---
title: Jar deployment
position: 3
shortdesc: Deploy a jar on Clever Cloud...
tags:
- java
---

# Deploy Jar files

Clever Cloud offers you to run any Java ARchive file. You do not
need to change your code, but running it on Clever Cloud needs some
configuration files, here is what you can do:

  * Commit and push a single jar file with all dependencies embedded;
  * Commit and push your sources, let Clever Cloud build them with maven or gradle, and run the resulting jar;

## Create an application

Refer to the page [Deploy an application on Clever Cloud](/doc/clever-cloud-overview/add-application/).

## Necessary information

You *must* provide a `clevercloud/jar.json` file (jar.json file in
clevercloud folder which is at the root of you application) that
contains at least the following:

```javascript
{
  "deploy": {
    "jarName": "path/to/jar"
  }
}
```

That is the least you need to do. Note that `path/to/jar` *must not*
start with a `/` and that the path should be relative to your
repository's root.

## Build options

You do not need to build and commit a jar. Instead you can push your
sources and build them with either gradle, ant or maven. Here is the
configuration you need to provide in the `clevercloud/jar.json` file:

```javascript
{
  "build": {
    "type": "maven"|"gradle"|"ant",
    "goal": "package"
  }
}
```

The `goal` field is what will be asked to the build command, like `mvn
package` for a `maven` build with the `package` goal.

An configuration example with a maven build that generates a jar with
embedded dependencies is:

```javascript
{
  "build": {
    "type": "maven",
    "goal": "package"
  },
  "deploy": {
    "jarName": "target/myproject-1.0-jar-with-dependencies.jar"
  }
}
```
## More configuration

The full configuration can look like the following:

```haskell
{
  "build": {
    "type": "<string>",
    "goal": "<string>"
  },
  "deploy": {
    "jarName": "<string>"
  },
  "hooks": {
     "postDeploy": "<string>"
  }
}
```
You can use the following properties:

* ``build``
    * ``"type"`` can be ``"maven"``, ``"gradle"`` or ``"ant"``.
    * ``"goal"`` is the target you want to use to build your project.
* ``deploy``
    * ``"goal"`` the goal/target and options you want to execute to deploy/run you project.
* ``hooks``
    * ``postDeploy`` execute a custom script after the deployment. Some frameworks or custom applications might require bootstrapping before the application may run.
You can achieve this by creating a custom script with your commands and adding the associated file name.

## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

For Java applications, the environment is injected in the
`System.getProperties()` object. So, to use a variable, you just do
`System.getProperties().getProperty("MY_VARIABLE")`.

For Groovy applications, just use the `System.getProperty("MY_VARIABLE")`.
