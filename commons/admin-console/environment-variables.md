---
title: Environment variables
position: 6
---
## Environment variables

Clever Cloud allows you to define environment variables that will be
injected in your application's environment.

### How variables are defined?

Variables are defined on a per-application basis. They are defined by two ways:

 * By provisionned add-ons linked to applications. The variables names
   are listed in the add-on's information panel

 * By adding variables yourself in the "Environment variables" panel of
   your application.

Please note that if you define or modify environment variables, you will
need to redeploy you application to make it use the variables.

### Variable definition constraints

For the second way, please note that *spaces are not allowed* in the
name of the variables, and that this name is automatically uppercased.

So if you define a "foobar" variable, il will register is "FOOBAR", so
you don't have to think about it, and you always use uppercased
variables.

As written in the previous section: please note that if you define or modify
environment variables, you will need to redeploy you application to make it use the variables.

### How do I use these variables?

Depending on the type of your application, you will need to use
different ways. You can find the various ways in the specific instances
documentations.

 * <a href="/nodejs/nodejs/#environment-injection">Node.js</a> (TL;DR: process.env["MY_VAR"])
 * <a href="/java/java-war/#environment-injection">Java WAR</a> (TL;DR: System.getProperties().getProperty("MY_VAR"))
 * <a href="/java/play-framework-1/#environment-injection">Play!  * Framework 1</a> (TL;DR: System.getenv("MY\_VAR") or ${MY_VAR} in application.conf)
 * <a href="/java/play-framework-2/#environment-injection">Play! Framework 2</a> (TL;DR: System.getenv("MY\_VAR") or ${MY_VAR} in application.conf)
 * <a href="/java/sbt/#environment-injection">Play! Framework 2</a> (TL;DR: System.getenv("MY\_VAR") or ${MY_VAR} in application.conf)
 * <a href="/php/php-apps/#environment-injection">PHP</a> (TL;DR: getenv("MY_VAR"))
 * <a href="/python/python-apps/#environment-injection">Python</a> (TL;DR: os.getenv("MY_VAR"))
 * <a href="/ruby/ruby/#environment-injection">Ruby</a> (TL;DR: ENV["MY_VAR"])


