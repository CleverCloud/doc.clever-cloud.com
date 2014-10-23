---
title: Environment variables
position: 7
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

 * A special environment vaiable `INSTANCE_NUMBER` is added automatically for each application.

Please note that if you define or modify environment variables, you will
need to redeploy you application to make it use the variables.

### What is the `INSTANCE_NUMBER` varianle used for?

This variable allows your application to differentiate each running node on the applicative level.

It will contain a different number for each instance of your application.

For example, if three instances are running, it will contain `0` for the first, `1` for the second and `2` for the
third.

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

<table class="table table-bordered table-striped dataTable">
<tr>
<th>language</th>
<th>use case</th>
</tr>
<tr>
<td><a href="/nodejs/nodejs/#environment-injection">Node.js</a>  </td>
<td>process.env["MY_VAR"]</td>
</tr>
<tr>
<td><a href="/java/java-war/#environment-injection">Java WAR</a> </td>
<td>System.getProperties().getProperty("MY_VAR")</td>
</tr>
<tr>
<td><a href="/java/play-framework-1/#environment-injection">Play! Framework 1</a></td>
<td>System.getenv("MY\_VAR") or \${MY_VAR} in application.conf</td>
</tr>
<tr>
<td><a href="/java/play-framework-2/#environment-injection">Play! Framework 2</a></td>
<td>System.getenv("MY\_VAR") or \${MY_VAR} in application.conf</td>
</tr>
<tr>
<td><a href="/scala/scala/#environment-injection">Scala</a> </td>
<td>System.getenv("MY\_VAR")</td>
</tr>
<tr>
<td><a href="/php/php-apps/#environment-injection">PHP</a></td>
<td>getenv("MY_VAR")</td>
</tr>
<tr>
<td><a href="/python/python_apps/#environment-injection">Python</a></td>
<td>os.getenv("MY_VAR")</td>
</tr>
<tr>
<td><a href="/ruby/ruby/#environment-injection">Ruby</a></td>
<td>env[“my_var”]<br></td>
</tr>
</table>

Please note that the variables are available at build-time, for
runtimes that support build-time instructions, such as
<a href="/java/java-war/#environment-injection">Java WAR</a>,
<a href="/java/play-framework-1/#environment-injection">Play! Framework 1</a>,
<a href="/java/play-framework-2/#environment-injection">Play! Framework 2</a>
or <a href="/scala/scala/#environment-injection">Scala</a>.
