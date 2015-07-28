---
title: Environment variables
position: 8
---
## Environment variables

Clever Cloud allows you to define environment variables that will be
injected in your application's environment.

### How variables are defined?

Variables are defined on a per-application basis. They are defined by three ways:

 * By provisionned add-ons linked to applications. The variables names
   are listed in the add-on's information panel

 * By adding variables yourself in the "Environment variables" panel of
   your application.

 * Some special environment variables are added automatically for each application.

Please note that if you define or modify environment variables, you will
need to redeploy you application to make it use the variables.

### Special environment variables

Some variables are injected to the environment of your application when you deploy it, to inform you
about the current context of your application and about the application itself.

They can be accessed as classics environment variables and can be used in your application to give you
information about the current context of the application.

 * `APP_ID` : the ID of the application. Each application has a unique identifier used to identify it in our system.
  This ID is the same than the one you can find in the information section of your application.

 * `INSTANCE_ID` : the ID of the current instance of your application. It's unique for each instance of your application
 and change every time you deploy it.

 * `COMMIT_ID` : the commit ID used as a base to deploy your application. As we remove the `.git` directory before the
 deployment (to avoid security problems), it can be used to know which version of your application is running on
 the server.

 * `APP_HOME` : The absolute path of your application on the server. Can be used to create absolute link
 in you application (ex : ${APP_HOME}/foo/bar).

 * `INSTANCE_NUMBER`

#### What is the `INSTANCE_NUMBER` variable used for?

This variable allows your application to differentiate each running node on the applicative level.

It will contain a different number for each instance of your application.

For example, if three instances are running, it will contain `0` for the first, `1` for the second and `2` for the
third.

### Variable definition constraints

For the second way, please note that *spaces are not allowed* in the
name of the variables.

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
<td><a href="/java/play-framework-1/#environment-injection">Play! Framework 1</a>
& <a href="/java/play-framework-2/#environment-injection">Play! Framework 2</a></td>
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
