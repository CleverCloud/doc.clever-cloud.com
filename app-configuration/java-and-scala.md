---
layout: page

id: java_and_scala
prev: security 
parent: app_configuration
next: php
---

Java - Scala
============

Java jar
--------

To run jars, just upload them on our server, as well as the cc_conf.json with the field "jarName" containing the name of the main jar.

Play! Framework
---------------

### Play 2


If you use Play! Framework 2, you have nothing to do for now. Just upload your files, and we take care of everything.

J2EE
----

To install a J2EE application, just pack it into a .war and follow the instructions to deploy on a Glassfish or a Jetty, in the next sections.

Scala
-----

Simply upload your app on our server, and you are good to go.

Glassfish
---------

To deploy a .war on Glassfish, you only have to upload the .war. You can also
upload a maven project which will be built before it’s deployed. See the Maven
section for that purpose.

If you want to connect your application to a database or declare any other
resources, you need to provide a file called glassfish-resources.xml in the WEB-INF/
folder of your application (using maven, it’s in src/main/webapp/). In this directory, you can also find a file called persistence.xml.  

In that file, you need to prefix your resources by "java:app" to tell glassfish
that the resource is application-scoped.

A resource file looks like this:
{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE resources PUBLIC "-//GlassFish.org//DTD GlassFish Application Server 3.1 Resource Definitions//EN" "http://glassfish.org/dtds/glassfish-resources_1_5.dtd">
<resources>
<!-- JDBC connection pool definition example -->
<jdbc-connection-pool name="java:app/jdbc/test-pool" res-type="javax.sql.DataSource"
      datasource-classname="org.apache.derby.jdbc.ClientDataSource"
      pool-resize-quantity="1" max-pool-size="5" steady-pool-size="0"
      statement-timeout-in-seconds="60" >
	<property name="serverName" value="localhost" />
	<property name="portNumber" value="1527" />
	<property name="dataBaseName" value="sun-appserv-samples" />
	<property name="User" value="APP" />
	<property name="Password" value="APP" />
	<property name="connectionAttributes" value=";create=true" />
	<property name="driverType" value="4" />
</jdbc-connection-pool>
<jdbc-resource jndi-name="jdbc/test-ds" pool-name="java:app/jdbc/test-pool" />
<!-- JMS resource definition example -->
<admin-object-resource enabled="true" jndi-name="java:app/jms/exampleQueue" object-type="user" res-adapter="jmsra" res-type="javax.jms.Queue">
	<description>Useful for mailing new user</description>
	<property name="Name" value="new_examplequeue_repo"/>
</admin-object-resource>
<connector-resource enabled="true" jndi-name="jms/example" object-type="user" pool-name="jms/example">
</connector-resource>
<connector-connection-pool associate-with-thread="false" connection-creation-retry-attempts="0"
       connection-creation-retry-interval-in-seconds="10" connection-definition-name="javax.jms.ConnectionFactory"
       connection-leak-reclaim="false" connection-leak-timeout-in-seconds="0" 
       fail-all-connections="false" idle-timeout-in-seconds="300"
       is-connection-validation-required="false" lazy-connection-association="false" 
       lazy-connection-enlistment="false" match-connections="true" max-connection-usage-count="0" 
       max-pool-size="32" max-wait-time-in-millis="60000"
       name="jms/example" pool-resize-quantity="2" resource-adapter-name="jmsra" steady-pool-size="8"
       validate-atmost-once-period-in-seconds="0"/>
</resources>
{% endhighlight %}
The fields "serverName", "User" and "Password" have to be filled in with the value we will give you in the admin panel when you’ll ask for a database.

Jetty
-----

If you choose to run your application on Jetty, you just have to upload a .war on our server. Alternatively, you can upload a Maven project which will be built before deploy. See the maven section for further informations.  
If you want to use a database, you have to fill the web.xml file. We support c3p0 and dbcp method. Fore more informations, take a look at the [Jetty doc](http://docs.codehaus.org/display/JETTY/DataSource+Examples).

Maven-Ant
----------

At the root of your project, you must add the cc_conf.json with the "goal" field filled. It contains a string with all the goals and parameters that must be executed, separated by a space. The build field contain "maven" or "ant". 

Sbt
---

To build your project using SBT, upload your files on our server with the cc_conf.json filled with 
    
    "build": "sbt"
  
(see the [deployment configuration section](/app-configuration/cc-conf.html) to read about the json.).  
You also can deploy via SBT, without nothing to do.  
If you build via SBT, the "package" command will be applied. If you deploy, the "run" command will.
