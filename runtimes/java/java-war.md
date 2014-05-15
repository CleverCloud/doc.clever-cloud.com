---
title: Java War/Ear
position: 4
shortdesc: In <abbr title="Java Enterprise Edition">J2EE</abbr> applications modules are packaged as EAR and WAR based on their functionality.
---

## Deploy WAR/EAR projects <span class="cc-alpha pull-right" title="Currently in Alpha version"></span>

Clever Cloud allows you to run WAR or EAR applications. You can deploy these projects without changing your code. We just need a configuration file with your targeted container.

### Overview

In <abbr title="Java Enterprise Edition">J2EE</abbr> application modules are packaged as EAR and WAR based on their functionality.

* <acronym title="Web Archive">WAR</acronym>: Web modules which contains Servlet class files, JSP FIles, supporting files, GIF and HTML files are packaged as JAR file with *.war* extension.

* <acronym title="Enterprise Archive">EAR</acronym>: *.war* and *.jar* files are packaged as JAR file with .ear extension and deployed into Application Server. EAR file contains configuration such as application security role mapping, EJB reference mapping and context root url mapping of web modules.

<div class="alert alert-hot-problems">
  <h5>Note for Alpha Version</h5>
  <div>WAR and EAR apps are free during the Alpha period. No credits will be charged.</div>
</div>

### Available containers

The Clever Cloud supports many servlet containers.
The supported containers are listed below:

<table id="containers" class="table table-bordered table-striped">
  <thead>
    <tr>
      <th>Apache</th>
      <th>Jetty</th>
      <th>Jboss</th>
      <th>Glassfish</th>
      <th>Resin</th>
    </tr>
  </thead>
  <tbody>
    <tr><td>Apache Tomcat 4.1</td><td>Jetty 6.1</td><td>Jboss 6.1</td><td>Glassfish 3.1</td><td>Resin 3.1</td></tr>
    <tr><td>Apache Tomcat 5.5</td><td>Jetty 7.6</td><td>Jboss AS 7.1</td><td> </td><td> </td></tr>
    <tr><td>Apache Tomcat 6.0</td><td>Jetty 8.1</td><td> </td><td> </td><td> </td></tr>
    <tr><td>Apache Tomcat 7.0</td><td>Jetty 9.0</td><td> </td><td> </td><td> </td></tr>
  </tbody>
</table>


### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/screens/javawar/javawar_create.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/screens/javawar/javawar_create.png"/></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>

### Necessary information

* your application must be set to listen on **port 8080**

* you need to provide a `clevercloud/war.json` file describing the container you want and the archives you want to deploy:

    ```javascript
    {
      "deploy":{
         "container":"<string>",
         "war" : [
				{
					"file":"<string>",
					"context":"/<string>"
				}
			]
       }
    }
    ```

    * **container**: that field should contain one of the values in the left column of the table below.
    * **war** : this field is a list of objects. **file** is mandatory
    and should contain the path of the war/ear file relative to your
application root. **context** is optional, *should start with a /* and
is the context under which you want your war to be deployed. If your
file is an ear, you do not need the **context** field. If you do not
define it, the default context will be the name of the war without the
extension. To deploy a WAR to the root context, just use the "/"
context.

<br/>

* Optional but recommended: you can add an array of paths to GET in order to check that your application is responding:

	```javascript
	{
	  "deploy": {
	    "container":"<string>",
	    "war": ["<string>"],
	    "pingPaths": ["<string>"]
	  }
	}
	```

   **pingPaths**: each string of this array should begin with a "/". They will all be tested. If you provide this field and the array is not empty, the deployer
   will try to issue a HTTP GET request to each of the paths: The deploy will succeed if and only if *all* the paths return a `200 OK` response.
   If you do not provide this field, we will try to `GET /` until the container replies. So your application might not be fully deployed by the time we switch backend servers in the reverse proxy.

<br />

* Optional: you can add a postdeploy webhook by adding its path:

    ```javascript
    {
      "deploy": {
        "container":"<string>",
        "war" : ["<string>"]
       },
      "hooks": {
        "postDeploy": "pathtoyourscript"
      }
    }
    ```

    **postDeploy**: execute a custom script after the deploy. Some frameworks or custom applications might require bootstrapping before the application may run.
    You can achieve this by creating a custom script with your commands and adding the associated file name in `clevercloud/war.json`.

### Prebuild your application

In addition to the war paths in the `clevercloud/war.json` file, you can just push your application's code and build it with maven. In order to do that, juste add the *build* field in your `clevercloud/war.json`:

	```javascript
	{
	  "build": {
	    "type": "maven",
	    "goal": "package",
	  },
	  "deploy": { … }
	}
	```

   **goal**: the maven goal to execute. That will be appended to the "mvn" command.

### Available containers

Available configuration values for "container" of war.json:
<table class="table table-bordered table-stripped">
   <thead>
      <tr>
         <th>Value</th>
         <th>Description</th>
      </tr>
   </thead>
   <tbody>
      <tr>
         <td>GLASSFISH3</td>
         <td>Use Glassfish 3.x
         (see <a href="http://glassfish.java.net/">http://glassfish.java.net/</a>)</td>
      </tr>
      <tr>
         <td>GLASSFISH4</td>
         <td>Use Glassfish 4.x
         (see <a href="http://glassfish.java.net/">http://glassfish.java.net/</a>)</td>
      </tr>
      <tr>
         <td>JBOSS6</td>
         <td>Use JBoss AS 6.x
         (see <a href="http://www.jboss.org/jbossas">http://www.jboss.org/jbossas</a>)</td>
      </tr>
      <tr>
         <td>JBOSS7</td>
         <td>Use JBoss AS 7.x
         (see <a href="http://www.jboss.org/jbossas">http://www.jboss.org/jbossas</a>) </td>
      </tr>
      <tr>
         <td>RESIN3</td>
         <td>Use Resin AS 3.x (see <a href="http://www.caucho.com/resin-3.1/doc/">http://www.caucho.com/resin-3.1/doc/</a>)</td>
      </tr>
      <tr>
         <td>RESIN4</td>
         <td>Use Resin AS 4.x (see <a href="http://www.caucho.com/resin-4/doc/">http://www.caucho.com/resin-4/doc/</a>)</td>
      </tr>
      <tr>
         <td>JETTY6</td>
         <td>Use Jetty servlet container 6.x (see <a
         href="http://jetty.codehaus.org/jetty/">http://jetty.codehaus.org/jetty/</a>)</td>
      </tr>
      <tr>
         <td>JETTY7</td>
         <td>Use Jetty servlet container 7.x (see <a
         href="https://www.eclipse.org/jetty/">https://www.eclipse.org/jetty/</a>)</td>
      </tr>
      <tr>
         <td>JETTY8</td>
         <td>Use Jetty servlet container 8.x (see <a
         href="https://www.eclipse.org/jetty/">https://www.eclipse.org/jetty/</a>)</td>
      </tr>
      <tr>
         <td>JETTY9</td>
         <td>Use Jetty servlet container 9.x (see <a
         href="http://www.eclipse.org/jetty/documentation/current/">http://www.eclipse.org/jetty/documentation/current/</a>)</td>
      </tr>
      <tr>
         <td>TOMCAT4</td>
         <td>Use Tomcat servlet container 4.x (see <a
         href="https://tomcat.apache.org/">https://tomcat.apache.org/</a>)</td>
      </tr>
      <tr>
         <td>TOMCAT5</td>
         <td>Use Tomcat servlet container 5.x (see <a
         href="https://tomcat.apache.org/">https://tomcat.apache.org/</a>)</td>
      </tr>
      <tr>
         <td>TOMCAT6</td>
         <td>Use Tomcat servlet container 6.x (see <a
         href="https://tomcat.apache.org/">https://tomcat.apache.org/</a>)</td>
      </tr>
      <tr>
         <td>TOMCAT7</td>
         <td>Use Tomcat servlet container 7.x (see <a
         href="https://tomcat.apache.org/">https://tomcat.apache.org/</a>)</td>
      </tr>
      <tr>
         <td>TOMCAT8</td>
         <td>Use Tomcat servlet container 8.x (see <a
         href="https://tomcat.apache.org/">https://tomcat.apache.org/</a>)</td>
      </tr>
   </tbody>
</table>

### Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

For WAR/EAR applications, the environment is injected in the
`System.getProperties()` object. So, to use a variable, you just do
`System.getProperties().getProperty("MY_VARIABLE")`.

### Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these steps](/clever-cloud-overview/add-application/) to deploy your application.
