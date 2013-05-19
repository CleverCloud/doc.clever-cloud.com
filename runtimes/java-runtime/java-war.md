---
title: Deploying WAR/EAR
---

## Deploying WAR/EAR projects <span class="cc-alpha pull-right" title="Currently in Alpha version"></span>

The Clever Cloud offers you to run your WAR or EAR apps. You can deploy this kind of project without changing your code, but running it on Clever Cloud needs some configuration files, to add parameters like your targeted container for instance.

### Overview

In <abbr title="Java Enterprise Edition">J2EE</abbr> application modules are packaged as EAR and WAR based on their functionality.

* <acronym title="Web Archive">WAR</acronym>: Web modules which contains Servlet class files, JSP FIles, supporting files, GIF and HTML files are packaged as JAR file with *.war* extension.

* <acronym title="Enterprise Archive">EAR</acronym>: *.war* and *.jar* files are packaged as JAR file with .ear extension and deployed into Application Server. EAR file contains configuration such as application security role mapping, EJB reference mapping and context root url mapping of web modules.

<div class="alert alert-hot-problems">
	<h5>Note for Alpha Version</h5>
	<div>WAR and EAR apps are free during the Alpha period. No Drops wil be charged.</div>
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

### Getting started

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".<figure class="cc-content-imglarge">
  <img src="/assets/images/appjavawar.png"/></figure>
3. The next screen called "Choose an instance" and let you choose the instances types. Here, we select "Java with WAR/EAR":  <figure class="cc-content-imglarge"><img src="/assets/images/javawarapp.png"></figure>
4. The next screen called "App Creation Review" . This is the last step before creating your app. Click on **Create** to get your git URI  : <figure class="cc-content-imglarge"><img src="/assets/images/appcreationreviewjavawar.png"></figure>
5. *Optional*: If you need a database, go to your the dashboard of your personal account or of your organisation. Select your name or your organisation in the headbard. <figure class="cc-content-img">
  <a href="/assets/images/gotohome.png"><img src="/assets/images/gotohome.png"/></a>
  <figcaption>Use the headbar to head back to your dashboard. 
  </figcaption>
</figure>
6. Then, click on **Services** in the left tab, and choose a database. In our case, we will choose mySQL. Click on the mySQL button, and then on "Add service". Your credentials will be sent by email.<figure class="cc-content-imglarge"><img src="/assets/images/mysql.png"></figure>




### Configuration file

To deploy war and ear files on the Clever Cloud, you need to provide a `clevercloud/war.json` file describing the container you want and the archives you want to deploy.

The file must contain the following fields:

```haskell
{
  "deploy":{
    "container":<string>,
    "war" : [<string>]
    }
}
```

* **container**: that field should contain one of the values in the right column of the table below. Pick the "configuration value" in the corresponding cell of your container's version.

* **war** : This field is a list of strings. It should contain the paths of the
war/ear files relative to your application root.


<div class="alert alert-hot-problems">
	<h5>Tip</h5>Do not forget the double quotes
	around the "container"’s value, and the strings in the "war" list.
	The "war" value must be a list, so don't forget the brackets.
</div>

Availables onfiguration values for "container" of war.json:
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
         <td>JBOSS6</td>
         <td>Use JBoss AS 6.x
         (see <a href="http://www.jboss.org/jbossas">http://www.jboss.org/jbossas</a></td>
      </tr>
      <tr>
         <td>JBOSS7</td>
         <td>Use JBoss AS 7.x
         (see <a href="http://www.jboss.org/jbossas">http://www.jboss.org/jbossas</a>) </td>
      </tr>
      <tr>
         <td>RESIN3</td>
         <td>Use Resin AS 3.x (see <a
         href="http://www.caucho.com/resin-3.1/doc/">http://www.caucho.com/resin-3.1/doc/</a>)</td>
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
   </tbody>
</table>     

### Git Deployment
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



<script type="text/javascript">
$('#center a').click(function(){
    $('html, body').animate({
        scrollTop: $( $(this).attr('href') ).offset().top - 0
    }, 500);
    return false;
});
</script>
