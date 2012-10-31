---
layout: page

id: java_start
parent: getting_started
prev: php_start
---

# Tutorial : Run a Java app

1. Create a new app by clicking on the “New application” button, next to the “Home” button, in the top bar. 
2. Enter your application's name and description and click "Next".<img class="thumbnail img_doc" src="/img/appjavaname.png">
3. The next screen called "Choose an instance type" and let you choose the instances types. Here, we select "Java + Maven", and click  <img class="thumbnail img_doc" src="/img/java.png">
4. The next screen called "Advanced Configuration" allow us to do two things:<img class="thumbnail img_doc" src="/img/advancedconfjava.png">
	* Set up the domain name
	* And choose the deployment tool. 
	
Here we setup a sub-domain under **cleverapps.io** and a Git deployment.
5. Just before creating your app, the review page appear. Click on create to get your git or FTP crendentials.  
	Important : Git is not recomended for PHP applications. Configuration files could be created by some frameworks while they are in production, so Git could not allow you to retrieve them.
6. The next step is the database : click on "Services" and choose between "PostgreSQL" et "mySQL". In our case, we will choose mySQL. Click on the postgreSQL button, and then on "Add services". Your database credentials will be sent by email.<img class="thumbnail img_doc" src="/img/postgre.png">

