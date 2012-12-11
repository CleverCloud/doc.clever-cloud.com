---
layout: page

title: Run a PHP app
tags:
- PHP
---
# Run a PHP app: Video tour 
<p>
<iframe style="width:738px" height="415" src="http://www.youtube.com/embed/jMwhkO3x8KM" frameborder="0" allowfullscreen="allowfullscreen"> </iframe>  
</p>

# Tutorial: Run a PHP app

1. Create a new app by clicking on the "New application" button, next to the “Home” button, in the top bar. 
2. Enter your application's name and description and click "Next".<img class="thumbnail img_doc" src="/img/appphpname.png">
3. The next screen called "Choose an instance type" and let you choose the instances types. Here, we select "PHP", and click  <img class="thumbnail img_doc" src="/img/php.png">
4. The next screen called "Advanced Configuration" allow us to do two things:<img class="thumbnail img_doc" src="/img/advancedconfphp.png">
	* Set up the domain name (with **\*.cleverapps.io** or your custom domain, <a href="/domain-namel">more info here</a>). Here we setup a sub-domain under *cleverapps.io* and a FTP deployment.
	* And choose the deployment tool. **Important**: Git is not recommended for PHP applications. Configuration files could be created by some frameworks while they are in production, so Git could not allow you to retrieve them.

5. Just before creating your app, the review page appear. Click on create to get your FTP credentials.
6. The next step is the database: click on "Services" and choose between "PostgreSQL" and "mySQL". In our case, we will choose mySQL. Click on the mySQL button, and then on "Add services" to validate. Your database credentials will be sent by email. <img class="thumbnail img_doc" src="/img/mysql.png">
