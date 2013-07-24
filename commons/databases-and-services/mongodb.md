---
title: MongoDB
position: 5
---


## MongoDB <span class="cc-beta pull-right" title="Currently in Beta version"></span>

MongoDB is an open source NoSQL document-oriented database.


### Add a database

[This article](/databases-and-services/add-service/) describes the process to add a database on Clever Cloud.

### Connect to your database

#### Standard connection string format

You will receive your MongoDB credentials by email. The URI to connect it to your application is the following:  

``mongodb://db_username:db_password@db_host/db_name``


#### Command line connection

You can connect to your database like on your local machine with this prompt:

``mongo db_name -u db_username -p db_password --host db_host``


### MongoDB + Node.js sample application

Simple MongoDB + Node.js todo list: 
