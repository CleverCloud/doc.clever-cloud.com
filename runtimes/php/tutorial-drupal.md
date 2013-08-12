---
title: Deploy a Drupal application
---

## Deploy a Drupal website

### Create an application

You can find in [this article](/clever-cloud-overview/add-application/#create-an-application) the process to create an application.


### Deploy via FTP

1. Download the Drupal source files on [drupal.org](http://drupal.org)
2. [Add a MySQL database](/databases-and-services/add-service/)
3. [Send these Drupal files via FTP](/clever-cloud-overview/add-application/#ftp-deployment) using the FTP credentials you received by email when you created the application.
4. When finished, launch the application with the url that you can find in the *domains* panel in the left sidebar.
5. Follow the Drupal installation steps
6. When asked for database informations, fill them with the ones you received by email:
	* database name
	* database username
	* database password
    * database host, for example `bj79c949bvl2deb6.mysql.clvrcld.net`
    * database port