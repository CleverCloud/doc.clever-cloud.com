---
title: Deploy a Wordpress application
---

## Deploy a Wordpress website

### Create an application

You can find in [this article](/clever-cloud-overview/add-application/#create-an-application) the process to create an application.


### Two choices for deployment

During the creation of a PHP application, it is asked if you want to deploy it via FTP or via Git. These two techniques have their own pros and cons but have the same end result.

### Deploy via FTP

1. Download the Wordpress source files on [wordpress.org](http://wordpress.org)
2. [Add a MySQL database](/databases-and-services/add-service/)
3. Rename the file `wp-config-sample.php` to `wp-config.php`
4. Replace in `wp-config.php` the host (like this: bj79c949bvl2deb6.mysql.clvrcld.net), database name, username and password that has been sent by email after the database creation.
3. [Send these Wordpress files via FTP](/clever-cloud-overview/add-application/#ftp-deployment) using the FTP credentials you received by email when you created the application.
6. When finished, you can launch the application with the url that you can find in the *domains* panel in the left sidebar.


### Deploy via Git

1. Download the Wordpress source files on [wordpress.org](http://wordpress.org)
2. [Add a MySQL database](/databases-and-services/add-service/)
3. Rename the file `wp-config-sample.php` to `wp-config.php`
4. Replace in `wp-config.php` the host (like this: bj79c949bvl2deb6.mysql.clvrcld.net), database name, username and password that has been sent by email after the database creation
5. As mentioned in this [article](/databases-and-services/fs-buckets/), with Git deployments, files that are uploaded by users must be persisted in a File System Bucket. In order to do so, [add a File Bucket](/databases-and-services/add-service/) via the console. You will then receive your bucket id.
6. At the root of your application, create a `clevercloud/buckets.json` file (create a `clevercloud` folder in which you create a `buckets.json` file).
7. Add the following lines in this file. Do not forget to replace bucketId by the bucketId you received by email:

    ```javascript
    [
      {
        "bucket" : "bucketId",
        "folder" : "/wp-content/uploads"
      }
    ]
    ```

5. Send these Wordpress files via Git. Read this [article](/clever-cloud-overview/add-application/#git-deployment) if you need more information about it.
6. When finished, you can launch the application with the url that you can find in the *domains* panel in the left sidebar.