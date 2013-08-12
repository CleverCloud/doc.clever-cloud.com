---
title: Deploy a Drupal website
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


### Deploy via Git

1. Download the Drupal source files on [drupal.org](http://drupal.org)
2. [Add a MySQL database](/databases-and-services/add-service/)
3. Open `.gitignore` file and delete `sites/*/settings*.php` line
4. Copy the file `sites/default/default.settings.php` to `sites/default/settings.php`
5. Open `sites/default/settings.php` and line 213, replace 

    ```php
    $databases = array();
    ```
    
    <br/>
    by
    
    ```php
    $databases = array (
      'default' => 
        array (
          'default' => 
          array (
            'database' => 'yourDatabaseName',
            'username' => 'yourDatabaseUsername',
            'password' => 'yourDatabasePassword',
            'host' => 'yourDatabaseHost(beh2n9c2pai0f98k.mysql.clvrcld.net)',
            'port' => '',
            'driver' => 'mysql',
            'prefix' => '',
          ),
        ),
    );
    ```
6. Replace the line `$drupal_hash_salt = ''` with `$drupal_hash_salt = 'yoursalt'`. You can generate salts with [this link](http://www.passwordtool.hu/).
5. As mentioned in this [article](/databases-and-services/fs-buckets/), with Git deployments, files that are uploaded by users must be persisted in a File System Bucket. In order to do so, [add a File Bucket](/databases-and-services/add-service/) via the console. You will then receive your bucket id.
6. At the root of your application, create a `clevercloud/buckets.json` file (create a `clevercloud` folder in which you create a `buckets.json` file).
7. Add the following lines in this file. Do not forget to replace bucketId by the bucketId you received by email:

    ```javascript
    [
      {
        "bucket" : "bucketId",
        "folder" : "/sites/default/files"
      }
    ]
    ```

5. Send these Drupal files via Git. Read this [article](/clever-cloud-overview/add-application/#git-deployment) if you need more information about it.
6. When finished, get the url that you can find in the *domains* panel in the left sidebar. Then open the following link:  

`http://yourapplication.cleverapps.io/install.php`  

Do not forget the **/install.php** page otherwise installation will not happen.
Follow the steps and you're done!
