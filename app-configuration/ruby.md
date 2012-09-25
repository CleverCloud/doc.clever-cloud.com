---
layout: page

id: ruby
prev: php
next: jruby

---
Ruby apps
====

Deployment
----------

With this image, you can deploy application in pure Ruby, Rack or Rails. 
The detection of your app is made by the presence of certain files.  

* For Rails, the file 'config/application.rb'  
* For pure-Rack or Rack-based application, like Sinatra or Merb, the file 'config.ru' at the root of the project.  
* For the other projects, a 'init.rb' file  

We also parse the Gemfile to find and install the gems you use.
The file 'config.ru' must contain the following lines (example for Sinatra):

    require './start_file'  
    run Sinatra::Application

The apps are, for now, automatically set to development, so make sure you have created a development set if needed.

Database
--------

We recommand you to use MySQL or PostGreSQL.  

### Rails app

We automatically migrate your database. Nothing is needed.

### Non-Rails app

If you need a database, you must use ActiveRecord as ORM. We do not support DataMapper and such.   
We recommand you to use the [activerecord_sans_rails gem](https://github.com/davidcornu/activerecord_sans_rails).    
If you upload a Sinatra app, you can alternately use the [sinatra-activerecord gem](https://github.com/janko-m/sinatra-activerecord).    

Do not forget to create a file to access your database with the information we gave you in the admin panel and requiring it in the main page of your application.
