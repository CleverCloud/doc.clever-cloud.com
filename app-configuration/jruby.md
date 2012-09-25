---
layout: page

id: jruby
parent: app_configuration
prev: ruby

---

Jruby apps
=====

Deployment
----------

This image can deploy the same application than the previous one, but also those like Jruby On Rails, Rack For Ruby …  
The same file are use to detect the application, except for Rack.  
You can have the following files:  

"config.ru", "web.xml", "WEB-INF/config.ru", "WEB-INF/web.xml", "WEB-INF/*/config.ru", "WEB-INF/*/web.xml"  [Syntax of the file web.xml](https://github.com/jruby/jruby-rack#for-other-rack-applications)  
You can also have a web.xml for Rails applications. [Example](https://github.com/jruby/jruby-rack#for-rails).    

The files of the application are grouped in a .war with Warbler and then deployed on a Jetty server.

The apps are, for now, automatically set to development, so make sure you use the right app server.

Database
----------

We recommand you to use MySQL or PostGreSQL.

### Rails app

The gemfile of a Rails application using Jruby must contain the following:
{% highlight ruby %}
      gem 'jdbc-mysql'
      gem 'activerecord-jdbc-adapter'
      gem 'activerecord-jdbcmysql-adapter'
      gem 'jruby-openssl'
      gem 'jruby-rack'
      gem 'warbler'
      gem 'jetty'
{% endhighlight %}
  
We recommand you to use MySQL or PostGreSQL. You can find the syntax of the adapters [here](http://kenai.com/projects/activerecord-jdbc/pages/Home).
  
Then, regenerate a Gemfile.lock

      $ rm Gemfile.lock 
      $ jruby -S bundle install 

### Non-Rails app

The same instructions must be used than for a ruby application (see the [corresponding section](/app-configuration/ruby.html)).
