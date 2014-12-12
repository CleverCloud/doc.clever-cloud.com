---
title: Deploy Docker images
shortdesc: Docker is an easy, lightweight virtualized environment for portable applications.
---

## Deploy Docker images

Clever Cloud allows you to deploy any application running inside a
Docker process. This page will explain you how to set up your application
to run it on our service.

<div class="alert alert-hot-problems">
<h5>Note for Beta Version</h5>
<div>
Docker support is still in beta. It does not yet support FS
Buckets, validation of your Dockerfile, or other advanced features.
</div>
</div>

### Overview

Docker is an easy, lightweight virtualized environment for portable
applications.

Docker containers can encapsulate any payload, and will run consistently
on and between virtually any server. The same container that a developer
builds and tests on a laptop will run at scale, in production*, on VMs,
bare-metal servers, public instances, or combinations of the above.

### Create an application

1. Create a new app by clicking on the **Add an Appplication** button, in the sidebar. 
2. Select a brand new instance (or a repository from GitHub if your account is linked).
3. Then select Docker the language list.
4. Configure your sclaing options.
5. Enter your application's name and description and click "Next". You can also select the region you want (US or Europe)
<figure class="cc-content-img">
  <img src="/assets/images/choose-name.png"/>
</figure>


### Requirements

Clever Cloud do not need a lot of requirements here are what you *need*
to do to ensure that your application will run:

* Push on the **master branch**.

* Commit a file named **Dockerfile**, [Here is what it will look like](http://docs.docker.io/introduction/working-with-docker/#working-with-the-dockerfile "Dockerfile")

* Listen on **port 8080**.

### Dockerfile contents

You can virtually put everything you want in your Dockerfile. The only
mandatory (for us) instruction to put in it is:

```bash
CMD <command to run>
```

   * **command to run**: this is the command that starts your
   application. Your application **must** listen on port 8080. It can be
   easier for you to put a script in your docker image and call it with
   the CMD instruction.


### Sample app

We forked the [**rails-meets-docker**](https://github.com/gemnasium/rails-meet-docker) demonstration
repository by [Gemnasium](https://gemnasium.com/) and adapted it so it
would run on Clever Cloud. If you want more information about this
example, we invite you to read their [well-written blog post](http://blog.gemnasium.com/post/66356385701/your-dockerfile-for-rails "Dockerfile for rails blog post")
about that repository.

You can find our demonstration application [on Github](https://github.com/clevercloud/rails-meet-docker-and-clever-cloud).
Fell free to clone it and play with it.

So, what's in this repo?

* Dockerfile

* rails/

* scripts/setup

* scripts/start

The *setup* and *start* scripts will be injected in the docker image
(see the Dockerfile below). The *rails* folder contains the rails
application that will be started. It's injected into the docker image
too.

The Dockerfile contains the following:

```bash
# -*- sh -*-
FROM fcat/ubuntu-universe:12.04

# development tools
RUN apt-get -qy install git vim tmux

# ruby 1.9.3 and build dependencies
RUN apt-get -qy install ruby1.9.1 ruby1.9.1-dev build-essential
libpq-dev libv8-dev libsqlite3-dev

# bundler
RUN gem install bundler

# create a "rails" user
# the Rails application will live in the /rails directory
RUN adduser --disabled-password --home=/rails --gecos "" rails

# copy the Rails app
# we assume we have cloned the "docrails" repository locally
#  and it is clean; see the "prepare" script
ADD rails /rails

# Fix ownership to avoid permissions problems.
RUN chown rails -R /rails

# copy and execute the setup script
# this will run bundler, setup the database, etc.
ADD scripts/setup /setup
RUN su rails -c /setup

# copy the start script
ADD scripts/start /start

# What's following is about the "docker run" command.
# Define the user that will do the next instructions
USER rails
# This defines a default command to run if someone executes
# `docker run my_image` without specifying any command.
# This is mandatory to make your app run on Clever Cloud.
CMD /start
```

