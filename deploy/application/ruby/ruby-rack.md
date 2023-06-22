---
title: Deploy Ruby and Rack-based applications
shortdesc: How to deploy a web application using Rack on Clever Cloud.
tags:
- deploy
keywords:
- ruby
- rack
str_replace_dict:
  "@application-type@": "Ruby"
---

## Overview

Currently, Clever Cloud supports Rack-based applications.
Created in 2007, Rack has become the de-facto standard for ruby web applications and is used in many frameworks such as Ruby on Rails.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

## Configure your Ruby and Rake application

### Mandatory configuration

Be sure that:

* Bundler is installed locally (`gem install bundler`)
* you have a `config.ru` file
* Git (for the deploy part)
* you have a `gems.rb` or `Gemfile` containing your dependencies

### Tutorial and sample app

You can find an hello world tutorial of a Ruby and Rack application [here]({{< ref "/deploy/application/ruby/tutorials/ruby-rack-app-tutorial.md" >}}) and find the source code of the demo [here](https://helloworld-rack-demo.cleverapps.io).

{{< readfile "/content/partials/new-relic.md" >}}

{{< readfile "/content/partials/env-injection.md" >}}

To access environment variables from your code, just get them from the environment with `ENV["MY_VARIABLE"]`.

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}