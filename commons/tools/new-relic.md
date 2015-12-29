---
title: New Relic
position: 2
tags:
- developer
---

# New Relic

[New Relic](http://www.newrelic.com/) is a software analytics company based in San Francisco, California.
New Relic's technology monitors Web and mobile applications in real-time that run in cloud, on-premise, or hybrid
environments. You can easily setup a New Relic based monitoring on your application to monitor it and find
performances problems.

New Relic can be used on Clever Cloud with Java, NodeJS, PHP, Ruby and Scala applications.

## New Relic for NodeJS and Ruby 

New Relic is very simple to install in a NodeJS or Ruby application as it is a simple dependency.
Install instructions for
[NodeJS](https://docs.newrelic.com/docs/agents/nodejs-agent/installation-configuration/installing-maintaining-nodejs)
and [Ruby](https://docs.newrelic.com/docs/agents/ruby-agent/installation-configuration/ruby-agent-installation)
are available in [the New Relic documentation](https://docs.newrelic.com/).

## New Relic for Java, Scala and PHP

To use New Relic in Java, Scala or Ruby instances, you need to configure it as it is provided directly inside the
instances.

### Necessary information

Before setting up your app, be sure to have a [New Relic Account](http://www.newrelic.com/).

### Configuration  

To configure your New Relic, you have to create and add a `./clevercloud/newrelic.json` file in your project, with the
following fields:

```javascript
{
  "license": "licenceID",
  "appname": "NameOfYourApp(optional)" 
}
```

Deploy your application on Clever Cloud with this file. A few minutes later, your application will begin sending data
to New Relic. Once we receive the data, your application will be listed on your dashboard.



