---
title: New Relic
position: 2
shortdesc: Configuring Newrelic on Clever Cloud
tags:
- apps
keywords:
- new relic
- newrelic
- metrics
- monitoring
---

## Overview

New Relic can be used on Clever Cloud with **Java, Node.js, PHP, Ruby and Scala** applications.

[New Relic](https://www.newrelic.com/) is a software analytics company based in the United States.
New Relic's technology monitors Web and mobile applications in real-time that run in cloud, on-premise, or hybrid environments. You can easily setup a New Relic based monitoring on your application to monitor it and find performances problems.

## New Relic for Node.js and Ruby

New Relic is very simple to install in a Node.js or Ruby application as it is a simple dependency.
Install instructions for [Node.js](https://docs.newrelic.com/docs/agents/nodejs-agent/installation-configuration/installing-maintaining-nodejs) and [Ruby](https://docs.newrelic.com/docs/agents/ruby-agent/installation-configuration/ruby-agent-installation) are available in [the New Relic documentation](https://docs.newrelic.com/).

## New Relic for Python

To use New Relic in Python, you have to add the [newrelic](https://pypi.python.org/pypi/newrelic) dependency into your `requirements.txt` file.


## New Relic for Java, Scala and PHP

To use New Relic in Java, Scala or PHP instances, you need to configure it as it is provided directly inside the instances.


## New Relic configuration

### Necessary information

Before setting up your app, be sure to have a [New Relic Account](https://www.newrelic.com/).

### Configuration

To configure your New Relic, you need to set the environment variables `NEW_RELIC_LICENSE_KEY` and `NEW_RELIC_APP_NAME` (optional).

Alternatively you can create and add a `./clevercloud/newrelic.json` file in your project, with the following fields:

```json
{
  "license": "licenceID",
  "appname": "NameOfYourApp(optional)"
}
```

If the `appname` or `NEW_RELIC_APP_NAME` is not specified, we use your application id for the name.

### Optional configuration (PHP only)

If you need to fine-tune agent settings, you can use the following environment variables:

[CC_NEWRELIC_BROWSER_MONITORING_AUTO_INSTRUMENT](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-autorum)  
[CC_NEWRELIC_DISTRIBUTED_TRACING_ENABLED](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-distributed-enabled)  
[CC_NEWRELIC_ERROR_COLLECTOR_ENABLED](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-err-enabled)  
[CC_NEWRELIC_TRANSACTION_TRACER_ENABLED](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-tt-enable)  
[CC_NEWRELIC_TRANSACTION_TRACER_RECORD_SQL](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-tt-sql)  

You can add other configuration inside your [.user.ini](https://www.clever-cloud.com/doc/deploy/application/php/php-apps/#change-php-settings). 
All the configuration settings are [available here](https://docs.newrelic.com/docs/apm/agents/php-agent/configuration/php-agent-configuration/#inivar-tt-settings)

### Usage

Just redeploy your application on Clever Cloud for the changes to take effect. A few minutes later, your application will begin sending data to New Relic. Once New Relic receives the data, your application will be listed in the dashboard.



