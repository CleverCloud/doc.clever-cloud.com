---
title: New Relic for Java
shortdesc: This section provides information about the installation process of New Relic for SBT/Play2 and WAR application on Clever Cloud
---

## New Relic

### Overview

New Relic is a software analytics company based in San Francisco, California. New Relic's technology monitors Web and mobile applications in real-time that run in cloud, on-premise, or hybrid environments.
You can easily setup a NewRelic-based monitoring on your SBT or WAR apps.

<div class="alert alert-hot-problems">
  <h5>New Relic support status</h5>
  <div>At this time, the New Relic support is only present on SBT/Play Framework2 for java&scala and WAR instances.</div>
</div>

### Necessary information

For now, only SBT and WAR apps are supported. Support for other instances will be released in the future.
Also, before setting up your app, be sure to have a [New Relic Account](http://newrelic.com/).

### New Relic Configuration

To configure your New Relic, you have to create and add a `./clevercloud/newrelic.json` file in your project, with the following fields:

```javascript
{
  "license": "licenceID",
  "appname": "NameOfYourApp(optional)"
}
```

Deploy your application on Clever Cloud with this file. A few minutes later, your application will begin sending data to New Relic.
Once we receive the data, your application will be listed on your dashboard.



