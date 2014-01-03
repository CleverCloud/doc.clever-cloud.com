---
title: New Relic
position: 1
---

## New Relic

### Overview

New Relic is a software analytics company based in San Francisco, California. New Relic's technology monitors Web and mobile applications in real-time that run in cloud, on-premise, or hybrid environments.
You can easily setup a NewRelic-based monitoring on your PHP apps. 

### Necessary information

For now, only PHP apps are supported. A support for every other tech will be added soon.
Also, before setting up your app, be sure to have a [New Relic Account](http://newrelic.com/).

### PHP Configuration  

To configure your New Relic, you have to create and add a `./clevercloud/newrelic.json` file in your project, with the following fields:

```javascript
{
  "license": "licenceID",
  "appname": "NameOfYourApp(optional)" 
}
```

Deploy your application on Clever Cloud with this file. A few minutes later, your application will begin sending data to New Relic.
Once we receive the data, your application will be listed on your dashboard.



