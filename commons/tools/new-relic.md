---
title: New Relic
position: 2
---

# New Relic

[New Relic](http://www.newrelic.com/) is a software analytics company based in San Francisco, California.
New Relic's technology monitors Web and mobile applications in real-time that run in cloud, on-premise, or hybrid
environments. You can easily setup a New Relic based monitoring on your application to monitor it and find
performances problems. 

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



