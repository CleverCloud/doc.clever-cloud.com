---
title: Intro to Apps
position: 1
---

## Intro to Apps

The aim of this article is to help you to make the difference between these three entities: 

* Apps
* Services
* Add-ons

These are all billable elements.

### Defining Apps

First, apps are declared on the Console. Each kind of app have :

* A dedicated language / framework
* A way of deployment (FTP and / or Git)
* Specific resources consumption (RAM, CPU etc.), depending on the language or framework used
* An optional configuration file you've added to your project

If one of these elements is missing, Clever Cloud can't deploy your app properly (except the configuration file, optional in some cases).
Available languages are listed below: 

* Java [Play!Framework 1 & 2, Maven, War files… ]
* Node.js
* PHP 
* Python [Django]

Each app is located in a Clever Cloud organisation. If you haven't created an organisation, the default location of your app in Clever Cloud console is your personal organisation.  
Once you create an app, Clever Cloud gives you immediately the way to deploy your app: via Git (a URL is generated) or via FTP (credentials are sent to you via email).


### Clever Cloud's architecture

When an app is pushed via Git, the platform receive it. Then its look at the resources' requirements, and and initiate the creation of your new app in the platform, when its target environment is ready. Our log system retrieves every traces from your application, to stream it to the console. The console gives you the ability to check your apps status, and to pause or restart it.

### Analytic and consumption
An app is billed according to its consumption. Clever Cloud checks every ten minutes an apps consumption, and fire an event of consumption to your account.  
So you can monitor in real time through the console your apps consumptions the amount of credits you have consumed (drops). 
The charts in the homepage shows you how many Drops were consumed each days:<figure class="cc-content-imglarge"><img src="/assets/images/analytics.png"></figure>