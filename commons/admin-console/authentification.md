---
title: Authentication
position: 1
---

# Authentication On Clever Cloud

The API of Clever Cloud uses oAuth 2 to perform authentication actions. This is the same for signup an login.  
There is two ways to signup to Clever Cloud: email or GitHub login.


## Email Authentication

This kind of auth requires a valid and non-temporary disposable e-mail, and a password having at least 6 characters.  

## Github Authentication

The GitHub signup allow you to create an account, or link your existing one to GitHub, in one click.
This process ask the following permsissions: 

* Read your Public Key
* Read User Repositories

The "repository permission" is used to deploy your GitHub apps directly to Clever Cloud, with a simple step.