---
title: Apps Management
position: 1
---

## Apps Management

### Custom domain Names
When creating an app, you have two choices for domain names:

* Using a **cleverapps.io** sub-domain for testing purpose or a quick deploy.
* Or/and using a custom domain name.

**Choosing a custom domain name require a DNS configuration on the domain's registrar.**

After your app creation, go to the **Domain Names** section and enter your domain name. You should note that sub-domains are supported: 
<figure class="cc-content-imglarge"><img  src="/assets/images/domain1.png"></figure>

#### DNS Configuration

Then you have to create a DNS record for this custom domain name.

You can use a **CNAME** record pointing on <pre>domain.clever-cloud.com.</pre>

But if you have other records on the same level *(eg. your root domain)* you **MUST\*** use a **A** record pointing on <pre>62.210.121.146</pre>
<figure class="cc-content-imglarge">
  <img  src="/assets/images/domain2.png">
</figure>
*\* Please refer to the section 2.4 "CNAME record" of <a href="http://tools.ietf.org/html/rfc1912">RFC 1912</a>*


### Logs

The Clever Cloud console gives you the ability to get logs from your apps.

To retrive an app's logs, select your app in the headbar.
Then, click on the *Logs* tab in the left-pane:

<figure class="cc-content-img">
  <img style=" margin: auto; display: block" src="/assets/images/logs.png"/>
  <figcaption>
    App's left-pane.
  </figcaption>
</figure>