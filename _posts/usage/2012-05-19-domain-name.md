---
layout: page

title: Domain names
tags:
- Usage
---
# Domain names configuration

When creating an app, you have two choice for domain names:
* Using a **cleverapps.io** sub-domain for testing purpose or a quick deploy.
* Using a custom domain name.

**Choosing a custom domain name require a DNS configuration on the domain's registrar.**

## Custom domain name configuration

When creating your application, choose the 2nd field named *www.your-custom-domain.com* and enter your domain name. <br/>*(Note that sub-domains are supported)*<img class="thumbnail img_doc" src="/img/domain1.png">

### DNS Configuration

Then you have to create a DNS record for this custom domain name.

You can use a **CNAME** record pointing on <pre>domain.clever-cloud.com.</pre>

But if you have other records on the same level *(eg. your root domain)* you **MUST\*** use a **A** record pointing on <pre>62.210.121.146</pre>

<img class="thumbnail img_doc" src="/img/domain2.png">

*\* Please refer to the section 2.4 "CNAME record" of <a href="http://tools.ietf.org/html/rfc1912">RFC 1912</a>*
