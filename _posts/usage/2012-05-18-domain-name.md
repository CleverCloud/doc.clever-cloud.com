---
layout: page

title: Domain names
tags:
- Usage
---
# Domain names configuration

When creating an app, you have two choice for domain names:
* Using a **\*.cleverapps.io** sub-domain for testing purpose or a quick deploy.
* Using a custom domain name.

**Choosing a custom domain name require a DNS configuration on the domain's registrar.**

When creating your application, choose the 2nd field named *www.your-custom-domain.com* and enter your domain name. <br/>*(Note that sub-domains are supported)*<img class="thumbnail img_doc" src="/img/domain1.png">

Then you have to create a DNS record of type **CNAME** on heading on <pre>domain.clever-cloud.com.</pre> <img class="thumbnail img_doc" src="/img/domain2.png">
