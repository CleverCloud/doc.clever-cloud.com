---
title: Custom domain names
position: 3
shortdesc: How to setup and configure domains names for your apps
tags:
- dashboard-setup
- developer
keywords:
- DNS
- domain
- URL
---

# Managing domain names

When creating an application, you have two (non exclusive) choices for domain names:

* Using a personal domain name
* Using a ***.cleverapps.io** free domain

<figure class="cc-content-img"><img  src="/doc/assets/images/domain1.png"></figure>
<figcaption>Example of domain names customization</figcaption>

## Personal domain name


<div class="alert alert-hot-problems">
<h5>Important: chosing between A and CNAME</h5>
<p>
The preferred way to point your domain name to Clever Cloud is through the use of a CNAME record. Please review the settings below before making changes in production. 
</p>
<p>
If you want to redirect your top-level domain name to a subdomain through HTTP (eg ``domain.com`` to ``www.domain.com``), most domain name registrars provide a web redirection service.
</p>
</div>


### Your application runs in the Europe ('PAR') zone

You have two non exclusive choices to add custom personal domain names:

  * Sub-domains like ``hello.example.com`` and wildcard sub-domains like ``*.example.com``

    On your domain name provider, point the associated *hello* **CNAME record** to ``domain.par.clever-cloud.com``

    <a href="http://www.gandi.net" target="_blank">Gandi.net</a> example :
    <figure class="cc-content-img">
      <img  src="/doc/assets/images/domain2.png">
    </figure>
    <figcaption>CNAME records example</figcaption>

  * Domains like ``example.com``

    On your domain name provider, point two **A records** to ``62.210.112.171`` and ``62.210.92.244``


### Your application runs in the North-America ('MTL') zone

You have two non exclusive choices to add custom personal domain names:

  * Sub-domains like ``hello.example.com`` and wildcard sub-domains like ``*.example.com``

    On your domain name provider, point the associated *hello* **CNAME record** to ``domain.mtl.clever-cloud.com``

    <a href="http://www.gandi.net" target="_blank">Gandi.net</a> example :
    <figure class="cc-content-img">
      <img  src="/doc/assets/images/domain2.png">
    </figure>
    <figcaption>CNAME records example</figcaption>

  * Domains like ``example.com``

    On your domain name provider, point two **A records** to ``68.71.34.21`` and ``68.71.34.20``


<br/>
<div class="alert alert-hot-problems">
  <h4>Reminder:</h4>
  <p>Remember that it may take up to a full day for DNS changes to propagate, so be patient.</p>
  <h4>Using a domain wildcard:</h4>
  <p><strong>*.example.com</strong> will match e.g. <strong>blog.example.com</strong> or <strong>www.example.com</strong>.
  </p>
  <p> But for the raw domain <strong>example.com</strong>, you will have to add
both <strong>*.example.com</strong> and example.com to your application.</p>
</div>


## *.cleverapps.io domain

In your application's domain section, just enter ``example.cleverapps.io``.
