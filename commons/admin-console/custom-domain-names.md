---
title: Custom domain names
position: 2
---

# Custom domain names

When creating an application, you have two (non exclusive) choices for domain names:

* Using a ***.cleverapps.io** free domain
* Using a personal domain name

<figure class="cc-content-img"><img  src="/assets/images/domain1.png"></figure>
<figcaption>Example of domain names customization</figcaption>

## *.cleverapps.io domain

In your application's domain section, just enter ``example.cleverapps.io``.


## Personal domain name

You have two non exclusive choices to add custom personal domain names:

  * Domains like ``example.com``
  
    On your domain name provider, point the **A record** to ``62.210.121.146``

  * Sub-domains like ``hello.example.com``

    On your domain name provider, point the associated *hello* **CNAME record** to ``domain.clever-cloud.com``

    <a href="http://www.gandi.net" target="_blank">Gandi.net</a> example :
    <figure class="cc-content-img">
      <img  src="/assets/images/domain2.png">
    </figure>
    <figcaption>CNAME records example</figcaption>

<!-- -->
<br/>
<div class="alert alert-hot-problems">
  <h4>Reminder:</h4>
  <p>Remember that it may take up to a full day for DNS changes to propagate, so be patient.</p>
</div>
