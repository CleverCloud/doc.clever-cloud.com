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

# Managing Domain Names

When creating an application, you have two (non exclusive) choices for domain names:

* Using a personal domain name
* Using a ***.cleverapps.io** free domain, with built-in SSL

## Linking your Domain Name to your Application

You can link one or several domain names in the console.

Add it in the application configuration: in the console, click on your **application name** in the first panel, then choose **domain names**. You'll have to choose to add a custom domain name or use a sub-domain under ``*.cleverapps.io``

There is a default entry configured by default for every new app: ``yourAppID.cleverapps.io``.
In your application's domain section, just enter ``example.cleverapps.io``. You have to choose a unique one. Trusted SSL is available on every sub-domains.

If you need to configure a custom domain name, see the steps below.

## Personal Domain Names

### Your Application Runs in the Europe/Paris ('PAR') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>``domain.par.clever-cloud.com.``</td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records on ``62.210.112.171`` and ``62.210.92.244``</td>
  </tr>
</table>


### Your Application Runs in the North-America/Montreal ('MTL') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>``domain.mtl.clever-cloud.com.``</td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records on ``68.71.34.33`` and ``68.71.34.35``</td>
  </tr>
</table>


<br/>

You can point your domain name to clever cloud either with a CNAME record or with A records. **The use of a CNAME record is highly recommended.** With a CNAME record, your DNS configuration is always up-to-date. Using A records will require you to keep the DNS configuration up-to-date manually. Domain names linked to clever cloud applications are monitored, so we will send you an email if your DNS configuration is obsolete or incorrect.


<div class="panel panel-warning">
  <div class="panel-heading">
     <h4>Warning on CNAME Availability</h4>
  </div>
  <div class="panel-body">
    You cannot use a CNAME  on a top-level domain, or on a subdomain which already has DNS records.
  </div>
</div>

If you want to make your application available from a domain name which does not support CNAME records (eg example.com in addition to ``www.example.com``), check if your registrar provides a web redirection service. This way, you only have to make ``www.example.com`` point to Clever Cloud. Please note that web redirection provided by registrars only work over HTTP.

Remember that DNS changes may take time to propagate (usually a few hours, sometimes up to a day or more). It depends on the TTL setting of your DNS configuration. For faster changes, you can lower the TTL value in advance, and rise it again afterwards.

Before changing your domain name configuration, you can lower the TTL of your domain name one day before the migration (like 3min). Any changes on your DNS configuration will be applied faster. Then, when everything is configured properly, you can put the default TTL value.


<div class="panel panel-warning">
  <div class="panel-heading">
     <h4>Note on using a domain wildcard</h4>
  </div>
  <div class="panel-body">
    <div>
      ``*.example.com`` will match e.g. ``blog.example.com`` or ``www.example.com``.
    </div>
    <div> But for the raw domain ``example.com``, you will have to add both ``*.example.com`` and example.com to your application.</div>
  </div>
</div>
<br>
### Contextual Example

<table class="table table-striped">
  <thead>
    <tr>
      <th>Domain Name Use Case</th> <th>CNAME config</th> <th>record A config</th> <th>Web redirections</th>
    </tr>
  </thead>
  <tboby>
    <tr>
      <td>``www.example.com`` <br> ``example.com``</td>
      <td> point ``www.example.com`` to ``domain.par/mtl.clever-cloud.com.``</td>
      <td>No A record needed</td>
      <td>Redirect ``example.com`` to ``www.example.com`` </td>
    </tr>
    <tr>
      <td>``www.example.com``</td>
      <td> point ``www.example.com`` to ``domain.par/mtl.clever-cloud.com.``</td>
      <td>No A record needed</td>
      <td>No redirect needed</td>
    </tr>
    <tr>
      <td>``example.com``</td>
      <td>No CNAME record needed</td>
      <td>point ``example.com`` to the two IP adresses of the selected region</td>
      <td>No redirect needed</td>
    </tr>
  </tboby>
</table>
