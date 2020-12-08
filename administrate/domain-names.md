---
title: Domain names
position: 3
shortdesc: How to setup and configure domains names for your apps
tags:
- dashboard-setup
- apps
keywords:
- DNS
- domain
- URL
---

When creating an application, you have two (non exclusive) choices for domain names:

* Using a personal domain name
* Using a **\*.cleverapps.io** free domain, with built-in SSL

You can link one or several domain names in the console.

Add it in the application configuration: in the console, click on your **application name** in the first panel, then choose **domain names**. You'll have to choose to add a custom domain name or use a sub-domain under ``*.cleverapps.io``.

## Using a **\*.cleverapps.io** free domain, with built-in SSL

In the console, in the domain name sub menu of your application, there is a default entry configured by default for every new app: ``yourAppID.cleverapps.io``, which can be removed.
In your application's domain section, just enter ``example.cleverapps.io``. You have to choose a unique one. Trusted SSL is available on every sub-domain.


## Using Personal Domain Names

You can point your domain name to Clever Cloud either with a CNAME record or with A records. **The use of a CNAME record is highly recommended.**
With a CNAME record, your DNS configuration is always up-to-date.
Using A records will require you to keep the DNS configuration up-to-date manually.
Domain names linked to Clever Cloud applications are monitored, so we will send you an email if your DNS configuration is obsolete or incorrect.


### Your Application Runs in the Europe/Paris ('PAR') Zone

Provide the following to your registrar:

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.par.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Four records:<br>
    ``@ 10800 IN A 185.42.117.108``<br>
    ``@ 10800 IN A 185.42.117.109``<br>
    ``@ 10800 IN A 46.252.181.103``<br>
    ``@ 10800 IN A 46.252.181.104``
    </td>
  </tr>
</table>

### Your Application Runs in the Europe/Roubaix ('RBX') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.rbx.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records:<br>
    ``@ 10800 IN A 87.98.180.173``<br>
    ``@ 10800 IN A 87.98.182.136``
    </td>
  </tr>
</table>

### Your Application Runs in the Europe/Warsaw ('WSW') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.wsw.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records:<br>
    ``@ 10800 IN A 145.239.17.127``<br>
    ``@ 10800 IN A 145.239.17.192``
    </td>
  </tr>
</table>

### Your Application Runs in the North-America/Montreal ('MTL') Zone

Provide the following to your registrar:

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.mtl.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records:<br>
    ``@ 10800 IN A 149.56.147.232``<br>
    ``@ 10800 IN A 149.56.126.234``<br>
    </td>
  </tr>
</table>

### Your Application Runs in the Asia/Singapore ('SGP') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.sgp.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records:<br>
    ``@ 10800 IN A 51.79.197.159``<br>
    ``@ 10800 IN A 51.79.197.160``
    </td>
  </tr>
</table>

### Your Application Runs in the Oceania/Sydney ('SYD') Zone

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Record Type</center></th>
    <th><center>Value</center></th>
  </tr>
  <tr>
    <td>CNAME <div><span class="label label-success">Recommended</span></div></td>
    <td>
    ```
    {yoursubdomain} 10800 IN CNAME domain.syd.clever-cloud.com.
    ```
    </td>
  </tr>
  <tr>
    <td>A<div><small>Only if CNAME is not available</small></div></td>
    <td>Two records:<br>
    ``@ 10800 IN A 139.99.253.215``<br>
    ``@ 10800 IN A 139.99.253.237``
    </td>
  </tr>
</table>

{{< alert "warning" "Warning on CNAME Availability" >}}
    You cannot use a CNAME  on a top-level domain, or on a subdomain which already has DNS records.
{{< /alert >}}


If you want to make your application available from a domain name which does not support CNAME records (eg ``example.com`` in addition to ``www.example.com``), check if your registrar provides a web redirection service. This way, you only have to make ``www.example.com`` point to Clever Cloud. Please note that web redirection provided by registrars only work over HTTP.

Remember that DNS changes may take time to propagate (usually a few hours, sometimes up to a day or more). It depends on the TTL setting of your DNS configuration. For faster changes, you can lower the TTL value in advance, and rise it again afterwards.

{{< alert "info" "Note on using a domain wildcard" >}}
    <div>
      ``*.example.com`` will match e.g. ``blog.example.com`` or ``www.example.com``.
    </div>
    <div> But for the raw domain ``example.com``, you will have to add both ``*.example.com`` and ``example.com`` to your application.</div>
{{< /alert >}}
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
      <td>point ``example.com`` to the two IP addresses of the selected region</td>
      <td>No redirect needed</td>
    </tr>
  </tboby>
</table>

## Prefix routing

Requests are routed to applications based on the domain name, but you can also route based
on a path prefix.

For instance, you can bind `example.com/api` to an app, and `example.com` to another one.
All the HTTP requests on `example.com` where the path starts with `/api` will be routed to
the first app. The other requests will be routed to the second app. You can add a path after every domain name you bind in the console (or with [clever tools]({{< ref "getting-started/cli.md" >}})).

## Gandi CNAME configuration

Here is <a href="https://blog.clever-cloud.com/blog/features/2019/03/05/gandi-domain-on-clever-cloud/" target="_blank">an article that demonstrates a simple setup for Gandi CNAMEs</a>.