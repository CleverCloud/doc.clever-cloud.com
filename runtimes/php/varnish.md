---
title: Use Varnish as HTTP Cache
shortdesc: This article shows you how enable Varnish on your application.
---

# Varnish

Varnish is a HTTP proxy-cache, which works as a reverse proxy between your application and the client.
Following rules defined by the user, Varnish will caches the data of an application to reduce the load.


## Enable Varnish for you appliaiton

Varnish is provided by default in PHP instances. To enable it, you just have to create a `varnish.vcl` file
in the `/clevercloud` folder.

This file describes how Varnish caches your applications and how it decides to return a cached resource or not.

***Warning***: the backend section of the `varnish.vcl` configuration file is not necessary as it is already handled
by Clever Cloud


## Varnish version / version migration

We use **Varnish 4**. If you already have a configuration file written for varnish 3, you can use
[varnish3to4](https://github.com/fgsch/varnish3to4) to convert it to varnsih 4. Dont forget to add `vcl 4.0;`
at the beginning of the config file.


## Example file for Wordpress

If your application is a Wordpress, you can use the following file to enable Varnish on your Wordpress.

We also recommend you to use a plugin such as **Varnish HTTP Purge** to automatically purge the Varnish cache when a post is
created/updated, a comment is posted, ...

```
vcl 4.0;

acl purge {
    "10.0.1.100";
    "10.0.1.101";
    "10.0.1.102";
    "10.0.1.103";
    "10.0.1.104";
}

sub vcl_recv {
  if (req.method == "PURGE") {
    if (!client.ip ~ purge) {
      return (synth(405, "Not allowed."));
    }
    return (hash);
  }
 
  if (req.url ~ "\.(gif|jpg|jpeg|swf|css|js|flv|mp3|mp4|pdf|ico|png)(\?.*|)$") {
    unset req.http.cookie;
    set req.url = regsub(req.url, "\?.*$", "");
  }

  if (req.url ~ "\?(utm_(campaign|medium|source|term)|adParams|client|cx|eid|fbid|feed|ref(id|src)?|v(er|iew))=") {
    set req.url = regsub(req.url, "\?.*$", "");
  }

  if (req.url ~ "wp-(login|admin)" || req.url ~ "preview=true" || req.url ~ "xmlrpc.php") {
    return (pass);
  }

  if (req.http.cookie) {
    if (req.http.cookie ~ "(wordpress_|wp-settings-)") {
      return(pass);
    } else {
      unset req.http.cookie;
    }
  }
}

sub vcl_backend_response {
  if ( (!(bereq.url ~ "(wp-(login|admin)|login)")) || (bereq.method == "GET") ) {
    unset beresp.http.set-cookie;
    set beresp.ttl = 1h;
  }

  if (bereq.url ~ "\.(gif|jpg|jpeg|swf|css|js|flv|mp3|mp4|pdf|ico|png)(\?.*|)$") {
    set beresp.ttl = 365d;
  }
}

sub vcl_deliver {
# multi-server webfarm? set a variable here so you can check
# the headers to see which frontend served the request
#   set resp.http.X-Server = "server-01";
   if (obj.hits > 0) {
     set resp.http.X-Cache = "HIT";
   } else {
     set resp.http.X-Cache = "MISS";
   }
}
sub vcl_hit {
  if (req.method == "PURGE") {
    return (synth(200, "OK"));
  }
}

sub vcl_miss {
  if (req.method == "PURGE") {
    return (synth(404, "Not cached"));
  }
}
```
