---
title: FS Buckets add-on
title: FS Buckets add-on
position: 4
shortdesc: This add-on allows applications to use a persistent file system, as git-based apps don't have one.
tags:
- addons
keywords:
- nfs
- file system
- buckets
- fs buckets
- storage
---

# FS Buckets: file system with persistence <span class="cc-beta pull-right" title="Currently in Beta version"></span>

When you deploy an application on any PaaS, a new virtual machine is created, the previous one is deleted.
If your application generates data, for example if you let users upload pictures and you do not store it on external
services like S3, you will lose anything stored on the disk.

The Git deployment does not allow you to keep generated data files between deployments. To avoid the loss of your data,
you have to mount a persistent filesystem. This is why we created File System Buckets.

You will be able to retrieve generated data between two deployments.

## FS Buckets plans

<table class="table table-bordered table-striped dataTable"><caption>FS Buckets pricing plans</caption>
<tr>
<th>Name</th>
<th>Disk</th>
<th>Mounts</th>
<th>Price /Go</th>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">DEV</span></td>
<td>100 MB</td>
<td>UNLIMITED</td>
<td>Free</td>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">S</span></td>
<td>UNLIMITED</td>
<td>UNLIMITED</td>
<td>1.50€</td>
</tr>
</table>

## Configuring your application

To configure your application to use buckets, use the
`clevercloud/buckets.json` file.

The `clevercloud` folder must be located at the root of your application.

The `buckets.json` file must contain the following structure:

```javascript
[
  {
    "bucket" : "bucketId",
    "folder" : "/myFolder",
    "apps"   : ["app_id"]

  },
  {
    "bucket_host" : "bucket-c65762b6-4086-4c99-84b0-23eb85695809-fsbucket.services.clever-cloud.com",
    "folder" : "/myotherFolder",
    "apps"   : ["app_id_2"]
  }
]
```

<div class="panel panel-info">
  <div class="panel-heading">
    <h4 class="panel-title">Getting a pre-filled conf file.</h4>
  </div>
  <div class="panel-body">
    You can find a pre-filled json object to copy in the dashboard of your FSBucket add-on, in the "Dashboard configuration" tab
  </div>
</div>

It's a json array containing objects with at least two fields:

<table id="nodedeps" class="table table-bordered table-striped">
<thead>
<tr>
<th>Usage</th>
<th>Field</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr>
<td><span class="label label-danger">Required</span></td>
<td>bucket</td>
<td>The bucket id you can find in the console. It begins with `bucket_`. This is for
"old-style" buckets (created before the 7 december 2015)</td>
</tr>
<tr>
<td><span class="label label-danger">Required</span></td>
<td>bucket_host</td>
<td>The bucket host you can find in the console. It begins with `bucket-` and ends with
`services.clever-cloud.com`. This is for "new-style" buckets.</td>
</tr>
<tr>
<td><span class="label label-danger">Required</span></td>
<td>folder</td>
<td>The folder you want the bucket to be mounted in. Should start with `/`. Using the example
*myFolder*, you can access your bucket via the *myFolder* folder at
the root of your application (which absolute path is available in the `APP_HOME` environment variable)</td>
</tr>
<tr>
<td class="cc-depusage"><span class="label label-default">Optional</span></td>
<td>apps</td>
<td>Whitelist of the applications allowed to mount this bucket. It's helpful if you need
to deploy a *preprod* app and a *prod* app using the exact same codebase but different
buckets</td>
</tr>
</tbody>
</table>

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4 class="panel-title">Important note about target folder</h4>
  </div>
  <div class="panel-body">
    <p>
    The folder must not exist in your repository (or it needs to be empty). Otherwise, the mount of your bucket will be ignored.
    </p>
    <p>
    You can mount the same bucket in different folders, but they will share the same
    content, so it's not the solution. You should prefer to mount the bucket in only one
    folder and then manage multiple subfolders in it.
    </p>
  </div>
</div>

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4 class="panel-title">Important notes</h4>
  </div>
  <div class="panel-body">
    <p>
    You cannot mount two buckets in the same folder for the same app.
    </p>
    <p>
    If you put the same "folder" value for two entries in the *buckets.json* array, **you better
    make sure** that the "apps" fields make the two buckets mutually exclusive upon deployment!
    </p>
  </div>
</div>

## Accessing your data inside the FS Bucket

### From the addon dashboard

The "File explorer" tab of the addon dashboard gives you access to your files
in the FS bucket.

### From your favourite FTP client

The "Addon information" tab of your FS Bucket add-on displays the information
you need to connect to your bucket using FTP.
