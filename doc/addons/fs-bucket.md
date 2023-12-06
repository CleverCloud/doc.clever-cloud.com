---
type: docs
title: File System Buckets
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
aliases:
- /doc/deploy/addon/fs-bucket
type: docs
---

When you deploy an application on Clever Cloud, like most PaaS, a new virtual machine is created, the previous one is deleted.
If your application generates data, for example if you let users upload pictures and you do not store it on external
services like S3, you will lose anything stored on the disk.

The Git deployment does not allow you to keep generated data files between deployments. To avoid the loss of your data,
you have to mount **a persistent filesystem**. This is why we created File System Buckets.

You will be able to retrieve generated data between two deployments.

**Notes:** 
- Back-ups are not included in the public cloud offer. You can still do them manually.
- FSBuckets are not available for Docker applications because of security concerns.

## Configuring your application

Buckets are configured using environment variables. Add the following to your application :

```
CC_FS_BUCKET=/some/empty/folder:bucket-xxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx-fsbucket.services.clever-cloud.com[:async]
```
_don't forget to replace the path of the mounted folder and the fs-bucket host with the targeted folder path (make sure the folder not exists) and your fs-bucket host_ 

Optionnally, you can add `:async` to the end of the environment variable.
This will make Clever Cloud mount the FS Bucket with the `async` option.
It will make the FS Bucket faster (sometimes 3× faster), but in case of a network issue it may cause file
corruption.

You can setup multiple buckets by appending a number at the end of the environment variable's name.
```
CC_FS_BUCKET=/some/empty/folder:fs_bucket_host
CC_FS_BUCKET_1=/some/otherempty/folder:fs_bucket_other_host:async
```

Note that the `async` parameter can be set per bucket.

## Configuring your application with buckets.json (@deprecated)

{{< callout type="warning" >}}
This method is deprecated, we strongly recommend that you use environment variables.
If you want to switch from this method to the environment variables, you need to remove the `buckets.json` file. Otherwise, the environment variables will be ignored.
Also, this method does not support the `async` parameter.
{{< /callout >}}

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
    "bucket_host" : "bucket-01234567-0123-0123-0123-012345678987-fsbucket.services.clever-cloud.com",
    "folder" : "/myotherFolder",
    "apps"   : ["app_id_2"]
  }
]
```
{{< callout type="info" >}}
You can find a pre-filled json object to copy in the dashboard of your FSBucket add-on, in the "Dashboard configuration" tab
{{< /callout >}}

It's a json array containing objects with at least two fields:

## Configuring your application with buckets.json (@deprecated)

{{< callout type="warning" >}}
This method is deprecated, we strongly recommend that you use environment variables.
If you want to switch from this method to the environment variables, you need to remove the `buckets.json` file. Otherwise, the environment variables will be ignored.
Also, this method does not support the `async` parameter.
{{< /callout >}}

To configure your application to use buckets, use the following markdown table:

Usage    | Field        | Description
---------|--------------|--------------------------------------------------------------
Required | bucket       | The bucket id you can find in the console. It begins with `bucket_`. This is for "old-style" buckets (created before the 7 December 2015)
Required | bucket_host  | The bucket host you can find in the console. It begins with `bucket-` and ends with `services.clever-cloud.com`. This is for "new-style" buckets.
Required | folder       | The folder you want the bucket to be mounted in. Should start with `/`. Using the example *myFolder*, you can access your bucket via the *myFolder* folder at the root of your application (which absolute path is available in the `APP_HOME` environment variable)
Optional | apps         | Whitelist of the applications allowed to mount this bucket. It's helpful if you need to deploy a *preprod* app and a *prod* app using the exact same codebase but different buckets

The folder must not exist in your repository (or it needs to be empty). Otherwise, the mount of your bucket will be ignored.
You can mount the same bucket in different folders, but they will share the same content, so it's not the solution. You should prefer to mount the bucket in only one folder and then manage multiple subfolders in it.

{{< callout type="warning" >}}
You cannot mount two buckets in the same folder for the same app. If you put the same "folder" value for two entries in your environment variables or the *buckets.json* array, **make sure** that the "apps" fields make the two buckets mutually exclusive upon deployment!
{{< /callout >}}

## Accessing your data inside the FS Bucket

### From your application

Your bucket is mounted at the configured path, starting from your application's
root folder.

If you want to use an absolute path, you can use the `APP_HOME` environment
variable, see [special environment variables]({{< ref "doc/develop/env-variables.md#special-environment-variables" >}})

### From the addon dashboard

The **File explorer** tab of the **addon dashboard** gives you access to your files
in the FS bucket.

### From your favorite FTP client

The **Addon information** tab of your FS Bucket add-on displays the information
you need to connect to your bucket using FTP.

