## FS Buckets: file system with persistance <span class="cc-beta pull-right" title="Currently in Beta version"></span>

<div class="alert alert-hot-problems">
  <h5>Note for Beta Version</h5>
  <div>FS Buckets are free during the beta period. No credits will be charged.</div>
</div>

When you deploy an application on any PaaS, a new application is created, the previous is deleted. If your application generates data, for example if you let users upload pictures and you do not store it on external services like S3, you will loose data.

The Git deployment does not allow you to keep generated data files between deployments. To avoid the loss of your data, you have to mount a persistent filesystem. This is why we created File System Buckets.

You will be able to retrieve generated data between two deployments.

### Creating a FS Bucket

[This article](/addons/clever-cloud-addons/) describes the process to add a File System Bucket on Clever Cloud.


### Configuring your application

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
    "bucket" : "bucketId2",
    "folder" : "/myotherFolder",
    "apps"   : ["app_id_2"]
  }
]
```


It's a json array containing objects with two fields:

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
<td><span class="label label-important">Required</span></td>
<td>bucket</td>
<td>The bucket id you can find in the console. It begins with `bucket_`</td>
</tr>
<tr>
<td><span class="label label-important">Required</span></td>
<td>folder</td>
<td>The folder you want the bucket to be mounted in. Should start with `/`. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application or via */app/myFolder*</td>
</tr>
<tr>
<td class="cc-depusage"><span class="label label-inverse">Optional</span></td>
<td>apps</td>
<td>Whitelist of the applications allowed to mount this bucket. It's helpful if you need
to deploy a *preprod* app and a *prod* app using the exact same codebase but different
buckets</td>
</tr>
</tbody>
</table>

<div class="alert alert-hot-problems">
<h5>Important note about target folder</h5>
<p>
The folder must not exists in your repository (or it needs to be empty). Otherwise, the mount of your bucket will be ignored.
</p>
<p>
You can mount the same bucket in different folders, but they will share the same
content, so it's not the solution. You should prefer to mount the bucket in only one
folder and then manage multiple subfolders in it.
</p>
</div>

<div class="alert alert-hot-problems">
<h5>Other things you *must* know</h5>
<p>
You cannot mount two buckets in the same folder for the same app.
</p>
<p>
If you put the same "folder" value for two entries in the *buckets.json* array, **you better
make sure** that the "apps" fields make the two buckets mutually exclusive upon deployment!
</p>
</div>

### Accessing your data inside the FS Bucket

The "configuration" tab of your FS Bucket add-on displays the information you need to connect to
your bucket using FTP.
