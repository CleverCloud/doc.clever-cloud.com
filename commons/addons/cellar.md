---
title: Cellar add-on
position: 3
shortdesc: Cellar is a Amazon S3-compatible file storage system created and hosted by Clever Cloud.
tags:
- addons
keywords:
- S3
- amazon
- Storage
- file
- files
---

# Cellar: a S3-like object storage service

Cellar is S3-compatible online file storage web service. You can use it with
your favorite S3 client.

To manually manage the files, you can use [s3cmd](http://s3tools.org/s3cmd).
You can download a s3cmd configuration file from the add-on configuration
page.

## Clever Cloud Cellar plans

<table class="table table-bordered table-striped dataTable"><caption>Cellar storage plans</caption>
<tr>
<th>Storage</th>
<th>Price / GB / mo</th>
<th>Price / TB / mo</th>
</tr>
<tr>
<td>First 1 TB</td>
<td>€ 0.02</td>
<td>€ 20.48</td>
</tr>
<tr>
<td>Till 25 TB</td>
<td>0.015€</td>
<td>15.36€</td>
</tr>
<tr>
<td>Till 50 TB</td>
<td>0.01€</td>
<td>10.24€</td>
</tr>
</table>

<table class="table table-bordered table-striped dataTable"><caption>Cellar trafic usage plans</caption>
<tr>
<th>Traffic (outbound)</th>
<th>Price / GB / mo</th>
<th>Price / TB / mo</th>
</tr>
<tr>
<td>till 10TB </td>
<td>0.09€</td>
<td>92.16€</td>
</tr>
<tr>
<td>Till 40 TB</td>
<td>0.07€</td>
<td>71.68€</td>
</tr>
</table>

## Creating a bucket

In Cellar, files are stored in buckets. When you create a Cellar addon, no
bucket is created yet.

To create a bucket, you can use s3cmd:

```bash
    s3cmd mb s3://bucket-name
```
You can list files

```bash
    s3cmd ls s3://bucket-name
```

You can upload files (`--acl-public` makes the file readable by everyone).

```bash
    s3cmd put --acl-public image.jpg s3://bucket-name
```

<div class="panel panel-warning">
  <div class="panel-heading">
    <h4 class="panel-title">S3 signature algorithm</h4>
  </div>
  <div class="panel-body">
    Cellar doesn't support the `v4` signature algorithm from S3. Please make sure
    your client is configured to use the `v2` signature algorithm. The
    s3cmd configuration file provided on the add-on configuration page already has it.
  </div>
</div>