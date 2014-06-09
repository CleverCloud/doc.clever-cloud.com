---
title: Clever Cloud Add-ons
position: 3
---

# Clever Cloud Add-ons

Clever Cloud provides internally the following add-ons:

* MySQL
* PostgreSQL
* Couchbase
* MongoDB
* FS bukets


## MySQL plans

<table class="table table-bordered table-striped dataTable"><caption>PostgreSQL pricing plans</caption>
  <tr>
    <th>Name</th>
    <th>Cache (Memory)</th>
    <th>Disk</th>
    <th>Conn. limit</th>
    <th>Price /mo</th>
  </tr> 
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">S</span></td>
    <td>SHARED</td>
    <td>256 MB</td>
    <td>2</td>
    <td>Free</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">M</span></td>
    <td>1 GB</td>
    <td>100 GB</td>
    <td>75</td>
    <td>30 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">L</span></td>
    <td>8 GB</td>
    <td>450 GB</td>
    <td>500</td>
    <td>240 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">XL</span></td>
    <td>32 GB</td>
    <td>600 GB</td>
    <td>750</td>
    <td>700 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">XXL</span></td>
    <td>64 GB</td>
    <td>900 GB</td>
    <td>1000</td>
    <td>1200 €</td>
  </tr>
</table>

### Migrating from an old database

Some applications require a populated database to run properly.  
If you want to import your **SQL** dump, you can use several methods:

1. <a href="https://dbms-pma.clever-cloud.com/">our WebGUI (PHP My Admin)</a>
2. command line tool for MySQL administration
3. any MySQL client such as MySQL Workbench.


<br/><br/>If you need to import a very large dump, please send an email to <support@clever-cloud.com>.


## PostgreSQL plans

<table class="table table-bordered table-striped dataTable"><caption>PostgreSQL pricing plans</caption>
  <tr>
    <th>Name</th>
    <th>Cache (Memory)</th>
    <th>Disk</th>
    <th>Conn. limit</th>
    <th>Price /mo</th>
  </tr> 
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">S</span></td>
    <td>SHARED</td>
    <td>256 MB</td>
    <td>2</td>
    <td>Free</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">M</span></td>
    <td>1 GB</td>
    <td>100 GB</td>
    <td>75</td>
    <td>30 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">L</span></td>
    <td>8 GB</td>
    <td>450 GB</td>
    <td>500</td>
    <td>240 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">XL</span></td>
    <td>32 GB</td>
    <td>600 GB</td>
    <td>750</td>
    <td>700 €</td>
  </tr>
  <tr>
    <td class="cc-col__price "><span class="label cc-label__price label-info">XXL</span></td>
    <td>64 GB</td>
    <td>900 GB</td>
    <td>1000</td>
    <td>1200 €</td>
  </tr>
</table>

### Migrating from an old database

Some applications require a populated database to run properly.  
If you want to import your **SQL** dump, you can use several methods:

1. <a href="https://dbms-adminer.clever-cloud.com/adminer/">our WebGUI (Adminer)</a>
2. command line tool for PostgreSQL administration like psql`
3. any PostgreSQL client such as pgAdmin 3.



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
    "folder" : "/myFolder"
  }
]
```


It's a json array containing objects with two fields:

* **bucket**
: The bucket id you received by email which begins with `bucket_`.

* **folder**
: The folder you want the bucket to be mounted in. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application or via */app/myFolder*.

<div class="alert alert-hot-problems">
  <h5>Important note about target folder</h5>
  <p>
    The folder must not exists in your repository (or it needs to be empty). Otherwise, the mount of your bucket will be ignored.
  </p>
</div>



## MongoDB <span class="cc-beta pull-right" title="Currently in Beta version"></span>
<div class="alert alert-hot-problems">
  <h5>Note for Beta Version</h5>
  <div>MongoDB is free during the beta period. No credits wil be charged.</div>
</div>
MongoDB is an open source NoSQL document-oriented database.




## Couchbase <span class="cc-beta pull-right" title="Currently in Beta version"></span>

<div class="alert alert-hot-problems">
  <h5>Note for Beta Version</h5>
  <div>Couchbase is free during the beta period. No credits wil be charged.</div>
</div>

Couchbase is an open source, distributed (shared nothing architecture) NoSQL document-oriented database.
