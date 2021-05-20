---
title: MongoDB
position: 5
shortdesc: A noSQL and document-oriented database, operated by Clever Cloud.
tags:
- addons
keywords:
- mongo
- mongodb
- document
- nosql
- database
---

MongoDB is an open source NoSQL document-oriented database. We provide these databases with daily backups and monitoring for both shared and dedicated plans.

## Versions

The version currently installed by the add-on is :

- on shared plans (Peanut) : MongoDB 4.0.3
- on newly created dedicated databases (plans Hazelnut and above) : MongoDB 4.0.3

## About Free Databases

Free plans are recommended for test and development usage only. Using these databases in production is not recommended, because performance may vary depending on the global usage of the cluster. Therefore, before switching to production, consider upgrading to a dedicated database for better performance.

### Important Note About Fair Use on Free Databases

Heavy usage of free databases may impact the shared cluster they rely upon. It will degrade performance of the other databases. To that extent, we set a soft limit of **15 operations/second**. Going above the limit will expose your database to disconnection, would you not answer our notices.

{{< readfile "/content/partials/db-backup.md" >}}

## Database Migration Process

The migration process is pretty much the same for each of these cases:

- migrating from a Clever Cloud shared database to a dedicated one
- migrating from an external database to a Clever Cloud one
- migrating between Clever Cloud Databases

The process consists in three steps:

1. First, perform a backup and download it, either with the Clever Cloud add-on dashboard or the `mongodump` command from your workstation.
2. Install `mongorestore` (a tool packaged with [MongoDB](https://docs.mongodb.com/manual/administration/install-community/))
3. On your workstation, use the taylor-made `mongorestore` command line located in your mongodb dashboard page. If needed, change the `nsFrom` and `nsTo` flags, depending on what you actually want to do (importing this database in another, importing another to this one, ...)

## Encryption at rest

Encryption at rest is available on MongoDB. You can have more information on the [dedicated page]({{< ref "administrate/encryption-at-rest.md" >}})

## Plans

{{< pricingAddon "mongodb-addon" "[\"cpu\", \"memory\", \"disk-size\", \"has-logs\", \"has-metrics\"]" >}}
