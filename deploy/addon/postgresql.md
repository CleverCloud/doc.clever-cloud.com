---
title: PostgreSQL
position: 7
shortdesc: PostgreSQL is an open-source relational database management system (RDBMS).
tags:
- addons
keywords:
- sql
- postgresql
- pg
- pgsql
- mariadb
---

PostgreSQL is an object-relational database management system (ORDBMS) with an emphasis on extensibility
and on standards-compliance.

## Version

The version currently installed by the add-on is :

- on shared plans (DEV) : PostgreSQL 11.1
- on newly created dedicated databases (plans XS Small Space and above) : PostgreSQL 9.6, 10, 11, 12, 13

{{< readfile "/content/partials/db-backup.md" >}}

## Migrating from an old database

Some applications require a populated database to run properly.
If you want to import your **SQL** dump, you can use several methods:

1. [Our WebGUI (Adminer)](https://dbms-adminer.clever-cloud.com/).
2. Command line tool for PostgreSQL administration like `psql`.
3. Any PostgreSQL client such as [pgAdmin](https://www.pgadmin.org/).


## Direct access

All our dedicated PostgreSQL databases are served via a proxy. To reduce the latency you can bypass this proxy by generating direct hostname and port for the addon. You can do it by clicking the "Generate direct hostname and port" on the addon dashboard.

This action will add new environment variables to reach the addon without any proxy.

Keep in mind that usage of direct access is a trade-off: when you migrate your addon, you will need to to generate hostname and port again so your application must update these environment while proxy usage changes nothing.

## Default extensions

Every PostgreSQL database managed by Clever Cloud comes with the following default extensions:
`adminpack`,
`autoinc`,
`btree_gin`,
`btree_gist`,
`citext`,
`cube`,
`dblink`,
`dict_int`,
`dict_xsyn`,
`earthdistance`,
`file_fdw`,
`fuzzystrmatch`,
`hstore`,
`insert_username`,
`intagg`,
`intarray`,
`isn`,
`lo`,
`ltree`,
`moddatetime`,
`pageinspect`,
`pg_buffercache`,
`pgcrypto`,
`pg_freespacemap`,
`pgrowlocks`,
`pg_stat_statements`,
`pgstattuple`,
`pg_trgm`,
`plcoffee`,
`plls`,
`plv8`,
`postgis`,
`postgis_tiger_geocoder`,
`postgis_topology`
`postgres_fdw`,
`refint`,
`seg`,
`sslinfo`,
`tablefunc`,
`tcn`,
`timetravel`,
`unaccent`,
`"uuid-ossp"`,
`xml2`

## Encryption at rest

Encryption at rest is available on PostgreSQL. You can have more information on the [dedicated page]({{< ref "administrate/encryption-at-rest.md" >}})

## Automatic vacuuming

[Autovacuum](https://www.postgresql.org/docs/current/routine-vacuuming.html) is automatically enabled on PostgreSQL add-ons.  
The autovacuum will proceed when a given percentage of rows of a table will be updated/inserted/deleted.  
Usually this threshold is set to 20%.

## Plans

<cc-smart-container context='{"productId": "postgresql-addon", "addonFeatures": ["cpu", "memory", "disk-size", "connection-limit", "has-logs", "has-metrics"]}'>
  <cc-pricing-product mode="addon" action="none">
    <div slot="head"></div>
  </cc-pricing-product>
</cc-smart-container>
