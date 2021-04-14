---
title: MySQL
position: 6
shortdesc: MySQL is an open-source relational database management system (RDBMS).
tags:
- addons
keywords:
- sql
- mysql
- mariadb
- RDBMS
---

MySQL is an open-source relational database management system (RDBMS).

## MySQL version

The version currently installed by the add-on is :

- on shared plan (DEV) : 8.0
- on dedicated databases (XXS Small Space and above) : 5.7 or 8.0

## MySQL plans

{{< alert "warning" "Note for Shared databases" >}}
  <p>As Shared databases (DEV) are shared between multiple applications and delays could appear in case of a high demand.</p>
  <p>If this delays create problems in your application or are problematcs, we recommend you to use a dedicated database (XS plans and above).</p>
{{< /alert >}}

<table class="table table-bordered table-striped dataTable">
  <caption>MySQL pricing plans</caption>
  <tr>
    <th>Name</th>
    <th>DB size</th>
    <th>Conn. limit</th>
    <th>Memory</th>
    <th>Type</th>
    <th>VCPUS</th>
    <th>Price / Month</th>
    <th>Logs & Metrics</th>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">DEV</span>
    </td>
    <td>10 MB</td>
    <td>5</td>
    <td>Shared</td>
    <td>Shared</td>
    <td>Shared</td>
    <td>Free</td>
    <td>No</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXS Small Space</span>
    </td>
    <td>512 MB</td>
    <td>15</td>
    <td>512 MB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>5.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXS Medium Space</span>
    </td>
    <td>1 GB</td>
    <td>15</td>
    <td>512 MB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>5.90 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXS Big Space</span>
    </td>
    <td>2 GB</td>
    <td>15</td>
    <td>512 MB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>6.80 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XS Tiny Space</span>
    </td>
    <td>2 GB</td>
    <td>75</td>
    <td>1 GB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>13.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XS Small Space</span>
    </td>
    <td>5 GB</td>
    <td>75</td>
    <td>1 GB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>22.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XS Medium Space</span>
    </td>
    <td>10 GB</td>
    <td>75</td>
    <td>1 GB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>24.50 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XS Big Space</span>
    </td>
    <td>15 GB</td>
    <td>75</td>
    <td>1 GB</td>
    <td>Dedicated</td>
    <td>1</td>
    <td>27.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">S Small Space</span>
    </td>
    <td>10 GB</td>
    <td>125</td>
    <td>2 GB</td>
    <td>Dedicated</td>
    <td>2</td>
    <td>44.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">S Medium Space</span>
    </td>
    <td>15 GB</td>
    <td>125</td>
    <td>2 GB</td>
    <td>Dedicated</td>
    <td>2</td>
    <td>46.50 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">S Big Space</span>
    </td>
    <td>20 GB</td>
    <td>125</td>
    <td>2 GB</td>
    <td>Dedicated</td>
    <td>2</td>
    <td>49.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">M Small Space</span>
    </td>
    <td>20 GB</td>
    <td>250</td>
    <td>4 GB</td>
    <td>Dedicated</td>
    <td>4</td>
    <td>103.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">M Medium Space</span>
    </td>
    <td>40 GB</td>
    <td>250</td>
    <td>4 GB</td>
    <td>Dedicated</td>
    <td>4</td>
    <td>113.60 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">M Big Space</span>
    </td>
    <td>80 GB</td>
    <td>250</td>
    <td>4 GB</td>
    <td>Dedicated</td>
    <td>4</td>
    <td>133.60 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">L Small Space</span>
    </td>
    <td>40 GB</td>
    <td>500</td>
    <td>8 GB</td>
    <td>Dedicated</td>
    <td>6</td>
    <td>207.20 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">L Medium Space</span>
    </td>
    <td>80 GB</td>
    <td>500</td>
    <td>8 GB</td>
    <td>Dedicated</td>
    <td>6</td>
    <td>227.20 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">L Big Space</span>
    </td>
    <td>120 GB</td>
    <td>500</td>
    <td>8 GB</td>
    <td>Dedicated</td>
    <td>6</td>
    <td>247.20 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XL Small Space</span>
    </td>
    <td>80 GB</td>
    <td>750</td>
    <td>16 GB</td>
    <td>Dedicated</td>
    <td>8</td>
    <td>414.40 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XL Medium Space</span>
    </td>
    <td>160 GB</td>
    <td>750</td>
    <td>16 GB</td>
    <td>Dedicated</td>
    <td>8</td>
    <td>454.40 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XL Big Space</span>
    </td>
    <td>320 GB</td>
    <td>750</td>
    <td>16 GB</td>
    <td>Dedicated</td>
    <td>8</td>
    <td>534.40 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXL Small Space</span>
    </td>
    <td>160 GB</td>
    <td>900</td>
    <td>32 GB</td>
    <td>Dedicated</td>
    <td>10</td>
    <td>1022.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXL Medium Space</span>
    </td>
    <td>320 GB</td>
    <td>900</td>
    <td>32 GB</td>
    <td>Dedicated</td>
    <td>10</td>
    <td>1134.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXL Big Space</span>
    </td>
    <td>640 GB</td>
    <td>900</td>
    <td>32 GB</td>
    <td>Dedicated</td>
    <td>10</td>
    <td>1358.00 €</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td class="cc-col__price">
      <span class="label cc-label__price label-info">XXL Huge Space</span>
    </td>
    <td>960 GB</td>
    <td>900</td>
    <td>32 GB</td>
    <td>Dedicated</td>
    <td>10</td>
    <td>1582.00 €</td>
    <td>Yes</td>
  </tr>
</table>

{{< readfile "/content/partials/db-backup.md" >}}

## Migrating from an old database

Some applications require a populated database to run properly.
If you want to import your **SQL** dump, you can use several methods:

1. [Our WebGUI (PHP My Admin)](https://dbms-pma.clever-cloud.com/).
2. Command line tool for MySQL administration.
3. Any MySQL client such as [MySQL Workbench](https://www.mysql.fr/products/workbench/).

If you need to import a very large dump, please send an email to <support@clever-cloud.com>.

## Direct access

All our dedicated MySQL databases are served via a proxy. To reduce the latency you can bypass this proxy by generating direct hostname and port for the addon. You can do it by clicking the "Generate direct hostname and port" on the addon dashboard.

This action will add new environment variables to reach the addon without any proxy.

Keep in mind that usage of direct access is a trade-off: when you migrate your addon, you will need to to generate hostname and port again so your application must update these environment while proxy usage changes nothing.

## Encryption at rest

Encryption at rest is available on MySQL. You can have more information on the [dedicated page]({{< ref "administrate/encryption-at-rest.md" >}})
