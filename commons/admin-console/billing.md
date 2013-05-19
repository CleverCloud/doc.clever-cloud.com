---
title: Billing
position: 2
---
## Billing
Each app and service consume computing ressources. Ressources are defined by two factors : time and drops. The drop is the unit of computing energy used by workers.
Workers are virtual machine dedicated to apps. When your application needs more ressrouces, you can decide to add workers or switch for a more powerful one.


### Estimating Costs

When you deploy an app, a worker is created to host it. Each languages have four type of workers

* Tiny
* Medium
* Large
* X-Large

#### Workers billing rates
<table class="table table-bordered table-striped">
  <thead>
    <tr>
      <th>Ressource</th>
      <th>Unit</th>
      <th>Unit cost</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><span class="label label-info">Small</span> Worker Java</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td>Medium Worker Java</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td><span class="label label-info">Small</span> Worker Node</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
      <td>Medium Worker Node</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td><span class="label label-info">Small</span> Worker Python</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td>Medium Worker Python</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td><span class="label label-info">Small</span> Worker Play!</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td>Medium Worker Play!</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td><span class="label label-info">Small</span> Worker PHP</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
    <tr>
      <td>Medium Worker PHP</td>
      <td>2 Drops</td>
      <td>0,012c €</td>
    </tr>
  </tbody>
</table>

#### Databases & Services billing rates

<table class="table table-bordered table-striped">
  <thead>
    <tr>
      <th>Operation</th>
      <th>Service</th>
      <th>Unit cost</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Read</td>
      <td>mySQL</td>
      <td>$0.10 per 100k operations</td>
    </tr>
    <tr>
      <td>Read</td>
      <td>PostgreSQL</td>
      <td>$0.10 per 100k operations</td>
    </tr>
    <tr>
      <td>Read</td>
      <td>Couchbase</td>
      <td>$0.10 per 100k operations</td>
    </tr>
  </tbody>
</table>

### Automatic Scaling

Clever Cloud allows you to scale your apps automatically if your application is under an unexpected load.
Go to your app's configuration to enable auto-scaling.

<div class="alert alert-hot-problems">
  <h5>Automatic-scaling and variable costs</h5>
  <p>
    Many of our users may fear auto-scaling if their is suddenly consuming a lot of ressources. First note you can set an interval for scaling.
  </p>
  <p>In the other hand, the apps scales down by itself if a low activity is observed on the app during a period of 10min.</p>
</div> 

### Making a Payment
#### Credit Card
#### Paypal