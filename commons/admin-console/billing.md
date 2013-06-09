---
title: Billing
position: 2
---
## Billing
Each app and service consume computing ressources. Ressources are defined by two factors : time and drops. The drop is the unit of computing energy used by workers.
Workers are virtual machine dedicated to apps. When your application needs more ressrouces, you can decide to add workers or switch for a more powerful one.


### Estimating Costs

When you deploy an app, a worker is created to host it. Each languages have four type of scalers, aka workers (see [what is a scaler](http://localhost:8000/get-help/faq/#what-is-a-scaler)):

* Tiny 
* Small 
* Medium 
* Large 
* XtraLarge 
* Huge 
* XtraHuge 

#### Scalers billing rates
<table class="table table-bordered table-striped">
  <thead>
    <tr>
      <th>Size</th>
      <th>Ressource</th>
      <th>Unit</th>
      <th>Unit cost per hour</th>
    </tr>
  </thead>
  <tbody class="billing-table">
  </tbody>
</table>

#### Databases & Services billing rates

<table class="table table-bordered table-striped">
   <tbody><tr><th>Action</th><th colspan="2">Drops</th></tr>
  <tr><th>&nbsp;</th><th>mySQL</th><th>PostgreSQL</th></tr>
  <tr><td>Create DB</td><td>6</td><td>6</td></tr>
  <tr><td>10k SELECT Queries</td><td>1</td><td>1</td></tr>
  <tr><td>10k INSERT Queries</td><td>2</td><td>2</td></tr>
  <tr><td>10k DELETE Queries</td><td>2</td><td>2</td></tr>
  <tr><td>10k UPDATE Queries</td><td>2</td><td>2</td></tr>
  <tr><td>10k other Queries</td><td>2</td><td>2</td></tr>
</tbody></table>


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
      <td>Create DB</td>
      <td>mySQL</td>
      <td>6 Drop per 10k operations</td>
    </tr>
    <tr>
      <td>Read</td>
      <td>mySQL</td>
      <td>1 Drop per 10k operations</td>
    </tr>
    <tr>
      <td>Insert</td>
      <td>mySQL</td>
      <td>2 Drops per 10k operations</td>
    </tr>
    <tr>
      <td>Delete</td>
      <td>mySQL</td>
      <td>2 Drops per 10k operations</td>
    </tr>
    <tr>
      <td>Create DB</td>
      <td>PostgreSQL</td>
      <td>6 Drop per 10k operations</td>
    </tr>
    <tr>
      <td>Read</td>
      <td>PostgreSQL</td>
      <td>1 Drop per 10k operations</td>
    </tr>
    <tr>
      <td>Insert</td>
      <td>PostgreSQL</td>
      <td>2 Drops per 10k operations</td>
    </tr>
    <tr>
      <td>Delete</td>
      <td>PostgreSQL</td>
      <td>2 Drops per 10k operations</td>
    </tr>
  </tbody>
</table>

### Manual Scaling

During the process of app's creation, you are requested to choose how much Scalers you want for your app. A scaler is an individual container hosting your app. You can attribute one or more Scalers to your apps.

Manual scaling is easy : 

1. Choose to have one or more instances
2. Select the size of your scalers (optimize will choose the best configuration for performances)
3. Save your configuration. Your app will be automatically updated with the right configuration.

<figure class="cc-content-img">
  <a href="/assets/images/instances-size-configuration.png"><img src="/assets/images/instances-size-configuration.png"/></a>
  <figcaption>Configuring app scaling 
  </figcaption>
</figure>



### Automatic Scaling

Clever Cloud allows you to scale your apps automatically if your application is under an unexpected load.

1. Go to your app's configuration  
2. Activate the checkbox _auto scale-out_.
3. You can now set up a range of minimum and maximum of scaler.

When you app will experience a load, it will progressively use more and more scalers, until it reaches the maximum amount you have setup.

<div class="alert alert-hot-problems"><h5>Automatic-scaling and variable costs</h5>
<p>Many of our users may fear auto-scaling if their is suddenly consuming a lot of ressources.</p><p>But the apps scales down by itself if a low activity is observed during a period of 10min.</p></div> 

