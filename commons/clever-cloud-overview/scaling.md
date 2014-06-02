---
title: Scaling
position: 3
---

# Scaling management

*Clever Cloud gives you the ability to fine tune your application's scaling by managing both horizontal and vertical scaling. These two parameters can be combined or not and are based on the notion of Scaler.*

## What is a Scaler?

A Scaler is the Clever Cloud "instance". It is an individual container hosting your application. They are not virtual machines and are independent.  
A Scaler is defined by two factors: RAM and CPU.  

With the Scalers, Clever Cloud gives you the ability to scale your application <b>up and down</b> with <b>two non exclusive methods</b>: horizontal and vertical scaling.

## Horizontal scaling

Provided by most PaaS providers. 

In case of large traffic, we detect a high load on your application and spawn <b>another instance in parallel</b>.
It gives more power to your application by setting up another identical application. Both will run in parallel with load balancing. And so on.  

This process is exactly the opposite when the <b>load decreases</b>. A Scaler is removed and so on till a <b>minimum reasonable level</b>.

The following scheme depicts a Scaler replication in case of a load increase:  

<figure class="cc-content-img" >
  <a href="/assets/images/scaling_horizontal_scheme.jpg"><img src="/assets/images/scaling_horizontal_scheme.jpg"/></a>
</figure>

<br/>
<br/>

You can manage the range of Scalers you consider in the application configurator. The range is from 1 to 40.

<figure class="cc-content-img" style="width:355px">
  <a href="/assets/images/scaling_horizontal.png"><img src="/assets/images/scaling_horizontal.png"/></a>
  <figcaption>Horizontal scaling: the amount of scaler will increase, not the scaler size.</figcaption>
</figure>



## Vertical scaling

In case of large traffic, we detect a high load on your application and setup <b>a new larger Scaler</b>.  

In case of low traffic, we detect a low load and setup <b>a new smaller Scaler</b>.  

You give more power to your application by setting up a larger instance that will replace the previous one. The more the load, the larger the instance.  

The following scheme depicts a larger Scaler replacement in case of a load increase:


<figure class="cc-content-img">
  <a href="/assets/images/scaling_vertical_scheme.jpg"><img src="/assets/images/scaling_vertical_scheme.jpg"/></a>
</figure>

<br/>
<br/>

You can choose the type of Scalers you consider of just let Clever Cloud choose for you:

<figure class="cc-content-img">
  <a href="/assets/images/scaling_vertical.png"><img src="/assets/images/scaling_vertical.png"/></a>
  <figcaption>Vertical scaling: the amount of scalers will not increase in this configuration, but the scaler size will, from S to XL.</figcaption>
</figure>



## Combination of both scalings

When both scalings are set up, Clever Cloud adjusts the required resources based on our own allocation system.
