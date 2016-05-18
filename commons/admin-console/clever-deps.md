---
title: Services Depedencies
position: 6
shortdesc: Clever Deps provide an elegant way to create data-based dependencues between applications.
tags:
- depedencies
keywords:
- billing
- consumption
---

# Services Depedencies

Clever Deps allows you to create data-based dependencies between two or more apps, in the same way add-ons share things like credentials via environment variables to linked apps.

Basically, this feature declares apps as "provider" and "consumer".

## Example

app-1 pragmatically creates environment variables app-2 needs.
So you declare app-1 as a provider, or you declare app-2 as a consumer.
If app-1 creates new environment variables, app-2 will be automatically redeployed.

<figure class="cc-content-img" style="width:355px">
  <a href="doc/assets/images/clever-deps-export.jpg"><img src="/doc/assets/images/clever-deps-export.jpg"></a>
  <figcaption>Clever Deps in action</figcaption>
</figure>


<div class="panel panel-warning">
  <div class="panel-heading">
     <h4>Note:</h4>
  </div>
  <div class="panel-body">
    Environment variables exported by an "provider" app *does not include* environment variables you have declared in the console.
  </div>
</div>

