---
title: Deploy Rust applications
shortdesc: Rust is a system programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety.
tags:
- deploy
keywords:
- rust
- cargo
str_replace_dict:
  "@application-type@": "Rust"
---

## Overview

Rust is a system programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety. You can build Rust web services with frameworks like [Actix](https://actix.rs/) or [Iron](https://github.com/iron/iron).

Clever Cloud allows you to deploy Rust web applications. This page will explain you how to set up your application to run it on our service.


{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

{{< readfile "/content/partials/language-specific-deploy/rust.md" >}}

{{< readfile "/content/partials/deploy-git.md" >}}

### Deployment Video

<iframe width="853" height="480" src="https://www.youtube.com/embed/mz_8jzrM13Y?rel=0&amp;showinfo=0" frameborder="0" allowfullscreen></iframe>

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
