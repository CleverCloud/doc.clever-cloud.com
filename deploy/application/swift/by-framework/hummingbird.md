---
title: Deploy Hummingbird applications
shortdesc: Hummingbird is a lightweight and flexible HTTP server framework written in Swift.
tags:
- deploy
keywords:
- swift
- hummingbird
str_replace_dict:
  "@application-type@": "Swift + Hummingbird"
---

## Overview

Clever Cloud allows you to deploy any [Hummingbird](https://github.com/hummingbird-project) application.
This page explains you how to set up your application to run it on our service.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

{{< readfile "/content/partials/language-specific-deploy/swift.md" >}}

{{< readfile "/content/partials/new-relic.md" >}}

{{< readfile "/content/partials/env-injection.md" >}}

To access environment variables from your code, you can use `HBEnvironment.shared.get("MY_VARIABLE")`.

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
