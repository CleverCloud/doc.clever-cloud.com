---
title: Deploy Vapor applications
shortdesc: Vapor is a framework for writing server applications, HTTP services and backends in Swift. It provides a really nice API to enable you to write safe, efficient and maintainable applications in Swift.
tags:
- deploy
keywords:
- swift
- vapor
str_replace_dict:
  "@application-type@": "Swift + Vapor"
---

## Overview

Clever Cloud allows you to deploy any [Vapor](https://vapor.codes/) application.
This page explains you how to set up your application to run it on our service.

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

{{< readfile "/content/partials/language-specific-deploy/swift.md" >}}

{{< readfile "/content/partials/new-relic.md" >}}

{{< readfile "/content/partials/env-injection.md" >}}

To access environment variables from your code, you can use `Environment.get("MY_VARIABLE")`.

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
