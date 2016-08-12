---
title: Clever Cloud CLI application
position: 1
shortdesc: List linked applications using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Linked applications

### Listing linked applications

You can list your linked applications with `clever applications`. For each application, the command shows you the alias, the id and the deployment url.

    # List linked applications
    clever applications

    # List only application aliases
    clever applications --only-aliases