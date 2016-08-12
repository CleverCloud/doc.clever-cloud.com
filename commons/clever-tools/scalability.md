---
title: Clever Cloud CLI scalability
position: 1
shortdesc: Change scalability of you application using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Scalability

### Flavors

You can choose the scale of your application using `--flavor <flavor>` where `<flavor>` can be pico, nano, XS, S, M, L or XL.

    # Change the scale of your application
    clever scale --flavor <flavor>

    # Set the minimum and maximum scale for your application
    clever scale --min-flavor <min-flavor> --max-flavor <max-flavor>

### Instances

You can choose the number of parallels instances using `--instances <instances>` where `<instance>` is a number between 1 and 20.

    # Change the number of parallels instances
    clever scale --instances <instances>

    # Set the minimum and maximum number of parallels instances
    clever scale --min-instances <min-instances> --max-instances <max-instances>