---
title: Clever Cloud CLI service dependencies
position: 1
shortdesc: Manage your service dependencies using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Manage your service dependencies

### List your dependencies

You can list your dependencies using `clever service`

    # Show dependencies of your application
    clever service

    # Only show apps (also work with addons using --only-addons)
    clever service --only-apps

    # List all your available apps and addons
    clever service --show-all

### Link or unlink dependencies

You can link dependencies using `clever service link`

    # Link an application
    clever service link-app <app-name>

    # Link an addon
    clever service link-addon <addon-name>

You can unlink dependency using `unlink-app` and `unlink-addon`