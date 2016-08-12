---
title: Clever Cloud CLI published-config
position: 1
shortdesc: Manage your application's exposed configuration using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
- configuration
- published-config
---

# Exposed configuration

Clever-tools allows you to get and update the exposed configuration of your application.

### Get the whole configuration

Simply use the commmand down below to get all your variables.

    clever published-config

### Add and update a variable

Use `set` to add or update a variable.

    clever published-config set <variable-name> <variable-value>

### Remove a variable

Use `rm` to remove a variable.

    clever published-config rm <variable-name>

### Import a configuration

Clever-tools allows you to load a configuration from STDIN.

    cat <configuration> | clever published-config import
