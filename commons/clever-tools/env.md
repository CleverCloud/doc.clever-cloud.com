---
title: Clever Cloud CLI env
position: 1
shortdesc: Manage application environment using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Environment

Clever-tools allows you to get and update the environment of your application.

## Get the whole environment

Simply use the commmand down below to get all your variables.
    clever env

## Add and update an environment variable

Use `set` to add or update an environment variable.
    clever env set <variable-name> <variable-value>

## Remove an environment variable

Use `rm` to remove an environment variable.
    clever env rm <variable-name>

## Load environment variables

Clever-tools allows you to load variables from STDIN.
######################################