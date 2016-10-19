---
title: Getting started with Clever Cloud CLI
position: 1
shortdesc: Installing and using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Clever Cloud CLI overview

In addition to the Clever Cloud console, you can manage your addons and
applications from the command line with `clever-tools`.

## Installing `clever-tools`

`clever-tools` is packaged and distributed through [npm](https://npmjs.com).
To use it, you need to have nodeJS and npm running on your system, then

    npm install -g clever-tools

### Installing auto-completion

`clever-tools` comes with a powerful auto-completion support. Make sure to
enable it for a better experience.

    install-clever-completion

## Linking your account

Once you have installed `clever-tools`, the next step is to link your account:

    clever login

This will open a login page in your browser, and will give you identification
tokens. Copy them and paste them in the `clever login` prompt.

## Linking an existing application

If you have an already existing application, you can start managing it with
`clever-tools`.

    # First, go to your local repository
    cd /path/to/your/application

    # Then, link it to the Clever Cloud application
    clever link <app_id>

    # You can also use the application name (make sure to specify the
    # organisation name if your application is in an organisation.
    clever link --org <org_name> <app_name>

    # Unlink an application
    clever unlink <app_id>

## Deploying new code

After having written new code, you can deploy it to Clever Cloud

    # Will git push your code to Clever Cloud and display logs
    clever deploy

    # Will open your application
    clever open
