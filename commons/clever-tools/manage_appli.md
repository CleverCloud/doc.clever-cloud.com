---
title: Clever Cloud CLI manage
position: 1
shortdesc: Manage your application using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Manage your application

### Stop or restart your application

`clever stop <app-name>` allows you to stop your application.
If you want to restart your application, you can use :

    clever restart <app-name>

    # Restart the application with a specific commit id
    clever restart <app-name> --commit <commit-id>

You can use `--quiet` when you restart, logs won't appear during deployment.

### Get continuous logs from your application

You can see logs with the command down below.

    clever logs

You can also add a flag `--before` or `--after` followed by a date (ISO8601 format).

    ## Here is an example
    clever logs --before 2016-08-11T14:54:33.971Z

### Status of your application

Clever-tools can show you the status of an application on Clever Cloud using `clever status`. This command shows you if the application is running or stopped and informations about the scalability of your application.

### Activity of your application

`clever-tools` can show you the activity of an application. For each deployment, you get :

* date and time of deployment
* status (OK or FAIL)
* action (DEPLOY or UNDEPLOY)
* commit ID
* tool used to deploy (Git/Console/clever-tools/...)


    # Show the last 10 deployments
    clever activity

    # Show all deployments
    clever activity --show-all

    # Show deployments and track new ones
    clever activity --follow