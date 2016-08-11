---
title: Clever Cloud CLI addon
position: 1
shortdesc: Manage addons using the Clever Cloud CLI tool
tags:
- cli-setup
keywords:
- cli
- clever-tools
---

# Addon

Using `clever addon`, you can add, rename or delete addon and list available addon providers

## Listing your addons

You can list your addons with `clever addon`

## Creating an addon

First of all, you have to list addon providers and then, create your addon.

    ## List addon providers
    clever addon providers

    ## Create your addon and link it to your application with --link
    clever addon create <provider-name> <addon-name> --link

    ## Create an addon for an organisation
    clever addon create <provider-name> <addon-name> --link --org <org-name>

## Choosing provider's plan and region

You can choose your addon's plan when you create it using `--plan <plan-name>`. The plan is by default 'dev'. With the flag "--yes" you can skip confirmation even if the addon is not free. You can also select the region that will provision the addon in.

    ## Show more informations about a provider(plans and available regions)
    clever addon providers show <provider-name>

    ## Create your addon
    clever addon create <provider-name> --plan <plan> --region <region-name> <addon-name>

## Rename or delete an addon

Use `addon rename` and `addon delete` to respectively rename and delete an addon.
If you don't want any confirmation, use `--yes`.

    ## Rename an addon
    clever addon rename <addon-name> <new-name>

    ## Delete an addon
    clever addon delete <addon-name>