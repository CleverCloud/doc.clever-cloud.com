---
title: Clever Cloud Add-ons
position: 3
---

# Clever Cloud Add-ons

Add-on are a way to add services to your application, for example a database or a caching system, to enrich its
features.

An add-on can be shared by different applications to share data between them. It can be a database shared by two or
three applications of your infrastructure for example, or they can be independent.

The add-ons offer different plan to adapt to your needs. You can find details about them on the documentation page of
the add-on.

## Available add-on

Clever Cloud provides multiple add-on to work with you application :

* [MySQL](mysql.md)
* [PostgreSQL](postgresql.md)
* [MongoDB](mongodb.md)
* [FS buckets](fs_buckets.md)
* [Cellar](cellar.md)
* [Redis](redis.md)

## Create an add-on for your application

In order to create an add-on for your application, go to the [Clever Cloud Console](https://console.clever-cloud.com/).
When you are in the main page of the console, go to the organisation in which you wan to create the add-on,
for example your [personal space](https://console.clever-cloud.com/users/me).

When you are in the organisation, click on **Add an add-on**. This space let you create and configure the
add-on to follow your needs.

First, choose which *type* of add-on you want to create. See above to have a list of available add-ons and the
corresponding pages for a description and information.

Now, select the plan you need for you add-on. You can find details about the pricing, the capacity of the add-on, ...
on this page or in the corresponding documentation page.

The third step offer you to choose with which application you want to link you add-on. Linking an add-on with an
application add [environment variables](/admin-console/environment-variables.md) to the application.
This environment variables must be used in your application to, for example, provide the credentials and the
localization of the database provided by the add-on.

The last step let you choose the name of the add-on (for example "My database") and the region where the add-on is
hosted. Click on the button **Create** and the add-on will now available in your organisation, and corresponding
variables will be available for the applications linked to the add-on you just created.

## Managing your add-on

You can find information on how to manage your add-ons [here](/addons/managing-addons.md).
