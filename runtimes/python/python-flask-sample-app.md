---
title: Sample Python application
shortdesc: The goal of this article is to show you how configure and deploy a simple Flask application on Clever Cloud.
tags:
- python
---

# Flask Demo

This is a small Flask app that replies with a header dump to any request. 

You can deploy this application as-is on Clever Cloud:

```bash
git clone https://github.com/CleverCloud/demo-flask
git remote add clevercloud {your-clever-cloud-endpoint}
git push clevercloud master
```

And it will do the trick.

We demonstrate several Clever Cloud possibilities in this application:

## Choose python version

We have a `python_version` file in the `/clevercloud` folder that contains **3**.
This will select the latest *Python 3* version installed (at the time of this writing, 3.4).

Documentation: [Use Python 3](https://www.clever-cloud.com/doc/python/python_apps/#use-python-3)

## Build app in a subfolder

In the `/clevercloud/python.json` file, we have the `"build"."folder"` field that points to the `app` folder.
We will build & run the app from this `app` folder.


## Static files

Documentation for Python static files: [Manage your static files](https://www.clever-cloud.com/doc/python/python_apps/#manage-your-static-files)

## Run the app

As explained in [Select your module](https://www.clever-cloud.com/doc/python/python_apps/#select-your-module) 
we ask Clever Cloud to run the "app" Flask application defined in the `hello.py` file. So the "module" field is `hello:app`.
