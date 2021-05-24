---
title: Deploy Django application
shortdesc: The goal of this article is to show you how to deploy a Django application on Clever Cloud.
tags:
- deploy
keywords:
- python
- django
---


## Overview

The goal of this article is to show you how to deploy a Django application on Clever Cloud.
The application is a very basic one. More information about the application:  

*  [GitHub repo](https://github.com/CleverCloud/django-example)
*  [Clever Cloud demo](https://django.cleverapps.io/)

{{< readfile "/content/partials/create-application.md" >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

## Configure your Django application
### My application does not exists already

If you want to test easily a Django deployment on Clever Cloud, just clone the [GitHub repo](https://github.com/CleverCloud/django-example) and go the next section.

### My application already exists

<br/><br/>
{{< alert "warning" "Reminder" >}}
  Do not forget to add the `CC_PYTHON_MODULE` environment variable  or the file [clevercloud/python.json](https://github.com/CleverCloud/django-example/blob/master/clevercloud/python.json) in any Python project so that we get your required modules.
{{< /alert >}}

### Fine tuning the application

You can find a lot more configuration options such as choosing python version and more on our dedicated [Python documentation]({{< ref "/deploy/application/python/python_apps.md" >}}).

{{< readfile "/content/partials/new-relic.md" >}}

{{< readfile "/content/partials/env-injection.md" >}}

To access [environment variables](#setting-up-environment-variables-on-clever-cloud) from your code, just get them from the environment with:

```python
import os
os.getenv("MY_VARIABLE")
```

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}
