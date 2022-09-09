---
title: Deploy a Vapor aplication
shortdesc: How to deploy a Vapor application using Docker on Clever Cloud.
tags:
- deploy
keywords:
- docker
- swift
- vapor
str_replace_dict:
  "@application-type@": "Docker"
---

## Overview

For now, Clever Cloud does not allow you to deploy a Swift application.
If you're planning on deploying a [server-side Swift](https://swift.org/server/) application,
you will need to use a Docker container.
This page will explain how to set up your application to run it on our service.

{{< readfile "/content/partials/create-application.md" >}}

{{< alert "info" "What vertical scalability do I choose?" >}}
For a Vapor app deployed using Docker, you can use a `nano` scaler, it's enough in most cases.
However, if you use such a small instance, you will need to configure a
[dedicated build instance]({{< ref "/administrate/apps-management.md#edit-application-configuration" >}})
with a flavor `M` or `L` at least. The bigger the build instance, the faster the build.
You will be billed depending on the time it takes to build your project
(see [Analytics and consumption]({{< ref "/billing/analytics-consumption.md" >}}) for more information),
so most of our users decide to use big build instances.
For more information about scalability, see [Application scaling]({{< ref "/administrate/scalability.md" >}}).
{{< /alert >}}

{{< readfile "/content/partials/set-env-vars.md" >}}

{{< readfile "/content/partials/language-specific-deploy/docker.md" >}}

{{< alert "info" "Vapor's template Dockerfile" >}}
When you create a Vapor app using `vapor new`, Vapor's template already contains a Dockerfile.
If you don't have it, or you want to check if it's up-to-date, you can find it at
[github.com/vapor/template/Dockerfile](https://github.com/vapor/template/blob/main/Dockerfile).
{{< /alert >}}

{{< readfile "/content/partials/env-injection.md" >}}

{{< readfile "/content/partials/deploy-git.md" >}}

{{< readfile "/content/partials/link-addon.md" >}}

{{< readfile "/content/partials/more-config.md" >}}

## Fixing problems

### Fix 'unable to find bundle'

If your Swift package includes bundled resources, it's possible they're not found at runtime
because your Dockerfile doesn't copy it.
You might have errors like `Fatal error: could not load resource bundle` or other similar messages.

{{< alert "info" "Issue fixed in template Dockerfile" >}}
The issue seems to have been fixed in commits
[`1cca791`](https://github.com/vapor/template/commit/1cca791a00d17211cb7b339b3a52465017d52465) and
[`49c4fca`](https://github.com/vapor/template/commit/49c4fcafe46ab9b8aa435a8efadbdda85bcbe371)
of Vapor's template Dockerfile. You should try it first.
{{< /alert >}}

To fix this, add the following line to your Dockerfile:

```diff
# Switch to the staging area
WORKDIR /staging

# Copy main executable to staging area
RUN cp "$(swift build --package-path /build -c release --show-bin-path)/Run" ./

# […]

+ # Copy bundles to staging area
+ RUN cp "$(swift build --package-path /build -c release --show-bin-path)/*.bundle" ./ 2>/dev/null || :
+
# ================================
# Run image
# ================================
```

### Check your Dockerfile

To check if your Dockerfile is up-to-date, head up to
[github.com/vapor/template/Dockerfile](https://github.com/vapor/template/blob/main/Dockerfile).
Look for recent commits, which could be patching an issue you're facing.

### Check Vapor's documenation

In addition, don't hesitate to check [Vapor's documentation on how to deploy using Docker](https://docs.vapor.codes/deploy/docker/)
if you have any Vapor-specific issue, as their documentation will stay up-to-date.

### Contact our support team

If you think your problem comes from our side, read the
[Find Help section]({{< ref "/find-help/_index.md" >}})
or [contact the support team]({{< ref "/find-help/support.md" >}}).
