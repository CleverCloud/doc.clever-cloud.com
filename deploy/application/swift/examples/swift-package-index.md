---
title: "Example Swift deployment: the Swift Package Index"
shortdesc: "The deployment of a real-world Swift application on Clever Cloud: the Swift Package Index"
tags:
- deploy
- example
- swift
- vapor
keywords:
- swift
- postgresql
- vapor
- spi
str_replace_dict:
  "@application-type@": "Swift"
---

{{< alert "info" "This tutorial uses Clever Cloud's web console by default but also shows CLI commands" >}}
This tutorial shows steps using [Clever Cloud's web console][console] by default
because it doesn't require any installation on your machine nor extra login steps.
However, we recommend you to have a look at the commands,
which you could find a lot quicker to use
if you already have [Clever Cloud's CLI — `clever`][cli] installed on your machine.

[console]: <https://console.clever-cloud.com>
[cli]: <{{< ref "/getting-started/cli.md" >}}>
{{< /alert >}}

## Clone the repository

```bash
git clone https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/
cd SwiftPackageIndex-Server/
# Checkout the last commit running Swift 5.7
git checkout -b swift-5.7 ffe47bd98315fd63c48e6aae6da1b6adfc3833af
# Cherry-pick a bug fix committed after `ffe47bd`
git cherry-pick ea04e88a58409fc881f6c9a16ff138f6616f0cc2
```

{{< alert "secondary" "Clever Cloud only supports Swift 5.7 for now" >}}
During the Swift runtime private beta, Clever Cloud will only support Swift 5.7 deployments.
This is caused by [a major change in the Swift 5.8 comiler structure][swift-5.8-bootstrapping]
that we haven't yet taken into account.
Rest assured, Clever Cloud's Swift runtime will be released with support for Swift 5.7, 5.8 and 5.9.

[swift-5.8-bootstrapping]: <https://forums.swift.org/t/implementing-parts-of-the-swift-compiler-in-swift/59524> "Implementing Parts of the Swift Compiler in Swift - Development / Compiler - Swift Forums"
{{< /alert >}}

## Create the application

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the left sidebar
3. Hit "Create…" then "an application"
4. Select "Create a brand new app"
5. Select "Swift"
6. Edit the scalability and choose a "nano" instance size (it is enough for this example) then hit "Next"
7. Give your application a name (e.g. "\[Examples/Swift\] SPI"),
   choose a zone then hit "Create"
8. You can click "I don't need any add-on", as we will create it in the next step

</details>

<details>
<summary>Using our CLI</summary>

{{< readfile "/content/partials/cli-first-time-use.md" >}}

```bash
clever create --type swift '[Examples/Swift] SPI'
clever scale --flavor nano
```

{{< alert "info" "Custom application owner" >}}
If you need your application to be created outside of your personal space,
you can add the `--owner <OWNER_ID>` argument to the `clever create` command
(replacing `<OWNER_ID>` by your unique organization identifier (`orga_…`)).
{{< /alert >}}

Creating an application from the CLI also runs `clever link`,
which creates a `.clever.json` in your repository.

If you are okay with pushing this little config file into the repository, you can run
`git add .clever.json && git commit -m 'Add Clever Cloud configuration file'`.
You can also ignore it from your commits using `echo '.clever.json' >> .gitignore`
and then `git add .gitignore && git commit -m 'Ignore Clever Cloud configuration file'`
if you prefer.

</details>

{{< alert "secondary" "About application scalability" >}}
We could use the "pico" instance size, but you will see that we start an ingestion script
when a deployment succeeds, and it will run quicker if the instance is bigger.

Anyway, you will be billed to the exact seconds you consume,
so as long as you [clean up](#clean-up) after you tried this tutorial,
you won't notice the difference.
{{< /alert >}}

## Configure the application

### Select a dedicated build instance

The Swift Package Index project doesn't need a lot of resources at run time.
However, if you use a small instance such as `nano` as we recommended, you will need to configure a
[dedicated build instance][dedicated-build-instance]
with a flavor `M` or `L` at least.
The bigger the build instance, the faster the build.

{{< alert "info" "Is it expensive?" >}}
You will be billed depending on the time it takes to build your project
(see [Analytics and consumption]({{< ref "/billing/analytics-consumption.md" >}}) for more information),
so most of our users decide to use big build instances.
For more information about scalability, see [Application scaling][doc-app-scaling].

As long as you [clean up](#clean-up) after you tried this tutorial, you should not see the difference.

[doc-app-scaling]: <{{< ref "/administrate/scalability.md" >}}>
{{< /alert >}}

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the leftmost sidebar
3. Select your example Swift application in the main sidebar
4. Select "Information" in the secondary sidebar
5. Check "Enable dedicated build instance" and choose the "XL" build flavor

</details>

<details>
<summary>Using our CLI</summary>

Using our CLI, you only need to run:

```bash
clever scale --build-flavor XL
```

</details>

### Create a PostgreSQL add-on

The Swift Package Index uses a PostgreSQL database to store its data.
To create a [managed PostgreSQL database][doc-pg-add-on] on Clever Cloud,
you can either use the [Clever Cloud console][console] or our [CLI][cli].

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the left sidebar
3. Hit "Create…" then "an add-on"
4. Select "PostgreSQL"
5. Select any plan ("Dev" is enough for this example) then hit "Next"
6. Link your newly created database to the "\[Examples/Swift\] SPI" app
   (or whatever you called it) then hit "Next"
7. Give your database a name (e.g. "\[Examples/Swift\] SPI DB"),
   choose a zone then hit "Next"

</details>

<details>
<summary>Using our CLI</summary>

To create a [managed PostgreSQL database][doc-pg-add-on]
and link it to your just-created Swift application
using our [CLI][cli], you only need to run:

```bash
clever addon create postgresql-addon '[Examples/Swift] SPI DB' \
    --link "[Examples/Swift] SPI" \
    --plan dev
```

{{< alert "info" "Custom add-on owner" >}}
If you need your add-on to be created outside of your personal space,
you can add the `--owner <OWNER_ID>` argument to the above command
(replacing `<OWNER_ID>` by your unique organization identifier (`orga_…`)).
{{< /alert >}}

</details>

### Create a GitHub token

Go to [github.com/settings/tokens](https://github.com/settings/tokens) then create a
Personal Access Token with the `public_repo` and `repo:status` scopes
as explained in [SwiftPackageIndex-Server/LOCAL_DEVELOPMENT_SETUP.md][gh-token-scopes].

<small>More information about Personal Access Tokens on [GitHub Docs][gh-docs-pat].</small>

### Set the application's environment variables

Go to <https://console.clever-cloud.com/users/me/applications>, select your application,
go to "Environment variables", select the "Expert" mode then paste the following environment:

{{< alert "warning" "Insert your Personal Access Token" >}}

Make sure you replace `<GITHUB_TOKEN>` with your just-created GitHub Personal Access Token.

{{< /alert >}}

```bash
# Run SPI logic
CC_POST_BUILD_HOOK="yarn && yarn build;"
CC_PRE_RUN_HOOK="scripts/clever-cloud/pre-run-hook.sh"
CC_SWIFT_BIN_ARGS="serve --hostname 0.0.0.0"
CC_WORKER_COMMAND="$BIN_PATH reconcile && $BIN_PATH ingest --limit 100 && $BIN_PATH analyze --limit 100;"
# Set a GitHub token for SPI to use
GITHUB_TOKEN="<GITHUB_TOKEN>"
# Configure the app to build in development mode
ENV="dev"
VAPOR_ENV="dev"
#CC_SWIFT_CONFIG="debug"
LOG_LEVEL="debug"
```

{{< alert "warning" "Click on \"Update changes\"" >}}
Make sure you click on "Update changes" otherwise your environment will not be saved.
{{< /alert >}}

Writing long commands/scripts in [deployment hooks]({{< ref "/develop/build-hooks.md" >}}) is not recommended.
That's why `CC_PRE_RUN_HOOK` starts a script present in the repository.
It doesn't exist yet, so we have to create it like so:

<small>The Swift Package Index already defines `make build-front-end`, `make migrate` and `make serve-front-end`,
but they are all tightly coupled to Docker, which Clever Cloud doesn't use so have to redefine them here.</small>

```bash
mkdir scripts/clever-cloud/;
echo '#!/bin/bash

# Rename the PostgreSQL add-on exposed environment variables to the names used by the SPI
echo "DATABASE_HOST=$POSTGRESQL_ADDON_HOST
DATABASE_NAME=$POSTGRESQL_ADDON_DB
DATABASE_PASSWORD=$POSTGRESQL_ADDON_PASSWORD
DATABASE_PORT=$POSTGRESQL_ADDON_PORT
DATABASE_USERNAME=$POSTGRESQL_ADDON_USER" >> .env;
# Optional: Also set other SPI environment variables
echo "SITE_URL=${APP_ID//_/-}.cleverapps.io" >> .env;' > scripts/clever-cloud/rename-env-vars.sh;
echo '#!/bin/bash

scripts/clever-cloud/rename-env-vars.sh;
$BIN_PATH migrate --yes;' > scripts/clever-cloud/pre-run-hook.sh;
chmod u+x scripts/clever-cloud/*;
git add scripts/clever-cloud/ && git commit -m 'Add Clever Cloud pre-run hook script';
```

## Start the application

### Push your code to Clever Cloud

Go to <https://console.clever-cloud.com/users/me/applications>, select your application,
go to "Informations" then copy and run the `git remote add clever` part
of the "Push to Clever Cloud" commands.

It should look *like*:

```bash
git remote add clever git+ssh://git@push-n2-par-clevercloud-customers.services.clever-cloud.com/<APP_ID>.git
```

Then, push the `swift-5.7` branch to Clever Cloud's `master` branch:

```bash
git push clever swift-5.7:master
```

## Enjoy!

Open `<APP_ID>.cleverapps.io`, it should show a waiting screen.
After a few minutes, when the deployment succeeds, you should see the Swift Package Index appear automatically.

### While waiting for your app to build

{{< readfile "/content/partials/while-waiting-for-app-to-build.md" >}}

### Deployment failed

If your deployment fails, please
read the [Find Help section]({{< ref "/find-help/_index.md" >}}),
add a comment to the [comments section](#comments)
or [contact the support team]({{< ref "/find-help/support.md" >}}).

## Clean up

Cleaning up is important, both for the environment, your invoices and our infrastructure,
so here is how to do it:

### Delete the application

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the leftmost sidebar
3. Select your example Swift application in the main sidebar
4. Select "Information" in the secondary sidebar
5. Scroll down and click on "Remove this application"
6. Enter the name of the application and click on "Remove"

</details>

<details>
<summary>Using our CLI</summary>

To delete the linked application using our [CLI][cli], you only need to run:

```bash
clever delete
```

Then type `y` or add the `-y`/`--yes` option to the command.

</details>

### Delete the database

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the leftmost sidebar
3. Select your [managed PostgreSQL database][doc-pg-add-on] in the main sidebar
4. Select "Information" in the secondary sidebar
5. Click on "Remove add-on"
6. Enter the name of the application and click on "Remove"

</details>

<details>
<summary>Using our CLI</summary>

To delete the just-created [managed PostgreSQL database][doc-pg-add-on] using our [CLI][cli],
you only need to run:

```bash
clever addon delete '[Examples/Swift] SPI DB'
```

Then type `y` or add the `-y`/`--yes` option to the command.

</details>

### Revoke your GitHub token

Go to [github.com/settings/tokens](https://github.com/settings/tokens) and delete
the token you created in [the "Create a GitHub token" step](#create-a-github-token).

## Going further

When [setting the application's environment variables](#set-the-applications-environment-variables),
we used `CC_WORKER_COMMAND` to ingest and analyze 100 packages.
This was useful to have some realistic data, but we could push it further to ingest and analyze
all of the indexed packages.

To do this, we could use three separate [workers]({{< ref "/develop/workers.md" >}})
to reconcile the package list, ingest the packages and analyze them.
The Swift Package Index already defines `make reconcile`, `make ingest`, `make analyze`, `ingest-loop.sh`
and `analyze-loop.sh`, but they are all tightly coupled to Docker which Clever Cloud doesn't use,
so have to create simpler scripts.

You can create them by running:

```bash
mkdir scripts/simple;
echo '#!/bin/bash

export LOG_LEVEL=notice
while true; do
    echo "Reconciling package list...";
    .build/release/Run reconcile;
    echo "Reconciliation done, waiting 120 seconds...";
    sleep 120;
done' > scripts/simple/reconcile-loop.sh;
echo '#!/bin/bash

export LOG_LEVEL=notice
while true; do
    echo "Ingesting 100 packages...";
    .build/release/Run ingest --limit 100;
    echo "Ingestion done. Waiting 300 seconds...";
    sleep 300;
done' > scripts/simple/ingest-loop.sh;
echo '#!/bin/bash

export LOG_LEVEL=notice
while true; do
    echo "Analyzing 25 packages...";
    .build/release/Run analyze --limit 25;
    echo "Analysis done. Waiting 20 seconds...";
    sleep 20;
done' > scripts/simple/analyze-loop.sh;
chmod u+x scripts/simple/*;
git add scripts/simple/ && git commit -m 'Add Docker-independent `reconcile`, `ingest` and `analyze` loops';
```

<small>Values come from [SwiftPackageIndex-Server/app.yml][app.yml].</small>

Then, remove the `CC_WORKER_COMMAND` environment variable and replace it by:

```bash
CC_WORKER_COMMAND_0="scripts/simple/reconcile-loop.sh"
CC_WORKER_COMMAND_1="scripts/simple/ingest-loop.sh"
CC_WORKER_COMMAND_2="scripts/simple/analyze-loop.sh"
```

Now run `git push clever swift-5.7:master` to update your app
and connect to the instance using:

```bash
clever ssh
```

After the deployment succeeds, you will see the running workers by doing:

```bash
systemctl list-units 'run-r*'
```

Then, check the logs using

```bash
journalctl -u <UNIT> -efa
```

{{< readfile "/content/partials/journalctl-options-explained.md" >}}

<details>
<summary><code>journalctl</code> usage example</summary>

For a service named `run-r912ba7d2f1e24f468i6fc4087bf9822b.service`, you could run:

```bash
journalctl -u run-r912ba7d2f1e24f468i6fc4087bf9822b.service -efa
```

or even

```bash
journalctl -u run-r912ba7d2f1e24f468i6fc4087bf9822b -efa
```

</details>

{{< alert "secondary" "Expected error" >}}

For some reason, the analyze loop often generates

```log
[ ERROR ] Connection request timed out. This might indicate a connection deadlock in your application. If you're running long running requests, consider increasing your connection timeout. [component: server, database-id: psql]
[ ERROR ] analysis error: The operation could not be completed. (AsyncKit.ConnectionPoolTimeoutError error 0.) [component: server, database-id: psql]
```

We did not dig into the issue, as it might have been fixed anyway
(we checked out a commit from April 10, 2023).
Analysis still seems to be working, so you can leave it as is.

{{< /alert >}}

[doc-pg-add-on]: <{{< ref "/deploy/addon/postgresql/postgresql.md" >}}>
[console]: <https://console.clever-cloud.com>
[dedicated-build-instance]: <{{< ref "/administrate/apps-management.md#edit-application-configuration" >}}>
[cli]: <{{< ref "/getting-started/cli.md" >}}>
[app.yml]: <https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/ca26072f7c8efcc91dadc44647b5063c78c3f87d/app.yml> "SwiftPackageIndex-Server/app.yml at ca26072f7c8efcc91dadc44647b5063c78c3f87d · SwiftPackageIndex/SwiftPackageIndex-Server"
[gh-token-scopes]: <https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/ffe47bd98315fd63c48e6aae6da1b6adfc3833af/LOCAL_DEVELOPMENT_SETUP.md#running-reconciliation-and-ingestion-locally> "SwiftPackageIndex-Server/LOCAL_DEVELOPMENT_SETUP.md at ffe47bd98315fd63c48e6aae6da1b6adfc3833af · SwiftPackageIndex/SwiftPackageIndex-Server"
[gh-docs-pat]: <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens> "Managing your personal access tokens - GitHub Docs"
