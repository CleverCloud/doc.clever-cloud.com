---
title: "Example Swift deployment: the Swift Package Index"
shortdesc: "The deployment of a real-world Swift application on Clever Cloud: the Swift Package Index"
tags:
- deploy
- example
keywords:
- swift
- postgresql
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

## Create the application

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
2. Select your personal space or organization in the left sidebar
3. Hit "Create…" then "an application"
4. Select "Create a brand new app"
5. Select "Swift"
6. Edit the scalability and choose a "nano" instance size (it is enough for this example) then hit "Next"
7. Link your newly created database to the "\[Examples/Swift\] SPI" app
   (or whatever you called it) then hit "Next"
8. Give your database a name (e.g. "\[Examples/Swift\] SPI DB"),
   choose a zone then hit "Create"

</details>

<details>
<summary>Using our CLI</summary>

{{< alert "info" "First time use of Clever Cloud's CLI" >}}
If you don't have access to Clever Cloud's CLI — `clever`, intall `clever-tools`
by following the installation guidelines in
[github.com/CleverCloud/clever-tools][clever-tools-install].

Then, log into your account using `clever login`[^clever-login].

[^clever-login]: More details in [github.com/CleverCloud/clever-tools][clever-tools-login].

[clever-tools-install]: <https://github.com/CleverCloud/clever-tools/#installation>
[clever-tools-login]: <https://github.com/CleverCloud/clever-tools/#login>
{{< /alert >}}

```bash
clever create --type swift '[Examples/Swift] SPI'
clever scale --flavor nano
```

Creating an application from the CLI also runs `clever link`,
which creates a `.clever.json` in your repository.

If you are okay with pushing this little config file into the repository, it's safe to do,
but you can also ignore it from your commits using `echo '.clever.json' >> .gitignore`
and then `git commit -m 'Ignore Clever Cloud configuration file' -- .gitignore`.

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
{{< /alert >}}

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
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

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
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
CC_POST_BUILD_HOOK="yarn && yarn build"
CC_PRE_RUN_HOOK=".build/debug/Run migrate --yes"
CC_RUN_COMMAND=".build/debug/Run serve --hostname 0.0.0.0"
CC_RUN_SUCCEEDED_HOOK="bin='.build/debug/Run'; $bin reconcile && $bin ingest --limit 100 && $bin analyze --limit 100"
# Rename the PostgreSQL add-on exposed environment variables to the names used by the SPI
DATABASE_HOST="$POSTGRESQL_ADDON_HOST"
DATABASE_NAME="$POSTGRESQL_ADDON_DB"
DATABASE_PASSWORD="$POSTGRESQL_ADDON_PASSWORD"
DATABASE_PORT="$POSTGRESQL_ADDON_PORT"
DATABASE_USERNAME="$POSTGRESQL_ADDON_USER"
# Set a GitHub token for SPI to use
GITHUB_TOKEN="<GITHUB_TOKEN>"
# Optional: Set other SPI environment variables
SITE_URL="${APP_ID//_/-}.cleverapps.io"
# Configure the app to run in dev mode
CC_SWIFT_CONFIG="debug"
ENV="dev"
LOG_LEVEL="debug"
VAPOR_ENV="dev"
```

{{< alert "warning" "Click on \"Update changes\"" >}}
Make sure you click on "Update changes" otherwise your environment will not be saved.
{{< /alert >}}

{{< alert "info" "Prefer running scripts in hooks" >}}
Writing long commands in hooks is not recommended.
Instead, you can define your commands in a `Makefile` or dedicated scripts (e.g. Bash)
and run them from the hooks.
You can find more information in [Develop > Deployment Hooks]({{< ref "/develop/build-hooks.md" >}}).

The Swift Package Index already defines `make build-front-end`, `make migrate` and `make serve-front-end`,
but they are all tightly coupled to Docker, which Clever Cloud doesn't use so have to redefine them here.
{{< /alert >}}

## Start the application

### Clone the repository

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
{{< /alert >}}

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

Wait a few minutes, then You should be able to 

### While waiting for your app to build

While waiting for your app to build, it can be the perfect time to dicover
how you can see your build/deployment logs on Clever Cloud.

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
2. Select your personal space or organization in the leftmost sidebar
3. Select your example Swift application in the main sidebar
4. Select "Logs" in the secondary sidebar

</details>

<details>
<summary>Using our CLI</summary>

To create a [managed PostgreSQL database][doc-pg-add-on]
and link it to your just-created Swift application
using our [CLI][cli], you only need to run:

```bash
clever addon create postgresql-addon "[Examples/Swift] SPI DB" \
    --link "[Examples/Swift] SPI" \
    --plan dev
```

{{< alert "info" "Custom add-on owner" >}}
If you need your add-on to be created outside of your personal space,
you can add the `--owner <OWNER_ID>` argument to the above command
(replacing `<OWNER_ID>` by your unique organization identifier (`orga_…`)).
{{< /alert >}}

</details>

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

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
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

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
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
we used `CC_RUN_SUCCEEDED_HOOK` to ingest and analyze 100 packages.
This was useful to have some realistic data, but we could push it further to ingest and analyze
all of the indexed packages.

To do this, we could remove `CC_RUN_SUCCEEDED_HOOK` and replace it by:

<small>Values come from [SwiftPackageIndex-Server/app.yml][app.yml].</small>

```bash
CC_WORKER_COMMAND_0="# Reconcile loop
export LOG_LEVEL=notice
while true; do
    echo 'Reconciling package list...';
    .build/debug/Run reconcile;
    echo 'Reconciliation done, waiting 120 seconds...';
    sleep 120;
done"
CC_WORKER_COMMAND_1="# Ingest loop
export LOG_LEVEL=notice
while true; do
    echo 'Ingesting 100 packages...';
    .build/debug/Run ingest --limit 100;
    echo 'Ingestion done. Waiting 300 seconds...';
    sleep 300;
done"
CC_WORKER_COMMAND_2="# Analyze loop
export LOG_LEVEL=notice
while true; do
    echo 'Analyzing 25 packages...';
    .build/debug/Run analyze --limit 25;
    echo 'Analysis done. Waiting 20 seconds...';
    sleep 20;
done"
```

{{< alert "info" "Prefer running scripts in workers" >}}
Writing long commands in workers is not recommended.
Instead, you can define your commands in a `Makefile` or dedicated scripts (e.g. Bash)
and run them from the workers.
You can find more information in [Develop > Workers]({{< ref "/develop/workers.md" >}}).

Worker environment variables are escaped for security reasons,
which means you can't use variables inside of them.
Using predefined scripts has the added benefit of removing this limitation.

The Swift Package Index already defines `make reconcile`, `make inges`, `make analyze` and `ingest-loop.sh`,
but they are all tightly coupled to Docker, which Clever Cloud doesn't use so have to redefine them here.

We could have committed new scripts, but another goal of this tutorial is to highlight
the fact that deploying on Clever Cloud can have literally no impact on your repository.
We will not lock you in with some proprietary code.
{{< /alert >}}

{{< alert "secondary" "Custom add-on owner" >}}
If you need your add-on to be created outside of your personal space,
you can add the `--owner <OWNER_ID>` argument to the above command
(replacing `<OWNER_ID>` by your unique organization identifier (`orga_…`)).
{{< /alert >}}

Now restart the app, and connect to the instance using:

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

<details>
<summary><code>journalctl</code> options explained</summary>

- `-u <UNIT>`: Show logs from the specified unit
- `-e`: Immediately jump to the end of the logs
- `-f`: Follow new logs
- `-a`: Show all fields, including long and unprintable

</details>

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

[doc-app-scaling]: <{{< ref "/administrate/scalability.md" >}}>
[doc-pg-add-on]: <{{< ref "/deploy/addon/postgresql/postgresql.md" >}}>
[console]: <https://console.clever-cloud.com>
[dedicated-build-instance]: <{{< ref "/administrate/apps-management.md#edit-application-configuration" >}}>
[cli]: <{{< ref "/getting-started/cli.md" >}}>
[swift-5.8-bootstrapping]: <https://forums.swift.org/t/implementing-parts-of-the-swift-compiler-in-swift/59524> "Implementing Parts of the Swift Compiler in Swift - Development / Compiler - Swift Forums"
[app.yml]: <https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/ca26072f7c8efcc91dadc44647b5063c78c3f87d/app.yml> "SwiftPackageIndex-Server/app.yml at ca26072f7c8efcc91dadc44647b5063c78c3f87d · SwiftPackageIndex/SwiftPackageIndex-Server"
[gh-token-scopes]: <https://github.com/SwiftPackageIndex/SwiftPackageIndex-Server/blob/ffe47bd98315fd63c48e6aae6da1b6adfc3833af/LOCAL_DEVELOPMENT_SETUP.md#running-reconciliation-and-ingestion-locally> "SwiftPackageIndex-Server/LOCAL_DEVELOPMENT_SETUP.md at ffe47bd98315fd63c48e6aae6da1b6adfc3833af · SwiftPackageIndex/SwiftPackageIndex-Server"
[gh-docs-pat]: <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens> "Managing your personal access tokens - GitHub Docs"
