---
title: "Example Swift deployment: Hummingbird multipart form decoding"
shortdesc: "The deployment of a Swift application on Clever Cloud: Hummingbird's multipart form decoding example"
tags:
- deploy
- example
- swift
- hummingbird
keywords:
- swift
- hummingbird
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
git clone https://github.com/hummingbird-project/hummingbird-examples
cd hummingbird-examples/
```

## Create the application

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com][console]
2. Select your personal space or organization in the left sidebar
3. Hit "Create…" then "an application"
4. Select "Create a brand new app"
5. Select "Swift"
6. Edit the scalability and choose a "pico" instance size (it is enough for this example) then hit "Next"
7. Give your application a name (e.g. "\[Examples/Swift\] Hummingbird Multipart Form decoding"),
   choose a zone then hit "Create"
8. Click "I don't need any add-on", as we won't need one

</details>

<details>
<summary>Using our CLI</summary>

{{< readfile "/content/partials/cli-first-time-use.md" >}}

```bash
clever create --type swift '[Examples/Swift] Hummingbird Multipart Form decoding'
clever scale --flavor pico
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

The project builds on very small scalers, but it takes a while
(e.g. 12 minutes on a nano scaler).
By using a [dedicated build instance][dedicated-build-instance],
your build will be a lot faster (e.g. 2.5 minutes on a XL scaler).
Therefore, if you use a small instance such as `pico` as we recommended,
we recommend you configure a [dedicated build instance][dedicated-build-instance]
with a flavor `S` or `M` at least.
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

### Set the application's environment variables

<details open="true">
<summary>Using the web console</summary>

Go to <https://console.clever-cloud.com/users/me/applications>, select your application,
go to "Environment variables", select the "Expert" mode then paste the following environment:

```bash
# Select the example to deploy
APP_FOLDER="multipart-form"
# Add `--hostname` option to the binary invocation
# NOTE: Listening to 0.0.0.0 is a requirement on Clever Cloud
CC_SWIFT_BIN_ARGS="--hostname 0.0.0.0"
```

{{< alert "warning" "Click on \"Update changes\"" >}}
Make sure you click on "Update changes" otherwise your environment will not be saved.
{{< /alert >}}

</details>

<details>
<summary>Using our CLI</summary>

Using our CLI, you only need to run:

```bash
echo 'APP_FOLDER="multipart-form"
CC_SWIFT_BIN_ARGS="--hostname 0.0.0.0"' > .env.clever;
cat .env.clever | clever env import;
```

<details>
<summary>Make sure you ignore <code>.env.clever</code> from git</summary>

`.env.clever` does not contain any sensitive information in this example,
but it is a good practice to ignore it.
If it's not already ignored by more global settings, you can run:

```bash
echo '.env.clever' >> .gitignore
git add .gitignore
git commit -m 'Ignore `.env.clever`'
```

</details>

</details>

## Start the application

### Push your code to Clever Cloud

Go to <https://console.clever-cloud.com/users/me/applications>, select your application,
go to "Informations" then copy and run the "Push to Clever Cloud" commands.

You can also run `clever deploy` which does it for you.

## Enjoy!

Open `<APP_ID>.cleverapps.io`, it should show a waiting screen.
After a few minutes, when the deployment succeeds, you should see the HTML form appear automatically.

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

[dedicated-build-instance]: <{{< ref "/administrate/apps-management.md#edit-application-configuration" >}}>
[console]: <https://console.clever-cloud.com>
[cli]: <{{< ref "/getting-started/cli.md" >}}>
