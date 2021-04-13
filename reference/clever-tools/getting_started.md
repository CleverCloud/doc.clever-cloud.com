---
title: Getting started with Clever Cloud CLI
position: 1
shortdesc: Installing and using the Clever Cloud CLI tool
tags:
- cli
- reference
keywords:
- clever-tools
- get started
---

In addition to the Clever Cloud console, you can manage your addons and applications from the command line with Clever Tools.

## Installing Clever Tools

The clever-tools CLI can be installed through many different channels depending on your system setup.

### Via npm

If you already have node/npm on your system, you can run:

```sh
npm install -g clever-tools
```

If you want to install our latest beta release, you can run:

```sh
npm install -g clever-tools@beta
```

### On GNU/Linux

#### Debian/Ubuntu (.deb)

{{< alert "warning" "End of Bintray" >}}
<p>Bintray is ending on May 1st 2021, we will migrate hosting as soon as possible.</p>
<p>Please refer to the dedicated <a href="https://github.com/CleverCloud/clever-tools/issues/454">issue</a> to track the migration.</p>
{{< /alert >}}

If you are using a GNU/Linux distribution that uses `.deb` packages like Debian or Ubuntu, you can run:

> Make sure you can download https hosted sources package. You may install :

```sh
apt install apt-transport-https ca-certificates
```

```sh
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "379CE192D401AB61"
echo "deb https://dl.bintray.com/clevercloud/deb stable main" | tee -a /etc/apt/sources.list.d/clevercloud.list
apt update
apt install clever-tools
```

NOTES:

* The `.deb` packages are hosted on Bintray (their GPG key is required to trust their signed packages).
* If you want access to the beta channel, you can use this in your `sources.list`:

```sh
echo "deb https://dl.bintray.com/clevercloud/deb unstable beta" | tee -a /etc/apt/sources.list
```

#### CentOS/Fedora (.rpm)

{{< alert "warning" "End of Bintray" >}}
<p>Bintray is ending on May 1st 2021, we will migrate hosting as soon as possible.</p>
<p>Please refer to the dedicated <a href="https://github.com/CleverCloud/clever-tools/issues/454">issue</a> to track the migration.</p>
{{< /alert >}}

If you are using a GNU/Linux distribution that uses `.rpm` packages like CentOS or Fedora, you can run:

```sh
curl https://bintray.com/clevercloud/rpm/rpm > /etc/yum.repos.d/bintray-clevercloud-rpm.repo
echo "exclude=*beta*" >> /etc/yum.repos.d/bintray-clevercloud-rpm.repo
yum install clever-tools
```

NOTES:

* The `.rpm` packages are hosted on Bintray.
* If you want access to the beta channel, you can omit the second line which contains an exclude option.

#### Arch Linux

If you are using Arch Linux, the packages can be installed from AUR with this repo: [clever-tools-bin](https://aur.archlinux.org/packages/clever-tools-bin/).
If you don't know how to use this, you can run:

```sh
git clone https://aur.archlinux.org/clever-tools-bin.git clever-tools
cd clever-tools
makepkg -si
```

NOTES:

* If you want access to the beta channel, you can use this repo [clever-tools-bin-beta](https://aur.archlinux.org/packages/clever-tools-bin-beta/).

#### Exherbo

If you are using Exherbo, you can run:

```sh
cave resolve repository/CleverCloud -zx1
cave resolve clever-tools-bin -zx
```

#### Other distributions (.tar.gz)

If you are using another GNU/Linux distribution, you can download a `.tar.gz` archive and extract the binary in your `PATH`:

```sh
wget https://clever-tools.clever-cloud.com/releases/latest/clever-tools-latest_linux.tar.gz
tar xvzf clever-tools-latest_linux.tar.gz
cp clever-tools-latest_linux/clever ~/.local/bin/
```

NOTES:

* The packages are available on Clever Cloud's Cellar bucket: [clever-tools-latest_linux.tar.gz](https://clever-tools.clever-cloud.com/releases/latest/clever-tools-latest_linux.tar.gz).
* You can also retrieve any release (including beta) on this Cellar bucket by replacing `latest` (path and filename) with the version number you need.

### On MacOS

#### Using homebrew

If you are using MacOS and you have [homebrew](https://brew.sh) installed, you can run:

```sh
brew install CleverCloud/homebrew-tap/clever-tools
```

NOTES:

* If you want access to the beta channel, you can use `CleverCloud/homebrew-tap-beta/clever-tools` instead.

#### Using the `.tar.gz` archive

If you are using MacOS but you don't have [homebrew](https://brew.sh) installed, you can download a `.tar.gz` archive and extract the binary in your `PATH`:

```sh
curl https://clever-tools.clever-cloud.com/releases/latest/clever-tools-latest_macos.tar.gz
tar xvzf clever-tools-latest_linux.tar.gz
cp clever-tools-latest_linux/clever ~/.local/bin/
```

NOTES:

* The packages are available on Clever Cloud's Cellar bucket: [clever-tools-latest_macos.tar.gz](https://clever-tools.clever-cloud.com/releases/latest/clever-tools-latest_macos.tar.gz).
* You can also retrieve any release (including beta) on this Cellar bucket by replacing `latest` (path and filename) with the version number you need.

### On Windows

#### Using chocolatey

{{< alert "warning" "End of Bintray" >}}
<p>Bintray is ending on May 1st 2021, we will migrate hosting as soon as possible.</p>
<p>Please refer to the dedicated <a href="https://github.com/CleverCloud/clever-tools/issues/454">issue</a> to track the migration.</p>
{{< /alert >}}

If you are using Windows and you have [chocolatey](https://chocolatey.org) installed, you can run: 

```bash
choco sources add -n=clevercloud -s='https://api.bintray.com/nuget/clevercloud/nupkg'
choco install clever-tools
```

NOTES:

* If you want access to the beta channel, you can use `choco install --pre clever-tools` instead.

#### Using the `.zip` archive

If you are using Windows but you don't have [chocolatey](https://chocolatey.org) installed, you can download a `.zip` archive and extract the binary in your `PATH`.

NOTES:

* The packages are available on Clever Cloud's Cellar bucket: [clever-tools-latest_win.tar.gz](https://clever-tools.clever-cloud.com/releases/latest/clever-tools-latest_win.zip).
* You can also retrieve any release (including beta) on this Cellar bucket by replacing `latest` (path and filename) with the version number you need.

## Enabling autocompletion

The clever-tools CLI comes with a comprehensive auto-completion system.
Some installation methods like `.deb` packages, `.rpm` packages or brew will try to enable it automatically.
If it does not work, try this for bash: 

```bash
clever --bash-autocomplete-script $(which clever) | sudo tee /usr/share/bash-completion/completions/clever
```

or this for zsh:

```bash
clever --zsh-autocomplete-script $(which clever) | sudo tee /usr/share/zsh/site-functions
```

## Linking your account

To use `clever-tools`, you have to login.

```sh
clever login
```

It will open the Web console in your browser and reuse your existing session if you're already logged in.

`clever login` tries to open a browser through `xdg-open` on GNU/Linux systems (and in bash for windows).
Make sure you have `xdg-utils` available as well as a default browser set (or you can copy and paste the URL displayed in the console.

## Linking an existing application

If you have an already existing application, you can start managing it with
Clever Tools.

```sh
# First, go to your local repository
cd /path/to/your/application

# Then, link it to the Clever Cloud application
clever link <app_id>

# You can also use the application name (make sure to specify the
# organization name if your application is in an organization.
clever link --org <org_name> <app_name>

# Unlink an application
clever unlink <app_id>
```

{{< alert "info" "Notes on `.clever.json`" >}}
    <div>
      Once you have linked an application, clever-tools will create a Json configuration file named `.clever.json` at the root of the directory.
    </div>
    <div>This file can be commited safely, others team members working on this repository will not have to link the application again.</div>
    <div>This configuration file contains the AppID, so keep this in mind if you publish it on a public repository.</div>
{{< /alert >}}

<br>

## Deploying new code

After having written new code, you can deploy it to Clever Cloud

```sh
# Will git push your code to Clever Cloud and display logs
clever deploy

# Will open your application
clever open
```
