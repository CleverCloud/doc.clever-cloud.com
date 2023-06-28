---
title: Environment Variable Reference
position: 3
shortdesc: List of all the environment variable references
tags:
- reference
keywords:
- env
- env variables
---

## Commons to all applications

### Set by the deployment process

These are read-only variables that are generated for each scaler before they build and start your application.

{{<table "table table- bordered" "text-align : center" >}}
| <center>Name</center> | <center>Description</center> | <center>Example value</center> |
|-----------------------|------------------------------|--------------------------------|
|[INSTANCE_NUMBER]({{< ref "develop/env-variables.md" >}}) | Allows your application to differentiate each running node on the application level. | 0, 1… |
|INSTANCE_TYPE | Whether this instance is a "build" instance or a "production" instance. | build, production |
|INSTANCE_ID | The ID of the current instance (scaler) of your application. It's unique for each instance of your application and changes every time you deploy it. | 518c8d8f-e304-4310-81e0-9c4237d55645 |
|CC_PRETTY_INSTANCE_NAME | The random generated string as instance pretty name using pokemon names. | Tiny rhyhorn |
|APP_ID | The ID of your Clever Cloud application | app_649a93d1-6677-44bc-aca7-6f46107d6e02 |
|APP_HOME | The absolute path to your application folder | /home/bas/app_649a93d1-6677-44bc-aca7-6f46107d6e02 |
|CC_DEPLOYMENT_ID | Internal id of current deployment | f7efaf04-1a63-45a1-8503-0de7c750ee48 |
|COMMIT_ID | The id of the commit that's currently running | d88cd2ae1aaa91923ed2bd689d95d713b6f3f45f |
|CC_REVERSE_PROXY_IPS | A comma separated list of trusted IP addresses. You should only accept requests  coming from these IP addresses. | x.y.z.z,x.y.z.z |
|ELASTIC_APM_SERVICE_NAME | Sets the name of your service/application in Elastic APM. Automatically defined when you have linked an Elastic APM service to your application. You can override it by defining it yourself | Your application's name conforming to Elastic APM naming convention |
|CC_APP_NAME | The customer defined application name | cloud-api-production |
|PORT | The mandatory port value is 8080 | 8080 |
{{< /table >}}

### Variables you can define

So you can alter the build&start process for your application.

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> |
|-----------------------|------------------------------|--------------------------------|
|APP_FOLDER | Folder in which the application is located (inside the git repository) |  |
|[CC_TROUBLESHOOT]({{< ref "find-help/troubleshooting.md" >}}) | Enable debug log level, will also keep the VM up after failure for 15 minutes so you can SSH and debug. Don't forget to cancel deployment if you push a new commit. | `false` |
|[CC_WORKER_COMMAND]({{< ref "develop/workers.md" >}}) | Command to run in background as a worker process. You can run multiple workers. |  |
|CC_WORKER_RESTART | One of `always`, `on-failure` or `no`. Control whether workers need to be restarted when they exit.<br />This setting controls all workers. | `on-failure` |
|CC_WORKER_RESTART_DELAY | Define a delay in seconds to restart the worker when they exit. | `1` |
|[CC_PRE_BUILD_HOOK]({{< ref "develop/build-hooks.md#pre-build-cc_pre_build_hook" >}}) | Ran before the dependencies are fetched. If it fails, the deployment fails. |  |
|[CC_POST_BUILD_HOOK]({{< ref "develop/build-hooks.md#pre-build-cc_post_build_hook" >}}) | Ran after the project is built, and before the cache archive is generated. If it fails, the deployment fails. |  |
|[CC_PRE_RUN_HOOK]({{< ref "develop/build-hooks.md#pre-run-cc_pre_run_hook" >}}) | Ran before the application is started, but after the cache archive has been generated. If it fails, the deployment fails. |  |
|[CC_RUN_SUCCEEDED_HOOK]({{< ref "develop/build-hooks.md#run-succeeded-cc_run_succeeded_hook-or-failed-cc_run_failed_hook" >}}) | Ran once the application has started successfuly. |  |
|[CC_RUN_FAILED_HOOK]({{< ref "develop/build-hooks.md#run-succeeded-cc_run_succeeded_hook-or-failed-cc_run_failed_hook" >}}) | Ran once the application has failed to start. |  |
|CC_RUN_COMMAND | Custom command to run your application. |  |
|CC_TASK | If set as true, the deployer runs `CC_RUN_COMMAND` and close the instance after havind run the task. Trigger an execution using `git push` or starting your instance  | `false` |
|CC_CACHE_DEPENDENCIES | Enable caching of your build dependencies to speed up following builds. | `false` |
|CC_SSH_PRIVATE_KEY | A ssh private key to setup for the user running your application |  |
|CC_SSH_PRIVATE_KEY_FILE | The name to use for the file containing the private ssh key | `id_ed25519` |
|CC_DISABLE_METRICS | Disable metrics collection. | `false` |
|[IGNORE_FROM_BUILDCACHE]({{< ref "develop/env-variables.md#settings-you-can-define-using-environment-variables" >}}) | Allows to specify paths to ignore when the build cache archive is created. |  |
|[CC_OVERRIDE_BUILDCACHE]({{< ref "develop/env-variables.md#settings-you-can-define-using-environment-variables" >}}) | Allows to specify paths that will be in the build cache. <br />Only those files / directories will be cached |  |
|[CC_METRICS_PROMETHEUS_PORT]({{< ref "administrate/metrics/overview.md#publish-your-own-metrics" >}}) | Define the port on which the Prometheus endpoint is available | `8080` |
|[CC_METRICS_PROMETHEUS_PATH]({{< ref "administrate/metrics/overview.md#publish-your-own-metrics" >}}) | Define the path on which the Prometheus endpoint is available | `/metrics` |
|[CC_METRICS_PROMETHEUS_USER]({{< ref "administrate/metrics/overview.md#publish-your-own-metrics" >}}) | Define the user for the basic auth of the Prometheus endpoint | |
|[CC_METRICS_PROMETHEUS_PASSWORD]({{< ref "administrate/metrics/overview.md#publish-your-own-metrics" >}}) | Define the password for the basic auth of the Prometheus endpoint | |
|[CC_METRICS_PROMETHEUS_RESPONSE_TIMEOUT]({{< ref "administrate/metrics/overview.md#publish-your-own-metrics" >}}) | Define the timeout in seconds to collect the application metrics. This value **must** be below 60 seconds as data are collected every minutes | `3` |
|[CC_CLAMAV]({{< ref "administrate/clamav.md" >}}) | Start the clamav and clamav-freshclam services (the database is updated every 2 hours). WARNING: Clamscan consumes a lot of resources (~ 1GB of memory), make sure you have a scaler with enough memory to avoid OOM. | `false` |
|CC_NODE_VERSION| Set Node.js version on non-Node.js application. Don't use it for Node.js applications, use [this](https://www.clever-cloud.com/doc/deploy/application/javascript/by-framework/nodejs/#select-node-version) instead | |
|[CC_VARNISH_STORAGE_SIZE]({{< ref "administrate/cache.md" >}}) | Configure the size of the Varnish cache. | `1G` |
|CC_DISABLE_GIT_SUBMODULES | Disable Git submodules initialization & synchronization | |
{{< /table >}}

### Tailscale support

[Tailscale](https://tailscale.com/) is a managed VPN service based on Wireguard that enable private networking between users, devices or machines. Clever Cloud provides a native integration of Tailscale, by mounting a VPN endpoint for each of your application's instances. 

Note that `Reusable keys` are required to use multiple instances. You can [generate one here](https://login.tailscale.com/admin/settings/keys).

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|[TAILSCALE_AUTH_KEY](https://tailscale.com/) | Contains your Tailscale Auth key |  |  |
|TAILSCALE_LOGIN_SERVER| Contains the login server |  |  |
{{< /table >}}

#### How it works?

For a given application with `TAILSCALE_AUTH_KEY` configured, each instance will be configured to join a Tailscale network. Instances will be named after your configured name, suffixed with the [INSTANCE_NUMBER]({{< ref "develop/env-variables.md" >}}) : `CC-<NAME>-<INSTANCE_NUMBER>`. If you have multiple instances and use one of them for being an admin instance (using [INSTANCE_NUMBER]({{< ref "develop/env-variables.md" >}})), you can match the instance from your deployment to reach it over VPN.

If `TAILSCALE_LOGIN_SERVER` is provided, the agent will be configured to reach an alternative control server. Note that using your own control server is at your own risks, and Tailscale can't be responsible. An alternative control server can still be useful to use for constraints environements. [Headscale](https://github.com/juanfont/headscale/) is an example of self-hosted implementation of the Tailscale control server that can run on Clever Cloud.


## Docker

[Docker Documentation]({{< ref "deploy/application/docker/docker.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_DOCKERFILE | The name of the Dockerfile to build. | `Dockerfile` |  |
|CC_MOUNT_DOCKER_SOCKET | Set to true to access the host Docker socket from inside your container. | `false` |  |
|CC_DOCKER_EXPOSED_HTTP_PORT | Set to custom HTTP port if your Docker container runs on custom port. | `8080` |  |
|CC_DOCKER_EXPOSED_TCP_PORT | Set to custom TCP port if your Docker container runs on custom port. | `4040` |  |
|CC_DOCKER_FIXED_CIDR_V6 | Activate the support of IPv6 with an IPv6 subnet int the docker daemon. |  |  |
|CC_DOCKER_LOGIN_USERNAME | The username to login to a private registry. |  |  |
|CC_DOCKER_LOGIN_PASSWORD | The password of your username. |  |  |
|CC_DOCKER_LOGIN_SERVER | The server of your private registry (optional). | `Docker’s public registry` |  |
{{< /table >}}

## .NET

[.NET Documentation]({{< ref "deploy/application/dotnet/dotnet.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_DOTNET_VERSION | Choose the .NET Core version between `5.0`,`6.0`. | 6.0 |  |
|CC_DOTNET_PROJ | The name of your project file to use for the build, without the .csproj / .fsproj / .vbproj extension. |  |  |
|CC_DOTNET_TFM | Compiles for a specific framework. The framework must be defined in the project file. Example : `net5.0` |  |  |
|CC_DOTNET_PROFILE | Override the build configuration settings in your project. | Release |  |
|CC_RUN_COMMAND | Custom command to run your application. |  |  |
{{< /table >}}

## Elixir

[Elixir Documentation]({{< ref "deploy/application/elixir/elixir.md" >}})

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |CC_ELIXIR_VERSION | Choose the Elixir version between `1.8`, `1.9`, `1.10`, `1.11` or `1.12` | `1.11` |  |
 |CC_MIX_BUILD_GOAL | The mix goal to build the application (default compile) |  |  |
 |CC_PHOENIX_ASSETS_DIR | Folder in which your Phoenix assets are located. |  |  |
 |CC_PHOENIX_DIGEST_GOAL | Phoenix digest goal. | phx.digest |  |
 |CC_PHOENIX_SERVER_GOAL | Phoenix server goal. | phx.server |  |
 |CC_PHOENIX_RUN_ECTO_MIGRATE | Whether to run 'mix ecto.migrate' or not. | true |  |
 |CC_RUN_COMMAND | Custom command to run your application. Replaces the default behaviour. |  |  |
 {{< /table >}}

## Go

[Go Documentation]({{< ref "deploy/application/golang/go.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_GO_PKG | Makes the deployer run go get `${CC_GO_PKG}` instead of go get `<app_id>`.  |  |  |
|CC_GO_BUILD_TOOL |Available values: `gomod`, `gobuild`, `goget`. Makes the deployer use `go modules`, `go get` or `go build` to build your application. |`goget` | |
|CC_GO_RUNDIR | Makes the deployer use the specified directory to run your binary.<br>If your application must be in `$GOPATH/src/company/project` for your vendored dependencies, set this variable to `company/project` |  | |
{{< /table >}}

## Haskell

[Haskell Documentation]({{< ref "deploy/application/haskell/haskell.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_RUN_COMMAND | Custom command to run your application. |  |  |
{{< /table >}}

## Java

[Java Documentation]({{< ref "deploy/application/java/java-jar.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_SBT_TARGET_DIR | Define where pick the bin to run. | `.` | Then `/target/universal/stage/bin` is concatenated. |
|CC_SBT_TARGET_BIN | Define the bin to pick in the `CC_SBT_TARGET_DIR`. | The first bin found in the `CC_SBT_TARGET_DIR`. |  |
|GRADLE_DEPLOY_GOAL | Define which gradle goals to run during build. |  |  |
|CC_JAVA_VERSION | Choose the JVM version between `7` to `17` for OpenJDK or `graalvm-ce` for GraalVM 21.0.0.2 (based on OpenJDK 11.0). | `11` |  |
|MAVEN_DEPLOY_GOAL | Define which maven goals to run during build. |  |  |
|CC_MAVEN_PROFILES | Define which maven profile to use during default build. |  |  |
|NUDGE_APPID |  |  |  |
|PLAY1_VERSION | Define which play1 version to use between `1.2`, `1.3`, `1.4` and `1.5` |  |  |
|SBT_DEPLOY_GOAL | Define which sbt goals to run during build. | `stage` |  |
|CC_JAR_PATH | Define the path to your jar. |  |  |
|CC_EXTRA_JAVA_ARGS | Define extra arguments to pass to 'java' for jars. |  |  |
|CC_JAR_ARGS | Define arguments to pass to the jar we launch. |  |  |
|CC_RUN_COMMAND | Custom command to run your application. Replaces the default behaviour. |  |  |
|CC_DISABLE_MAX_METASPACE | Allows to disable the Java option -XX:MaxMetaspaceSize |  |  |
{{< /table >}}

## Node.js

[Node.js Documentation]({{< ref "deploy/application/javascript/by-framework/nodejs.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_NODE_DEV_DEPENDENCIES | Control if development dependencies are installed or not. Values are either `install` or `ignore` | `ignore` |  |
|CC_RUN_COMMAND | Define a custom command. | Example for Meteor: `node .build/bundle/main.js <options>` |  |
|CC_NODE_BUILD_TOOL | Choose your build tool between `npm`, `npm-ci`, `yarn`, `yarn2` and `custom` | `npm` |  |
|CC_CUSTOM_BUILD_TOOL| A custom command to run (with `CC_NODE_BUILD_TOOL` set to `custom`) | | |
|CC_NPM_REGISTRY | The host of your private repository, available values: `github` or the registry host. | registry.npmjs.org |  |
|NPM_TOKEN | Private repository token |  |  |
{{< /table >}}

## PHP

[PHP Documentation]({{< ref "deploy/application/php/php-apps.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|ALWAYS_POPULATE_RAW_POST_DATA |  |  |  |
|CC_LDAP_CA_CERT |  |  |  |
|CC_WEBROOT | Define the `DocumentRoot` of your project | . |  |
|LDAPTLS_CACERT |  |  |  |
|ENABLE_ELASTIC_APM_AGENT | Elastic APM Agent for PHP | `true` if `ELASTIC_APM_SERVER_URL` is defined, `false` otherwise | |
|ENABLE_REDIS |  | `false` |  |
|HTTP_TIMEOUT | Define a custom HTTP timeout | `180` |  |
|MAX_INPUT_VARS |  |  |  |
|MEMORY_LIMIT | Change the default memory limit |  |  |
|CC_PHP_VERSION | Choose your PHP version between `5.6`, `7.2`, `7.3`, `7.4`, `8.0`, `8.1` and `8.2` | `7` |  |
|CC_COMPOSER_VERSION | Choose your composer version between `1` and `2` | `2` |  |
|[CC_PHP_DEV_DEPENDENCIES]({{< ref "deploy/application/php/php-apps.md#development-dependencies" >}}) | Control if development dependencies are installed or not. Values are either `install` or `ignore` |  |  |
|[CC_CGI_IMPLEMENTATION]({{< ref "deploy/application/php/php-apps.md#development-dependencies" >}}) | Choose the Apache FastCGI module between `fastcgi` and `proxy_fcgi` | `fastcgi` |  |
|[SESSION_TYPE]({{< ref "deploy/application/php/php-apps.md#use-redis-to-store-php-sessions" >}}) | Choose `redis` to use it as session store |  |  |
|SOCKSIFY_EVERYTHING |  |  |  |
|USE_SOCKS |  | `false` |  |
|CC_OPCACHE_MEMORY | Set the shared opcache memory size | Default is about 1/8 of the RAM |  |
|CC_OPCACHE_MAX_ACCELERATED_FILES | Maximum number of files handled by opcache. | Default depends on the scaler size |  |
|CC_OPCACHE_INTERNED_STRINGS_BUFFER | The amount of memory used to store interned strings, in megabytes. | Default 4 (PHP5), 8 (PHP7) |  |
|CC_OPCACHE_PRELOAD | The path of the PHP preload file (PHP version 7.4 or higher). |  |  |
|CC_MTA_SERVER_HOST | Host of the SMTP server |  |  |
|CC_MTA_SERVER_PORT | Port of the SMTP server | 465 |  |
|CC_MTA_AUTH_USER | User to authenticate to the SMTP server |  |  |
|CC_MTA_AUTH_PASSWORD | Password to authenticate to the SMTP server |  |  |
|CC_MTA_SERVER_USE_TLS | Enable or disable TLS when connecting to the SMTP server | true |  |
|CC_MTA_SERVER_AUTH_METHOD | Enable or disable authentication to the SMTP server | on |  |
|CC_REALPATH_CACHE_TTL | The size of the realpath cache to be used by PHP | 120 |  |
|SQREEN_API_APP_NAME | The name of your sqreen application. |  |  |
|SQREEN_API_TOKEN | Organization token. |  |  |
|CC_HTTP_BASIC_AUTH | Restrict HTTP access to your application. Example: `login:password`. You can define multiple credentials using additional `CC_HTTP_BASIC_AUTH_n` (where `n` is a number) environment variables. |  |  |
{{< /table >}}

## Python

[Python Documentation]({{< ref "deploy/application/python/python_apps.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_PIP_REQUIREMENTS_FILE | Allows you to define a custom `requirements.txt` file  | `requirements.txt`  |  |
|CC_PYTHON_CELERY_LOGFILE | Relative path to your Celery logfile: `/path/to/logdir`  |  |  |
|CC_PYTHON_CELERY_MODULE | Specify the Celery module you want to start: `mymodule` |  |  |
|CC_PYTHON_CELERY_USE_BEAT | Set to `true` to activate Beat support |  |  |
|CC_PYTHON_MODULE | Select which module you want to start: `mymodule:app`. 'mymodule' refers to the path to the folder containing the app object. So a module called 'server.py' in a folder called 'app' would be used here as `app.server:app` |  |  |
|[CC_PYTHON_MANAGE_TASKS]({{< ref "deploy/application/python/tutorials/python-django-sample.md#manage-py-tasks" >}}) | Comma-separated list of Django manage tasks |  |  |
|CC_PYTHON_USE_GEVENT | Set to `true` to enable Gevent |  |  |
|HARAKIRI | Timeout (in seconds) after which an unresponding process is killed | `180` |  |
|CC_PYTHON_BACKEND | Choose the Python backend to use between `daphne`, `gunicorn`, `uvicorn` and `uwsgi` | `uwsgi` |  |
|CC_PYTHON_VERSION | Choose the Python version between `2.7`, `3.7`, `3.8`, `3.9`, `3.10` and `3.11` |  |  |
|PYTHON_SETUP_PY_GOAL | Custom setup goal to be launch after `requirements.txt` have been installed |  |  |
|STATIC_FILES_PATH | Relative path to where your static files are stored: `path/to/static` |  |  |
|[STATIC_URL_PREFIX]({{< ref "deploy/application/python/python_apps.md#configure-your-python-application" >}}) | The URL path under which you want to serve static file, usually `/public` |  |  |
|STATIC_WEBROOT |  |  |  |
|UWSGI_INTERCEPT_ERRORS |  |  |  |
|[UWSGI_ASYNC]({{< ref "deploy/application/python/python_apps.md#configure-your-python-application" >}}) | Number of cores to use for uWSGI asynchronous/non-blocking modes |  |  |
|UWSGI_ASYNC_ENGINE | Select the asynchronous engine for uWSGI (optional) |  |  |
|WSGI_WORKERS | Number of workers. (Defaut: automatically setup with the scaler size) |  |  |
|WSGI_THREADS | Number of threads per worker. (Defaut: automatically setup with the scaler size) |  |  |
|WSGI_BUFFER_SIZE | Buffer size (in bytes) for uploads. | `4096` |  |
|WSGI_POST_BUFFERING | Maximal size (in bytes) for the headers of a request.  | `4096` |  |
|ENABLE_GZIP_COMPRESSION | Set to `true` to gzip-compress the output of uwsgi |  |  |
|GZIP_TYPES | Set the mime types to compress. | `text/* application/json application/xml application/javascript image/svg+xml` |  |
|NGINX_READ_TIMEOUT | Read timeout in seconds | `300` |  |
|CC_NGINX_PROXY_BUFFER_SIZE | Sets the size of the buffer used for reading the first part of the response received from the proxied server. <a href="https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffer_size" target="_blank" rel="noreferrer noopener">Nginx documentation</a> |  |  |
|CC_NGINX_PROXY_BUFFERS | Sets the number and size of the buffers used for reading a response from the proxied server, for a single connection. <a href="https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffers" target="_blank" rel="noreferrer noopener">Nginx documentation</a> |  |  |
|CC_HTTP_BASIC_AUTH | Restrict HTTP access to your application. Example: `login:password`. You can define multiple credentials using additional `CC_HTTP_BASIC_AUTH_n` (where `n` is a number) environment variables. |  |  |
{{< /table >}}

## Ruby

[Ruby Documentation]({{< ref "deploy/application/ruby/ruby-rack.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_RACKUP_SERVER | The server to use for serving the ruby application | puma |  |
|RACK_ENV |  |  |  |
|RAILS_ENV |  |  |  |
|CC_RUBY_VERSION | Choose the Ruby version to use but we strongly advise to set Ruby version in your Gemfile |  |  |
|[CC_RAKEGOALS]({{< ref "deploy/application/ruby/ruby-rack.md#configure-rake-goals" >}}) | A list of comma-separated rake goals to execute e.g. `db:migrate, assets:precompile` |  |  |
|[CC_ENABLE_SIDEKIQ]({{< ref "deploy/application/ruby/ruby-rack.md#configure-sidekiq" >}}) | Enable Sidekiq background process | `false` |  |
|CC_SIDEKIQ_FILES | Specify a list of Sidekiq configuration files e.g. `./config/sidekiq_1.yml,./config/sidekiq_2.yml` |  |  |
|STATIC_FILES_PATH | Relative path to where your static files are stored: `path/to/static` |  |  |
|[STATIC_URL_PREFIX]({{< ref "deploy/application/ruby/ruby-rack.md#manage-your-static-files-and-assets" >}}) | The URL path under which you want to serve static file, usually `/public` |  |  |
|STATIC_WEBROOT |  |  |  |
|NGINX_READ_TIMEOUT | Read timeout in seconds | `300` |  |
|CC_NGINX_PROXY_BUFFER_SIZE | Sets the size of the buffer used for reading the first part of the response received from the proxied server. <a href="https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffer_size" target="_blank" rel="noreferrer noopener">Nginx documentation</a> |  |  |
|CC_NGINX_PROXY_BUFFERS | Sets the number and size of the buffers used for reading a response from the proxied server, for a single connection. <a href="https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_buffers" target="_blank" rel="noreferrer noopener">Nginx documentation</a> |  |  |
|CC_HTTP_BASIC_AUTH | Restrict HTTP access to your application. Example: `login:password`. You can define multiple credentials using additional `CC_HTTP_BASIC_AUTH_n` (where `n` is a number) environment variables. |  |  |
{{< /table >}}

## Rust

[Rust Documentation]({{< ref "deploy/application/rust/rust.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_RUST_BIN | The name of the binary to launch once built |  |  |
|CC_RUSTUP_CHANNEL | The rust channel to use. Use a specific channel version with `stable`, `beta`, `nightly` or a specific version like `1.13.0`  | `stable` |  |
|CC_RUST_FEATURES | The list of features to enable |  |  |
|CC_RUN_COMMAND | Custom command to run your application. |  |  |
{{< /table >}}

## Addons-related environment variable

### FS Bucket

[FS Bucket Documentation]({{< ref "deploy/addon/fs-bucket.md" >}})

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |BUCKET_HOST | Hostname of the bucket |  | X |
 |CC_FS_BUCKET | Defines which bucket to mount on which path |  |  |
 {{< /table >}}

### MongoDB

[MongoDB Documentation]({{< ref "deploy/addon/mongodb.md" >}})

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |MONGODB_ADDON_DB |  | Generated upon creation | X |
 |MONGODB_ADDON_PASSWORD |  | Generated upon creation | X  |
 |MONGODB_ADDON_USER |  | Generated upon creation | X  |
 {{< /table >}}

### MySQL

[MySQL Documentation]({{< ref "deploy/addon/mysql/mysql.md" >}})

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |MYSQL_ADDON_DB |  | Generated upon creation | X  |
 |MYSQL_ADDON_PASSWORD |  | Generated upon creation | X  |
 |MYSQL_ADDON_ROLE |  | Generated upon creation | X  |
 |MYSQL_ADDON_USER |  | Generated upon creation | X  |
 {{< /table >}}

### ProxySQL

[ProxySQL Documentation]({{< ref "deploy/addon/mysql/proxysql.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_ENABLE_MYSQL_PROXYSQL | Enable the ProxySQL  feature | `false`  | |
|CC_MYSQL_PROXYSQL_MAX_CONNECTIONS | Defines the maximum number of connections the local ProxySQL will open to your MySQL add-on | `10` | |
|CC_MYSQL_PROXYSQL_USE_TLS | Enable or disable secured connection using TLS to your MySQL add-on | `true` | |
|CC_MYSQL_PROXYSQL_SOCKET_PATH | Contains the path to the Unix Datagram Socket to connect to ProxySQL | | `true` |
{{< /table >}}

### PostgreSQL

[PostgreSQL Documentation]({{< ref "deploy/addon/postgresql/postgresql.md" >}})

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |POSTGRESQL_ADDON_DB |  | Generated upon creation | X  |
 |POSTGRESQL_ADDON_PASSWORD |  | Generated upon creation | X  |
 |POSTGRESQL_ADDON_ROLE |  | Generated upon creation | X  |
 |POSTGRESQL_ADDON_USER |  | Generated upon creation | X  |
 {{< /table >}}

### Pgpool-II

[Pgpool-II Documentation]({{< ref "deploy/addon/postgresql/pgpool.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|CC_ENABLE_PGPOOL | Enables the Pgpool-II feature | `false` | |
|CC_PGPOOL_SOCKET_PATH | Contains the path to the Unix Datagram Socket to connect to Pgpool-II | `true` | |
|CC_PGPOOL_PCP_SOCKET_PATH | Contains the path to the Unix Datagram Socket to connect to PCP | `true` | |
|CC_PGPOOL_EXTRA_USERS | Add new user/password pairs to the pool_passwd file, separated by commas (user1:password,user2:password...) | `''` | |
|CC_PGPOOL_RESERVED_CONNECTIONS | Number of reserved connections | `0` | |
|CC_PGPOOL_LISTEN_BACKLOG_MULTIPLIER | Specifies the length of connection queue from frontend to Pgpool-II | `2` | |
|CC_PGPOOL_LEADER_WEIGHT | Weight for backend 0 (Leader) | `1` | |
|CC_PGPOOL_NUM_INIT_CHILDREN | Number of concurrent sessions allowed | `16` | |
|CC_PGPOOL_MAX_POOL | Number of connection pool caches per connection | `1` | |
|CC_PGPOOL_CHILD_LIFE_TIME | Pool exits after being idle for this many seconds | `300` | |
|CC_PGPOOL_CHILD_MAX_CONNECTIONS | Pool exits after receiving that many connections | `0` (no exit)  | |
|CC_PGPOOL_CONNECTION_LIFE_TIME | Connection to backend closes after being idle for this many seconds | `0` (no close) | |
|CC_PGPOOL_CLIENT_IDLE_LIMIT | Client is disconnected after being idle for that many seconds (even inside an explicit transactions!) | `0` (no disconnection) | |
|CC_PGPOOL_LOG_CONNECTIONS | Log connections | `off` | |
|CC_PGPOOL_LOG_DISCONNECTIONS | Log disconnections | `off` | |
|CC_PGPOOL_LOG_HOSTNAME | Hostname will be shown in ps status and in logs if connections are logged | `off` | |
|CC_PGPOOL_LOG_STATEMENT | Log all statements | `off`  | |
|CC_PGPOOL_LOG_PER_NODE_STATEMENT | Log all statements with node and backend informations | `off` | |
|CC_PGPOOL_LOG_CLIENT_MESSAGES | Log any client messages | `off` | |
|CC_PGPOOL_LOG_STANDBY_DELAY | Log standby delay. Valid values are combinations of `always`, `if_over_threshold` and `none` | `if_over_threshold` | |
|CC_PGPOOL_CONNECTION_CACHE | Activate connection pools | `on` | |
|CC_PGPOOL_REPLICATE_SELECT | Replicate SELECT statements in replication mode | `off` | |
|CC_PGPOOL_INSERT_LOCK | Automatically locks a dummy row or a table with INSERT statements to keep SERIAL data consistency | `on` | |
|CC_PGPOOL_LOBJ_LOCK_TABLE | When rewriting lo_creat command in replication mode, specify table name to lock | `''` | |
|CC_PGPOOL_REPLICATION_STOP_ON_MISMATCH | On disagreement with the packet kind sent from backend, degenerate the node which is most likely "minority" If off, just force to exit this session | `off` | |
|CC_PGPOOL_FAILOVER_IF_AFFECTED_TUPLES_MISMATCH | On disagreement with the number of affected tuples in UPDATE/DELETE queries, then degenerate the node which is most likely "minority". If off, just abort the transaction to keep the consistency | `off` | |
|CC_PGPOOL_LOAD_BALANCE_MODE | Activate load balancing mode | `on` | |
|CC_PGPOOL_IGNORE_LEADING_WHITE_SPACE | Ignore leading white spaces of each query | `on` | |
|CC_PGPOOL_READ_ONLY_FUNCTION_LIST | Comma separated list of function names that don't write to database (regexp are accepted) | `''` | |
|CC_PGPOOL_WRITE_FUNCTION_LIST | Comma separated list of function names that write to database (regexp are accepted) | `''` | |
|CC_PGPOOL_PRIMARY_ROUTING_QUERY_PATTERN_LIST | Semicolon separated list of query patterns that should be sent to primary node (regexp are accepted) | `''` | |
|CC_PGPOOL_DATABASE_REDIRECT_PREFERENCE_LIST | Comma separated list of pairs of database and node id | `''`  | |
|CC_PGPOOL_APP_NAME_REDIRECT_PREFERENCE_LIST | Comma separated list of pairs of app name and node id | `''`  | |
|CC_PGPOOL_ALLOW_SQL_COMMENTS | If on, ignore SQL comments when judging if load balance or query cache is possible | `off` | |
|CC_PGPOOL_DISABLE_LOAD_BALANCE_ON_WRITE | Load balance behavior when write query is issued in an explicit transaction. Valid values are `transaction`, `trans_transaction`, `dml_adaptive` or `always` | `transaction` | |
|CC_PGPOOL_DML_ADAPTIVE_OBJECT_RELATIONSHIP_LIST | Comma separated list of object pairs | `''` | |
|CC_PGPOOL_STATEMENT_LEVEL_LOAD_BALANCE | Enables statement level load balancing | `off` | |
|CC_PGPOOL_SR_CHECK_PERIOD | Streaming replication check period | `10` | |
|CC_PGPOOL_DELAY_THRESHOLD | Threshold before not dispatching query to standby node Unit is in bytes | `10000000` | |
|CC_PGPOOL_HEALTH_CHECK_PERIOD | Health check period | `20` | |
|CC_PGPOOL_HEALTH_CHECK_TIMEOUT | Health check timeout | `20`  | |
|CC_PGPOOL_HEALTH_CHECK_MAX_RETRIES | Maximum number of times to retry a failed health check before giving up | `0` | |
|CC_PGPOOL_HEALTH_CHECK_RETRY_DELAY | Amount of time to wait (in seconds) between retries | `1`  | |
|CC_PGPOOL_CONNECT_TIMEOUT | Timeout value in milliseconds before giving up to connect to backend | `10000` | |
|CC_PGPOOL_MEMORY_CACHE_ENABLED | Use the memory cache functionality | `off` | |
|CC_PGPOOL_MEMQCACHE_TOTAL_SIZE | Total memory size in megabytes for storing memory cache | `64` | |
|CC_PGPOOL_MEMQCACHE_MAX_NUM_CACHE | Total number of cache entries | `1000000` | |
|CC_PGPOOL_MEMQCACHE_EXPIRE | Memory cache entry life time specified in seconds | `0` | |
|CC_PGPOOL_MEMQCACHE_AUTO_CACHE_INVALIDATION | Invalidation of query cache is triggered by corresponding DDL/DML/DCL | `on` | |
|CC_PGPOOL_MEMQCACHE_MAXCACHE | Maximum SELECT result size in kilobytes (must be smaller than MEMQCACHE_CACHE_BLOCK_SIZE)  | `400` | |
|CC_PGPOOL_MEMQCACHE_CACHE_BLOCK_SIZE | Cache block size in megabytes  | `1` | |
|CC_PGPOOL_CACHE_SAFE_MEMQCACHE_TABLE_LIST | Comma separated list of table names to memcache that don't write to database (regexp are accepted)  | `''` | |
|CC_PGPOOL_CACHE_UNSAFE_MEMQCACHE_TABLE_LIST | Comma separated list of table names not to memcache that don't write to database (regexp are accepted)  | `''` | |
{{< /table >}}


### Elastic Stack

[Elastic Stack Documentation]({{< ref "deploy/addon/elastic/elastic.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|ELASTIC_APM_SERVER_URL | URI to connect APM Server | Generated upon creation | X  |
|ES_ADDON_APM_HOST | APM Server hostname | Generated upon creation | X  |
|ES_ADDON_APM_AUTH_TOKEN | Authentication token to send metrics to APM Server | Generated upon creation | X  |
|ELASTIC_APM_SECRET_TOKEN | Authentication token to send metrics to APM Server | Generated upon creation | X  |
|ES_ADDON_APM_USER | Username credential used by APM Server to send metrics to Elasticsearch | Generated upon creation | X  |
|ES_ADDON_APM_PASSWORD | Password credential used by APM Server to send metrics to Elasticsearch | Generated upon creation | X  |
|ES_ADDON_KIBANA_HOST | Kibana hostname | Generated upon creation | X  |
|ES_ADDON_KIBANA_USER | Username credential used by Kibana to query Elasticsearch | Generated upon creation | X  |
|ES_ADDON_KIBANA_PASSWORD | Password credential used by Kibana to query Elasticsearch | Generated upon creation | X  |
|ES_ADDON_URI | URI to query Elasticsearch | Generated upon creation | X  |
|ES_ADDON_HOST | Elasticsearch hostname | Generated upon creation | X  |
|ES_ADDON_USER | Username credential to authenticate to Elasticsearch | Generated upon creation | X  |
|ES_ADDON_PASSWORD | Password credential to authenticate to Elasticsearch | Generated upon creation | X  |
|ES_ADDON_VERSION | ElasticSearch Version | 7 | X  |
{{< /table >}}

### New Relic

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|[NEW_RELIC_APP_NAME](https://docs.newrelic.com/docs/apm/agents/java-agent/configuration/java-agent-configuration-config-file/#ev-NEW_RELIC_APP_NAME) | Contains the application name |  |  |
|[NEW_RELIC_LICENSE_KEY](https://docs.newrelic.com/docs/apm/agents/java-agent/configuration/java-agent-configuration-config-file/#ev-NEW_RELIC_LICENSE_KEY) | Contains your New Relic account license |  |  |
{{< /table >}}

### Pulsar

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center>    | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
| ------------------------ | ----------------------------------------- | ------------------------------ | -------------------------- |
|ADDON_PULSAR_BINARY_URL | The complete URL to use in your application | Generated upon creation | X |
|ADDON_PULSAR_BINARY_PORT | The port to connect to the Pulsar Cluster | Generated upon creation | X |
|ADDON_PULSAR_HOSTNAME | The host to connect to the Pulsar Cluster | Generated upon creation | X |
|ADDON_PULSAR_HTTP_URL | The complete URL to connect with WebSocket | Generated upon creation | X |
|ADDON_PULSAR_HTTP_PORT | The port to connect with WebSocket | Generated upon creation | X |
|ADDON_PULSAR_NAMESPACE | Your add-on Pulsar ID | Generated upon creation | X |
|ADDON_PULSAR_TENANT | Your Clever Cloud tenant ID | Generated upon creation | X |
|ADDON_PULSAR_TOKEN | Your Biscuit authentication token | Generated upon creation        | X |
{{< /table >}}

### Redis

[Redis Documentation]({{< ref "deploy/addon/redis.md" >}})

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
|-----------------------|------------------------------|--------------------------------|--------------------------------|
|REDIS_HOST |  | Generated upon creation | X  |
|REDIS_PORT |  | Generated upon creation | X  |
|REDIS_PASSWORD |  | Generated upon creation | X  |
{{< /table >}}

### Socks

 {{<table "table table- bordered" "text-align:center" >}}
 | <center>Name</center> | <center>Description</center> | <center>Default value</center> | <center>Read Only</center> |
 |-----------------------|------------------------------|--------------------------------|--------------------------------|
 |SOCKS_ADDON_HOST |  |  |  |
 |SOCKS_ADDON_PORT |  |  |  |
 |SOCKS_ADDON_PRIVATE_KEY |  |  |  |
 {{< /table >}}

### VPN

The VPN addon provides a fixed-ip outgoing node. This can be used to work
with services protected by ip address filtering. `VPN_ADDON_*` variables will
be provided by Clever Cloud upon setup, the only configuration you have to
provide is a list of CIDRs (eg. 1.2.3.0/24) for which you want the traffic
to be routed through the exit node.

{{<table "table table- bordered" "text-align:center" >}}
| <center>Name</center> | <center>Description</center>                                                                    | <center>Default value</center> | <center>Read Only</center> |
| --------------------- | ----------------------------------------------------------------------------------------------- | ------------------------------ | -------------------------- |
| VPN_ADDON_CRT         | Client certificate                                                                              |                                |                            |
| VPN_ADDON_CACRT       | Server CA certificate                                                                           |                                |                            |
| VPN_ADDON_KEY         | Client certificate private key                                                                  |                                |                            |
| VPN_ADDON_HOST        | Server host or IP address                                                                       |                                |                            |
| VPN_ADDON_PORT        | Server port                                                                                     |                                |                            |
| VPN_ADDON_TAKEY       | Pre-shared secret                                                                               |                                |                            |
| VPN_TARGETS           | Comma-separated list of CIDRs for which you want the traffic to be routed through the exit node |                                |                            |
{{< /table >}}
