## Configure your Swift application

### Mandatory configuration

To deploy a Swift application on Clever Cloud, you need to make sure that:

- You have a [`Package.swift`][package-manifest] file (referred to as the "package manifest")
- Your package manifest[^pm] declares at least one [`executableTarget`][executable-target]
  - If it declares more than one [`executableTarget`][executable-target],
    you can set the `CC_SWIFT_BIN` environment variable
    to the name of the [`executableTarget`][executable-target] you want us to execute
- You application listens to port `8080`

Then deploy the application using `git`, as described in [Getting Started > Quickstart > Git Deployment][git-deployment].

While it's not mandatory, we recommend you to commit your [`Package.resolved`][package-resolved] file.
<!-- TODO: Add env var to force use of resolved versions -->

- `CC_SWIFT_CONFIG`: Swift build configuration (debug|release) [default: release]
- `CC_SWIFT_BIN`: Name of executable product

### Advanced configuration

#### Build configuration

`CC_SWIFT_CONFIG` allows you to choose which configuration will be passed to `swift build`.
Possible values are `debug` and `release`.
If you don't define `CC_SWIFT_CONFIG`, we will build your application using the `release` configuration.

#### Binary target name

If your package manifest[^pm] declares more than one [`executableTarget`][executable-target],
you can set the `CC_SWIFT_BIN` environment variable
to the name of the [`executableTarget`][executable-target] you want us to execute.

#### Custom run command

As introduced in [Reference > Environment Variable Reference][cc-run-command],
`CC_RUN_COMMAND` allows you to override the command we will use to run your application.

By default, we simply execute the binary previously built by `swift build <options>`.
If you need to pass arguments to the invocation of your binary, this is the way to do it.

An example of such use case can be found in {{< ref "/deploy/application/swift/examples/swift-package-index.md" >}}




<!--

---

The result of `cargo build --release --locked` must be an executable which starts a web server listening on `0.0.0.0:8080`.

For instance, a minimal [iron](https://github.com/iron/iron) application can look like this:

```swift
extern crate iron;

use iron::prelude::*;
use iron::status;

fn main() {
    fn hello_world(_: &mut Request) -> IronResult<Response> {
        Ok(Response::with((status::Ok, "Hello World!")))
    }

    let _server = Iron::new(hello_world).http("0.0.0.0:8080").unwrap();
    println!("On 8080");
}
```

### Multiple binary targets

If your `Cargo.toml` defines multiple targets, you must specify the one you want to run, with the `CC_SWIFT_BIN` environment variable.
If `CC_SWIFT_BIN` is specified, then the executable produced by this target is used to start the application.

### Custom run command

If you need to run a custom command (or just pass options to the program), you can specify it through the `CC_RUN_COMMAND` [environment variable](#setting-up-environment-variables-on-clever-cloud).

For instance, you can have `CC_RUN_COMMAND=./target/release/myapp <options>`.

### Dependencies

Make sure to list all your dependencies in `Cargo.toml`. For the example above, you need:

```toml
[package]
name = "my-app"
version = "0.1.0"
authors = []

[dependencies]
iron = "0.4.0"
```

### Cached dependencies

#### Enabling dependencies caching

You can enable dependencies caching by adding the `CC_CACHE_DEPENDENCIES=true` [environment variable](#setting-up-environment-variables-on-clever-cloud) in your application.

#### Disabling dependencies caching

You can disable dependencies caching completely by removing the `CC_CACHE_DEPENDENCIES` environment variable from the Clever Cloud console, in the **Environment variables** menu of your application. Or by setting it to `CC_CACHE_DEPENDENCIES=false`.

To fully remove cached dependencies, you have to rebuild your application from scratch. You can select "rebuild and restart" from the Clever Cloud console or launch `clever restart --without-cache` with the Clever Tools CLI.

### Private dependencies

If you use dependencies on a private git repository inside your project, it needs a bit of configuration until [this cargo issue](https://GitHub.com/rust-lang/cargo/issues/1851) has been resolved.

First, you need to use the `HTTPS` url as the git url for your dependency in your `Cargo.toml`:

`private-dep = { git = "https://github.com/user/my-private-dep.git" }`

Then, you need to create a personal access token. It allows to not use your password:
- `Github`: https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
- `Gitlab`: https://docs.gitlab.com/ce/user/profile/personal_access_tokens.html (API rights are needed)

Once you have the token, we need to tell Git to use a credential store. For that, we are going to create it.

Create a `clevercloud/pre-build.sh` file at the root of your application and paste:

```bash
#! /usr/bin/env bash

git config --global credential.helper store
echo "https://${GIT_USERNAME}:${GIT_PASSWORD}@gitlab.com" > ~/.git-credentials
chmod 600 ~/.git-credentials
```

If you have multiple private repositories, add them accordingly.

Now, go into the environment variables page of your application and create those environment variables:

- `GIT_USERNAME`: your Github / gitlab / other username
- `GIT_PASSWORD`: your Github / gitlab / other password
- `CC_PRE_BUILD_HOOK`: clevercloud/pre-build.sh
- `CC_POST_BUILD_HOOK`: rm /home/bas/.git-credentials

This adds the git configuration before the build start and it cleans it after the build has been done.

### Swift channels

By default, your application is built with the latest stable rust version. If you require beta, nightly or a specific Swift version, you can set `CC_RUSTUP_CHANNEL` [environment variable](#setting-up-environment-variables-on-clever-cloud) value to `beta`, `nightly` or a specific version (eg. `1.36.0`). 

The build uses `rustup` to select the Swift version you need.

### Cargo features

You can enable specific features for your crate by settings the `CC_RUST_FEATURES`
environment variable to the list of features to enable.

To access environment variables from your code, just get them from the environment with `std::env::var(<KEY_NAME>)`.

If some environment variables are critical to your application, here is an approach you can use:

```swift
use std::env;

fn main() {
    let my_config_value = env::var("MY_VAR").expect("Missing env var `MY_VAR`");

    something_that_runs_a_web_server(my_config_value);
}
```

This loads the environment variable in your `main` function and use `.expect` to fail early. This way, the application will refuse to start with an helpful error message if `MY_VAR` is not defined.

-->






[^pm]: In Swift, the "package manifest" refers to the [`Package.swift`][package-manifest] file in the main directory of the package.

[package-manifest]: <https://developer.apple.com/documentation/packagedescription> "PackageDescription | Apple Developer Documentation"
[executable-target]: <https://developer.apple.com/documentation/packagedescription/target/executabletarget(name:dependencies:path:exclude:sources:resources:publicheaderspath:csettings:cxxsettings:swiftsettings:linkersettings:plugins:)> "executableTarget(name:dependencies:path:exclude:sources:resources:publicHeadersPath:cSettings:cxxSettings:swiftSettings:linkerSettings:plugins:) | Apple Developer Documentation"
[package-resolved]: <https://github.com/apple/swift-package-manager/blob/809cb152a303926b243edb09ba4f0590e4b0c8b7/Documentation/Usage.md#resolving-versions-packageresolved-file> "swift-package-manager/Documentation/Usage.md at 809cb152a303926b243edb09ba4f0590e4b0c8b7 · apple/swift-package-manager"

[git-deployment]: <{{< ref "/getting-started/quickstart.html#git-deployment" >}}> "Quickstart | Clever Cloud Documentation"
[cc-run-command]: <{{< ref "/reference/reference-environment-variables#variables-you-can-define" >}}> "Environment Variable Reference | Clever Cloud Documentation"
