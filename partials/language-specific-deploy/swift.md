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

An example of such use case can be found in [Example Swift deployment: the Swift Package Index][doc-example-spi].

[^pm]: In Swift, the "package manifest" refers to the [`Package.swift`][package-manifest] file in the main directory of the package.

[package-manifest]: <https://developer.apple.com/documentation/packagedescription> "PackageDescription | Apple Developer Documentation"
[executable-target]: <https://developer.apple.com/documentation/packagedescription/target/executabletarget(name:dependencies:path:exclude:sources:resources:publicheaderspath:csettings:cxxsettings:swiftsettings:linkersettings:plugins:)> "executableTarget(name:dependencies:path:exclude:sources:resources:publicHeadersPath:cSettings:cxxSettings:swiftSettings:linkerSettings:plugins:) | Apple Developer Documentation"
[package-resolved]: <https://github.com/apple/swift-package-manager/blob/809cb152a303926b243edb09ba4f0590e4b0c8b7/Documentation/Usage.md#resolving-versions-packageresolved-file> "swift-package-manager/Documentation/Usage.md at 809cb152a303926b243edb09ba4f0590e4b0c8b7 · apple/swift-package-manager"

[git-deployment]: <{{< ref "/getting-started/quickstart.html#git-deployment" >}}> "Quickstart | Clever Cloud Documentation"
[cc-run-command]: <{{< ref "/reference/reference-environment-variables#variables-you-can-define" >}}> "Environment Variable Reference | Clever Cloud Documentation"
[doc-example-spi]: <{{< ref "/deploy/application/swift/examples/swift-package-index.md" >}}> "Example Swift deployment: the Swift Package Index | Clever Cloud Documentation"
