---
title: Deploy rust apps
shortdesc: Rust is a systems programming language that runs blazingly fast, prevents segfaults, and guarantees thread safety.
tags:
- rust
keywords:
- rust
- cargo
---

# Deploy Rust apps

<div class="panel panel-warning">
  <div class="panel-heading">
     <h4>Rust support is in private beta</h4>
  </div>
  <div class="panel-body">
    If you are interested in accessing the beta test, please contact the support.
  </div>
</div>

Clever Cloud allows you to deploy rust web applications. This page will explain
you how to set up your application to run it on our service.

You do not need to change a lot in your application, the *requirements* will
help you to configure your apps with some mandatory files to add, and
properties to setup.

## Overview

Rust is a systems programming language that runs blazingly fast, prevents
segfaults, and guarantees thread safety. You can build rust web services with
frameworks like [Iron](http://ironframework.io/)

## Create an application

Refer to the page [Deploy an application on Clever Cloud](/doc/clever-cloud-overview/add-application/).

## Necessary information

Be sure that:

* you have pushed in <b>master branch</b>
* you listen on <b>port 8080</b>
* you have listed all your dependencies in `Cargo.toml`
* you have one (and only one) binary target in `Cargo.toml`

## Requirements

The result of `cargo run --release` must be a process listening on
`0.0.0.0:8080`.

For instance, a minimal iron application can look like this:

```rust
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

### Dependencies

Make sure to list all your dependencies in `Cargo.toml`. For the example
above, you need:

```toml
[package]
name = "my-app"
version = "0.1.0"
authors = []

[dependencies]
iron = "0.4.0"
```

## Environment injection

Clever Cloud can inject environment variables that are defined in the
dashboard and by add-ons linked to your application.

You can access environment variables with `std::env::var(<KEY_NAME>)`.

If environment variables are needed for your application to work properly, the
best solution is to load them in your `main` function and use `.expect` to
fail early with a descriptive error.

```rust
use std::env;

fn main() {
    fn hello_world(_: &mut Request) -> IronResult<Response> {
        Ok(Response::with((status::Ok, "Hello World!")))
    }
    let my_config_value = env::var("MY_KEY").expect("Missing env var `MY_KEY`");

    something_that_runs_a_web_server(my_config_value);
}
```

## Deploy on Clever Cloud

Application deployment on Clever Cloud is via Git. Follow [these
steps](/doc/clever-cloud-overview/add-application/) to deploy your
application.
