---
title: Pulsar
shortdesc: A distributed messaging and streaming platform on the publish/subsribe model
tags:
- addons
keywords:
- pulsar
- messaging
- cold storage
- queue
- hot storage
- biscuit
- rabbitmq
- kafka
---

## Overview

[Pulsar](https://pulsar.apache.org/) works on a publisher/subscriber model, just like RabbitMQ or Kafka.

```
client               Pulsar             client
publishes to   =>    topic      <=  subscribes to

"producer"                           "consumer"
```

They are several modes of subscription. A consumer may subscribe exclusively, or share the subscription with other consumers. There is the subscription mode types:

- Exclusive (only one consumer for the subscription)
- Failover (if a consumer fails, another one receives the message)
- Shared (messages are distributed to several consumers)
- Key_Shared (messages come with keys and go to consumers with the corresponding key)

More on this in the [official documentation](https://pulsar.apache.org/docs/en/concepts-messaging/#subscriptions)

A topic is defined this way:

`{persistent|non-persistent}://tenant/namespace/topic`

Tenants and namespaces allow for grouping and subgrouping of topics.

A Clever Cloud Pulsar add-on is basically a immutable `tenant/namespace` where the tenant is your user id, and the namespace is the add-on id.
It allows you to create and use topics following this pattern:

`{persistent|non-persistent}://<USER_ID>/<ADDON_ID>/<TOPIC_NAME>`

## Authorization

Pulsar add-on uses [Biscuit for Pulsar](https://github.com/CleverCloud/biscuit-pulsar) implementation which is directly pluggable to the Pulsar authentication and authorization system. Each add-on exposes its own Biscuit token.

### Usage

We advise you to use [`pulsarctl`](https://github.com/streamnative/pulsarctl) provided by StreamNative. Here is an example to list topics in your add-on (in your namespace):

```bash
pulsarctl --admin-service-url $ADDON_PULSAR_WEB_URL --auth-params $ADDON_PULSAR_TOKEN --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken topics list $ADDON_PULSAR_TENANT/$ADDON_PULSAR_NAMESPACE
```

As Biscuit is a token, you can use `AuthenticationToken($ADDON_PULSAR_TOKEN)` provided by clients librairies to authenticate to our clusters without any tweak.

* C++ client
* C# client
* Go client
* Java client
* Node.js client
* Python client
* Rust client
* WebSocket client

### Operations

The pulsar addon given Biscuit token enables you to run several operations on namespace, its policies and the related topics.

#### Namespace

Namespace operations authorized:

```
CREATE_TOPIC
GET_TOPIC
GET_TOPICS
DELETE_TOPIC
ADD_BUNDLE
DELETE_BUNDLE
GET_BUNDLE
CLEAR_BACKLOG
UNSUBSCRIBE
```

Namespace policies operations authorized:

```
ALL_READ
ANTY_AFFINITY_READ
BACKLOG_READ
BACKLOG_WRITE
COMPACTION_READ
COMPACTION_WRITE
DELAYED_DELIVERY_READ
DELAYED_DELIVERY_WRITE
DEDUPLICATION_READ
DEDUPLICATION_WRITE
MAX_CONSUMERS_READ
MAX_CONSUMERS_WRITE
MAX_PRODUCERS_READ
MAX_PRODUCERS_WRITE
MAX_UNACKED_READ
MAX_UNACKED_WRITE
OFFLOAD_READ
PERSISTENCE_READ
PERSISTENCE_WRITE
RATE_WRITE
RATE_READ
RETENTION_READ
RETENTION_WRITE
REPLICATION_READ
REPLICATION_RATE_READ
SCHEMA_COMPATIBILITY_STRATEGY_READ
SCHEMA_COMPATIBILITY_STRATEGY_WRITE
SUBSCRIPTION_AUTH_MODE_READ
SUBSCRIPTION_AUTH_MODE_WRITE
ENCRYPTION_READ
ENCRYPTION_WRITE
TTL_READ
TTL_WRITE
```

#### Topics

Topics operations authorized:

```
LOOKUP
PRODUCE
CONSUME
COMPACT
EXPIRE_MESSAGES
OFFLOAD
PEEK_MESSAGES
RESET_CURSOR
SKIP
TERMINATE
SUBSCRIBE
GET_SUBSCRIPTIONS
UNSUBSCRIBE
GET_STATS
```

Topics policies operations authorized:

```
PARTITION_READ
PARTITION_WRITE
```

### Attenuation

The Pulsar add-on given Biscuit token can be attenuated, here is an attenuation example using [biscuit-cli](https://github.com/biscuit-auth/biscuit-cli) from the given Biscuit token to produce/consume topics starting with a custom topic prefix called `"my-own-prefix"`.

Put your Biscuit token in a file:

```bash
echo $ADDON_PULSAR_TOKEN > addon.biscuit
```

Inspect your Biscuit token:

```bash
$ biscuit inspect addon.biscuit
Authority block:
== Datalog ==
right(#authority, #admin);

== Revocation ids ==
Content-based: fe7526a27b43fa7d5386c31b3efb6c53b551b4a8e3e969f4dc074497b3942a57
Unique:        faf6cab8a4dffad77633181d6f924414980bb6d76c5298b88f700c11659c7407

==========

Block n°1:
== Datalog ==
check if topic_operation(#ambient, "user_1235678-f54e-4e09-848c-1953af6e3e89", "pulsar_1235678-6b36-4af2-be1f-d97862c0c41c", $2, $3) or namespace_operation(#ambient, "user_12345668-f54e-4e09-848c-1953af6e3e89", "pulsar_1235678-6b36-4af2-be1f-d97862c0c41c", $2);

== Revocation ids ==
Content-based: a35a92a278a3ad9ec5db8dc3e905cfbc17f5a8e2cb13ff39b69b003500fe6c46
Unique:        17dfec62b62da36562a0998d496bb3aa30f229138ec810a070084bdf1c55be3a

==========
```

* The authority block is the cluster authentication block (the cluster admin Biscuit token).
* The block n°1 is an attenuation of the authority block to only authorize operations on `tenant = "user_1235678-f54e-4e09-848c-1953af6e3e89"` and `namespace = "pulsar_1235678-6b36-4af2-be1f-d97862c0c41c"`.

Attenuate it:

```bash
biscuit attenuate addon.biscuit
```

This will open your `$EDITOR` to type the attenuation.

Put

```bash
check if topic_operation(#ambient, "user_12345678-f54e-4e09-848c-1953af6e3e89", "pulsar_12345678-6b36-4af2-be1f-d97862c0c41c", $topic, $operation), $topic.starts_with("my-own-prefix")
```

Then it outputs the attenuated token. Inspect it to ensure your attenuation:

```bash
Authority block:
== Datalog ==
right(#authority, #admin);

== Revocation ids ==
Content-based: fe7526a27b43fa7d5386c31b3efb6c53b551b4a8e3e969f4dc074497b3942a57
Unique:        faf6cab8a4dffad77633181d6f924414980bb6d76c5298b88f700c11659c7407

==========

Block n°1:
== Datalog ==
check if topic_operation(#ambient, "user_12345678-f54e-4e09-848c-1953af6e3e89", "pulsar_50ee8f69-6b36-4af2-be1f-d97862c0c41c", $2, $3) or namespace_operation(#ambient, "user_12345678-f54e-4e09-848c-1953af6e3e89", "pulsar_12345678-6b36-4af2-be1f-d97862c0c41c", $2);

== Revocation ids ==
Content-based: a35a92a278a3ad9ec5db8dc3e905cfbc17f5a8e2cb13ff39b69b003500fe6c46
Unique:        17dfec62b62da36562a0998d496bb3aa30f229138ec810a070084bdf1c55be3a

==========

Block n°2:
== Datalog ==
check if topic_operation(#ambient, "user_12345678-f54e-4e09-848c-1953af6e3e89", "pulsar_12345678-6b36-4af2-be1f-d97862c0c41c", $topic, $operation), $topic.starts_with("my-own-prefix");

== Revocation ids ==
Content-based: 8eaaa639d5b94c3e053ad840e8dcca6fe66a621442ecc99eadf1df03d6138f1d
Unique:        f608dc2f724fc14faf0daf50774ef0b9425cda26f56ee93317ca80ca13736027

==========
```

Now the block n°2 ensure that topics must starts with `"my-own-prefix"`.

You can find more examples on the [biscuit-pulsar authorization java tests](https://github.com/CleverCloud/biscuit-pulsar/blob/master/src/test/java/com/clevercloud/biscuitpulsar/AuthorizationProviderBiscuitTest.java).

## Storage

### Retention

Provided Pulsar add-on have infinite retention policies which can be changed using:

```bash
# Example to set retention of namespace to 2 weeks and/or 100 GB
pulsarctl --admin-service-url $ADDON_PULSAR_WEB_URL \\
          --auth-params $ADDON_PULSAR_BISCUIT_TOKEN \\
          --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken \\
          namespaces set-retention $ADDON_PULSAR_TENANT/$ADDON_PULSAR_NAMESPACE --time 2w --size 100G
```

### Offload storage to Cellar (S3)

Pulsar has a [tiered storage feature](https://pulsar.apache.org/docs/en/tiered-storage-overview/) allowing to offload heavy data to cold storage once a threshold is reached.

For each Pulsar add-on we provide, we also provide an hidden [Cellar add-on](https://www.clever-cloud.com/doc/deploy/addon/cellar/) (which is our object storage addon) which is directly binded to the Pulsar namespace offload policies.

The offload threshold of the namespace is deactivated by default, you can activate it with:

```bash
# Example to set offload to run when hot storage is > 10G and put data to Cellar Addon
pulsarctl --admin-service-url $ADDON_PULSAR_WEB_URL \\
          --auth-params $ADDON_PULSAR_BISCUIT_TOKEN \\
          --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken \\
          namespaces set-offload-treshold $ADDON_PULSAR_TENANT/$ADDON_PULSAR_NAMESPACE 10G
```
