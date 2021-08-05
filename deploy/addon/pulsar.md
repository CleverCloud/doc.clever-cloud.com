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
---

## Overview

Pulsar works on a publisher / subscriber model, just like RabbitMQ or Kafka.

```
client               Pulsar             client
publishes to   =>    topic      <=  subscribes to

"producer"                           "consumer"
```

They are several modes of subscription. A consumer may subscribe exclusively, or share the subscription with other consumers.

- Exclusive
- Failover (if a consumer fails, another one receives the message)
- Shared (messages are distributed to several consumers)
- Key_Shared (messages come with keys and go to consumers with the corresponding key)

More on this in the [official documentation](https://pulsar.apache.org/docs/en/concepts-messaging/#subscriptions)

A topic is defined this way:

`{persistent|non-persistent}://tenant/namespace/topic`

Tenants and namespaces allow for grouping and subgrouping of topics.

What a pulsar add-on basically is, is a tenant/namespace pair that allows you to create and use as many topics as you wantt following this pattern:

`{persistent|non-persistent}://user_id/pulsar_addon_id/name_of_your_topic`

## Authorization

Pulsar addon uses (Biscuit for Pulsar)[https://github.com/CleverCloud/biscuit-pulsar/commits/master] implementation which is directly pluggable to the Pulsar authentication and authorization system. Each addon exposes its own biscuit token.

### Usage

We advise you to use (`pulsarctl`)[https://github.com/streamnative/pulsarctl] provided by StreamNative. Here is an example to list topics in your addon (in your namespace):

```bash
pulsarctl --admin-service-url https://<PULSAR_REST_URL> --auth-params <BISCUIT> --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken topics list <tenant>/<namespace>
```

As Biscuit is a token, you can use `AuthenticationToken("<BISCUIT>")` provided by clients librairies to authenticate to our clusters without any tweak.

* C++ client
* C# client
* Go client
* Java client
* Node.js client
* Python client
* Rust client
* WebSocket client

### Operations

The pulsar addon given biscuit enable you to run several operations on namespace, its policies and the related topics.

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

The pulsar addon given biscuit can be attenuated, here is an attenuation example using (biscuit-cli)[https://github.com/biscuit-auth/biscuit-cli] from the given biscuit to produce/consume topics starting with `TOPIC_PREFIX`:

First inspect your biscuit:
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

* The authority block is the cluster authentication block (the cluster admin biscuit).
* The block n°1 is an attenuation of the authority block to only authorize operations on `tenant = "user_1235678-f54e-4e09-848c-1953af6e3e89"` and `namespace = "pulsar_1235678-6b36-4af2-be1f-d97862c0c41c"`.

Now attenuate it:

```bash
$ biscuit attenuate addon.biscuit
```

This will open your `$EDITOR` to type the attenuation.

Put

```bash
check if topic_operation(#ambient, "user_12345678-f54e-4e09-848c-1953af6e3e89", "pulsar_12345678-6b36-4af2-be1f-d97862c0c41c", $topic, $operation), $topic.starts_with("TOPIC_PREFIX")
```

Then it outputs the attenuated token. You can find more examples on the (biscuit-pulsar authorization java tests)[https://github.com/CleverCloud/biscuit-pulsar/blob/master/src/test/java/com/clevercloud/biscuitpulsar/AuthorizationProviderBiscuitTest.java].

## Storage

### Retention

Provided addon pulsar have a infinite retention policies which can be changed using:

```bash
# Example to set retention of namespace to 2 weeks and/or 100 GB
pulsarctl --admin-service-url https://<PULSAR_REST_URL> --auth-params <BISCUIT> --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken namespaces set-retention <tenant>/<namespace> --time 2w --size 100G
```

### Offload storage to Cellar (S3)

Pulsar has a [tiered storage feature](https://pulsar.apache.org/docs/en/tiered-storage-overview/) allowing to offload heavy data to cold storage once a threshold is reached.

For each pulsar addon we provide, we also provide an hidden cellar addon (which is our object storage addon) which is directly binded to the pulsar namespace offload policies. 

The offload threshold of the namespace is deactivated by default, but you can activate it with:

```bash
# Example to set offload to run when hot storage is > 10G and put data to Cellar Addon
pulsarctl --admin-service-url https://<PULSAR_REST_URL> --auth-params <BISCUIT> --auth-plugin org.apache.pulsar.client.impl.auth.AuthenticationToken namespaces set-offload-treshold <tenant>/<namespace> 10G
```
