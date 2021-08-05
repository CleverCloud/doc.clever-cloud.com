---
title: Pulsar
shortdesc: A distributed messaging and streaming platform on the publish/subsribe model
tags:
- addons
keywords:
- pulsar
- messaging
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

## Use your pulsar addon

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

