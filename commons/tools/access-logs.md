---
title: Access Logs
shortdesc: Get and process your access logs.
tags:
- apps, metrics, accesslogs, warp10
---

# Access Logs analysis

All your applications access logs are pushed to Warp10. You are now able to process them directly on the console on the Metrics tab of your applications.

# Access Log model

Access logs are defined in the `'accessLogs'` Warp10 class and there are three Warp10 labels available:

* owner_id;
* app_id;
* adc (reverse proxy used).

To reduce space used to store access logs, we defined a light key-value model as following:

```bash
t -> timestamp
a -> appId
o -> ownerId
i -> instanceId
ipS -> ipSource
pS -> portSource # 0 if undefined
s -> source
  lt -> latitude
  lg -> longitude
  ct -> city
  co -> country
ipD -> ipDestination
pD -> portDestination # 0 if undefined
d -> destination
  lt -> latitude
  lg -> longitude
  ct -> city
  co -> country
vb -> verb
path -> path
bIn -> bytesInt
bOut -> bytesOut
h -> hostname
rTime -> responseTime
sTime -> serviceTime
scheme -> scheme
sC -> statusCode
sT -> statusText
tS -> Haproxy termination_state
adc -> Reverse proxy hostname
w -> workerId (Sozu)
r -> requestId (Sozu)
```

# A Warp10 query example:

```bash
# retrieve all accesslogs since 30 seconds ago
[ '<READTOKEN>' 'accessLogs' { } $NOW 30 s ] FETCH

# get all path
<% DROP
    VALUES
    <% DROP
        JSON->
        'path' GET
    %> LMAP
%> LMAP
FLATTEN

# distinct|unique results
UNIQUE
```
