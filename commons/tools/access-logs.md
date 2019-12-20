---
title: Access Logs
shortdesc: Get and process your access logs.
tags:
- apps, metrics, accesslogs, warp10
---

# Access Logs analysis

All your applications access logs are pushed to Warp10. You are now able to process them directly in the console in the Metrics tab of your applications.

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

# Warp10 

## Main concepts :

Warp10 is a time series database. The notion of `class`, `labels`, `longitude`, `latitude`, `altitude` and `value` are used.

A GTS is a GeoTime Serie define by a `class` and somes `labels`. They are indexed and used to quickly retrived the data.

`labels` is a kind of dictionnary. That is called a **map** under the warp10 terminology

A GTS may contain some values which have the following models : `[ timestamp longitude latitude altitude value ]`

The `warpscript` langage is the langage run by warp10. It is a **reverse polish notation** with the notion of **stack**

> Notice that the timestamp is in **milliseconds**

> [Warp1O documentation is availlable in their website](https://www.warp10.io/doc/reference)

## queries examples:

The main ways to use `accessLogs` data are to `FETCH` over its and to get interesting values by a JSON processing.

```warpscript
// Fetch on the 'accessLogs' class for your application id as labels
[ '<READTOKEN>' 'accessLogs' { 'app_id' '<APP_ID>'  } NOW 30 s ] FETCH

// get the path field
<% 
    DROP
    VALUES
    <% DROP
        JSON->
        'path' GET
    %> LMAP
%> LMAP
FLATTEN

// distinct|unique results
UNIQUE
```

A convenient way to integrate the intercepted data in a workflow, is to use the power of warp10. Hence, it is often a good idea to use the GTS format to be able to apply all GTS treatment on the output.

In the following example, we get the `accessLogs` status codes and create a GTS as output to be able to use some FILTER or other treatment on it in a second time.

```warpscript
// Get all application status code  for the last hour
[ '<READTOKEN>' 'accessLogs' { 'app_id' '<APPLICATION ID>' } NOW 10 m ] FETCH
<%
  DROP
  'gts' STORE
  // output new GTS
  NEWGTS
  $gts 
  <%
    DUP
    // store the timestamp
    0 GET 'ts' STORE
    // store the status code
    -1 GET JSON-> 'sC' GET 'sC' STORE
    // Keep the same labels in the output GTS than in the input ones
    $gts LABELS RELABEL
    // Add timestamp and status code value to the output GTS
    [ $ts NaN NaN NaN $sC ] ADDVALUE
  %>
  FOREACH
%> LMAP
```
