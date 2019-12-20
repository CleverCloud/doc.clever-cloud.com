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

## technicals constraints

We had fixed some limits in our Warp10 database. For each we have a *soft* and a *hard* one. To pass over the *soft* on. The stack must be [`AUTHENTICATE`](https://www.warp10.io/doc/AUTHENTICATE) and the limit must be **explicitly** set under the maximum define by the *hard*.

| Warpscript Operator | Warp10 limit description | soft limit | hard limit |
|:-:|:-:|:-:|:-:|
| [MAXGTS](https://www.warp10.io/doc/MAXGTS) | Maximum number of GTS which can be fetched | 10e5 | 5e7 |
| [LIMIT](https://www.warp10.io/doc/LIMIT) | maximum number of datapoints which can be fetched during a script execution | 10e6 | 10e7 |
| [MAXBUCKETS](https://www.warp10.io/doc/MAXBUCKETS) | maximum number of buckets which can be created by a call to BUCKETIZE |  |  |
| [MAXDEPTH](https://www.warp10.io/doc/MAXDEPTH) | maximum depth (number of levels) of the execution stack | 5e3 | 5e3 |
| [MAXLOOP](https://www.warp10.io/doc/MAXLOOP) | maximum number of **milliseconds** which can be spent in a loop | 5e3 | 10e3 |
| [MAXOPS](https://www.warp10.io/doc/MAXOPS) | maximum number of operations which can be performed during a single WarpScript execution | 5e6 | 5e7 |
| [MAXSYMBOLS](https://www.warp10.io/doc/MAXSYMBOLS) | maximum number of simultaneous symbols which can be defined on the stack during a single WarpScript execution | 64 | 256 |
| [MAXGEOCELLS](https://www.warp10.io/doc/MAXGEOCELLS) | maximum number of cells a GEOSHAPE  | 10e3 | 10e4 |
| [MAXPIXELS](https://www.warp10.io/doc/MAXPIXELS) | maximum size (in pixels) of images which can be created by PGraphics | 10e5 | 10e5 |
| [MAXRECURSION](https://www.warp10.io/doc/MAXRECURSION) | maximum nesting depth of macro calls | 16 | 32 |

> **NOTICE THAT OPERATIONS OVER SOFT LIMITS MAY BE REALLY  INTENSIVES AND SLOWS. THEY SHOULDN'T NOT BE USED**

### Usage:

An example where it is needed to increase the fetch limit by the `LIMIT` function

```warpscript
'<READTOKEN>' AUTHENTICATE
50e6 TOLONG LIMIT
// Fetch on the 'accessLogs' class for your application id as labels
[ '<READTOKEN>' 'accessLogs' { 'app_id' '<APP_ID>'  } NOW 1 w ] FETCH
```

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
