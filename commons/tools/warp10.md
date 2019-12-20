---
title: Access Logs
shortdesc: Get and process your access logs.
tags:
- apps, GTS, warp10, Quantum, visualization
---


# Warp10 

## Main concepts :

Warp10 is a time series database. The notion of `class`, `labels`, `longitude`, `latitude`, `altitude` and `value` are used.

A GTS is a GeoTime Serie define by a `class` and somes `labels`. They are indexed and used to quickly retrived the data.

`labels` is a kind of dictionnary. That is called a **map** under the warp10 terminology

A GTS may contain some values which have the following models : `[ timestamp longitude latitude altitude value ]`

The `warpscript` langage is the langage run by warp10. It is a **reverse polish notation** with the notion of **stack**

> Notice that the timestamp is in **milliseconds**

> [Warp1O documentation is availlable in their website](https://www.warp10.io/doc/reference)

## Endpoint

The clevercloud warp10 endpoint is:

```
https://c1-warp10-clevercloud-customers.services.clever-cloud.com/api/v0
```

You can find some documentation about endpoint gateway [here](https://www.warp10.io/content/03_Documentation/03_Interacting_with_Warp_10/01_Introduction).

> You can find the endpoint and an available token under the `metric` tab of your application

Hence, you can request our warp10 platform by your own script. With a curl for expample :

```bash
  curl -T <Path/to/a/warpscript_file> https://c1-warp10-clevercloud-customers.services.clever-cloud.com/api/v0/exec
```

> Do not forget the endpoint. `exec` in the previous exemple.

## Token

Tokens are based on your application with the notion of producer and owner. Hence, only the data owner can see it.

You can find a 5 days available token in the `metric` tab of your application.

## technicals constraints

We had fixed some limits in our Warp10 database. For each we have a *soft* and a *hard* one. To pass over the *soft* on. The stack must be [`AUTHENTICATE`](https://www.warp10.io/doc/AUTHENTICATE) and the limit must be **explicitly** set under the maximum define by the *hard*.

| Warpscript Operator | Warp10 limit description | soft limit | hard limit |
|:-:|:-:|:-:|:-:|
| [MAXGTS](https://www.warp10.io/doc/MAXGTS) | Maximum number of GTS which can be fetched | 10e5 | 5e7 |
| [LIMIT](https://www.warp10.io/doc/LIMIT) | maximum number of datapoints which can be fetched during a script execution | 10e6 | 10e7 |
| [MAXBUCKETS](https://www.warp10.io/doc/MAXBUCKETS) | maximum number of buckets which can be created by a call to BUCKETIZE | 10e5 | 50e5 |
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

# Quantum

Quantum is a web tool use to run some warpscript. You can access to it from your metrics interface.

It provide the path to the clevercloud warp10 gateway and let you explore your data.
