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

# Warp10 queries examples:

```bash
# retrieve all accesslogs from now to last 30s
[ '<READTOKEN>' 'accessLogs' { } NOW 30 s ] FETCH

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

```bash
# Get all application status code  for the last hour
[ '<READTOKEN>' 'accessLogs' { 'app_id' '<APPLICATION ID>' } NOW 1 h ] FETCH
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
