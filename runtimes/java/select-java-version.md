---
title: Selecting the java version
shortdesc: This section provides information for selecting the appropriate java version for your application
tags:
- java
---

## Selecting your java version

Simply set the environment variable **JAVA_VERSION** to the version you want

Accepted values are `7` and `8`

<div class="alert alert-hot-problems">
<h4>**Default version**</h4>
We are still using Java version 7 by default for backward compatibility.<br/>
However, new applications will have the **JAVA_VERSION** environment variable set to 8.
</div>


## Retrocompatibility with the old configuration system

The configuration of the JAVA_VERSION through the **clevercloud/java_version** file still works. This is totally backward compatible.
