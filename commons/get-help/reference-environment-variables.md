---
title: Environment Variable Reference
position: 3
tags:
- support
---

# Environment Variable Reference

##  Applications

### Commons to all Applications

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_TROUBLESHOOT</td>
    <td>Enable debug log level, will also keep the VM up after failure for 15 minutes so you can SSH and debug. Don't forget to cancel deployment if you push a new commit.</td>
    <td>false</td>
  </tr> 
  <tr>
    <td>CC_WORKER_COMMAND</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_PRE_BUILD_HOOK</td>
    <td>Ran before the dependencies are fetched. If it fails, the deployment fails.</td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_POST_BUILD_HOOK</td>
    <td>Ran after the project is built, and before the cache archive is generated. If it fails, the deployment fails.</td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_PRE_RUN_HOOK</td>
    <td>Ran before the application is started, but after the cache archive has been generated. If it fails, the deployment fails.</td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_RUN_SUCCEEDED_HOOK</td>
    <td>Ran once the application has started successfuly.</td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_RUN_FAILED_HOOK</td>
    <td>Ran once the application has failed to start.</td>
    <td></td>
  </tr> 
  <tr>
    <td>ENABLE_METRICS</td>
    <td>BETA: Enable metrics collection, Contact support.</td>
    <td>false</td>
  </tr> 
  <tr>
    <td>IGNORE_FROM_BUILDCACHE</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>CACHE_DEPENDENCIES</td>
    <td></td>
    <td>false</td>
  </tr> 
  <tr>
    <td>INSTANCE_NUMBER</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>APP_FOLDER</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>APP_ID</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>APP_HOME</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_DEPLOYMENT_ID</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>COMMIT_ID</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>CC_RACKUP_SERVER</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>ENABLE_GZIP_COMPRESSION</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>NGINX_READ_TIMEOUT</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_FILES_PATH</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_URL_PREFIX</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_WEBROOT</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>UWSGI_INTERCEPT_ERRORS</td>
    <td></td>
    <td></td>
  </tr> 
</table>


### Docker

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_MOUNT_DOCKER_SOCKET</td>
    <td>Set to true to access the host Docker socket from inside your container.</td>
    <td>false</td>
  </tr>
</table>

### Elixir


<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_PHOENIX_APP_DIR</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PHOENIX_ASSETS_DIR</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PHOENIX_DIGEST_GOAL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PHOENIX_SERVER_GOAL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_RUN_COMMAND</td>
    <td></td>
    <td></td>
  </tr>
</table>

### Go

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_GO_PKG</td>
    <td>Makes the deployer run go get ${CC_GO_PKG} instead of go get &lt;app_id&gt;. </td>
    <td></td>
  </tr>
</table>

### Haskell

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_RUN_COMMAND</td>
    <td></td>
    <td></td>
  </tr>
</table>

### Java

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_SBT_TARGET_DIR</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>GRADLE_DEPLOY_GOAL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>JAVA_VERSION</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>MAVEN_DEPLOY_GOAL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>NUDGE_APPID</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>PLAY1_VERSION</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>SBT_DEPLOY_GOAL</td>
    <td></td>
    <td></td>
  </tr>
</table>



### NodeJS

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CACHE_DEPENDENCIES</td>
    <td></td>
    <td>false</td>
  </tr>
  <tr>
    <td>CC_NODE_START_GOAL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_RUN_COMMAND</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>NODE_BUILD_TOOL</td>
    <td></td>
    <td>npm</td>
  </tr>
  <tr>
    <td>NPM_TOKEN</td>
    <td></td>
    <td></td>
  </tr>
</table>

### PHP


<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>ALWAYS_POPULATE_RAW_POST_DATA</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_LDAP_CA_CERT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>ENABLE_REDIS</td>
    <td></td>
    <td>false</td>
  </tr> 
  <tr>
    <td>HTTP_TIMEOUT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>LDAPTLS_CACERT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>MAX_INPUT_VARS</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>PHP_VERSION</td>
    <td></td>
    <td>7</td>
  </tr>
  <tr>
    <td>SESSION_TYPE</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>SOCKSIFY_EVERYTHING</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>USE_SOCKS</td>
    <td></td>
    <td>false</td>
  </tr> 
</table>

### Python

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_PYTHON_CELERY_LOGFILE</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PYTHON_CELERY_MODULE</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PYTHON_CELERY_USE_BEAT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PYTHON_MODULE</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_PYTHON_USE_GEVENT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>HARAKIRI</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>PYTHON_VERSION</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>STATIC_FILES_PATH</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_PATH</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_URL_PREFIX</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_WEBROOT</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>UWSGI_ASYNC</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>UWSGI_ASYNC_ENGINE</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>WSGI_WORKERS</td>
    <td></td>
    <td></td>
  </tr> 
</table>

### Ruby

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_RACKUP_SERVER</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>HARAKIRI</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>RACK_ENV</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>RAILS_ENV</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>RUBY_VERSION</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>STATIC_FILES_PATH</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_PATH</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_URL_PREFIX</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>STATIC_WEBROOT</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>UWSGI_ASYNC</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>UWSGI_ASYNC_ENGINE</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>WSGI_BUFFER_SIZE</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>WSGI_POST_BUFFERING</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>WSGI_THREADS</td>
    <td></td>
    <td></td>
  </tr> 
  <tr>
    <td>WSGI_WORKERS</td>
    <td></td>
    <td></td>
  </tr> 
</table>

### Rust

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>CC_TROUBLESHOOT</td>
    <td></td>
    <td>false</td>
  </tr>
  <tr>
    <td>INSTANCE_TYPE</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>RUSTUP_CHANNEL</td>
    <td></td>
    <td></td>
  </tr>
</table>

## Addons


### Couchbase

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>COUCHBASE_HOST</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>COUCHBASE_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>COUCHBASE_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>ENV_TOKEN</td>
    <td></td>
    <td></td>
  </tr>
</table>

### Elasticsearch:

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>ES_ADDON_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>ES_ADDON_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>ES_HEAP_SIZE</td>
    <td></td>
    <td></td>
  </tr>
</table>

### Jenkins

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>JENKINS_HOST</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>JENKINS_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>JENKINS_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
</table>

### Mongodb:

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>MONGODB_ADDON_DB</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>MONGODB_ADDON_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>MONGODB_ADDON_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
</table>

### MySQL


<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>ADDON_API_HOST</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>EXISTING_BACKUP_URL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>INTERNAL_ADDON_AUTH_PASS</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>MYSQL_ADDON_DB</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>MYSQL_ADDON_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>MYSQL_ADDON_ROLE</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>MYSQL_ADDON_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
</table>

### PostgreSQL


<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>ADDON_API_HOST</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>EXISTING_BACKUP_URL</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>INTERNAL_ADDON_AUTH_PASS</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>POSTGRESQL_ADDON_DB</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>POSTGRESQL_ADDON_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>POSTGRESQL_ADDON_ROLE</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>POSTGRESQL_ADDON_USER</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
</table>



### Redis

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>REDIS_HOST</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>REDIS_PORT</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>REDIS_PASSWORD</td>
    <td></td>
    <td>Generated on creation</td>
  </tr>
  <tr>
    <td>REDIS_TOKEN</td>
    <td></td>
    <td></td>
  </tr>
</table>


### New Relic

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>NEWRELIC_APPNAME</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>NEWRELIC_LICENSE</td>
    <td></td>
    <td></td>
  </tr>
</table>



### Socks

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>SOCKS_ADDON_HOST</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>SOCKS_ADDON_PORT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>SOCKS_ADDON_PRIVATE_KEY</td>
    <td></td>
    <td></td>
  </tr>
</table>

### VPN

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>VPN_ADDON_CRT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_ADDON_CACRT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_ADDON_KEY</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_ADDON_HOST</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_ADDON_PORT</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_ADDON_TAKEY</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>VPN_TARGETS</td>
    <td></td>
    <td></td>
  </tr>
</table>

### FS Bucket

<table class="table table-bordered" style="text-align:center">
  <tr>
    <th><center>Name</center></th>
    <th><center>Description</center></th>
    <th><center>Default value</center></th>
  </tr>
  <tr>
    <td>APP_BUCKET_HOST</td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <td>CC_FS_BUCKET</td>
    <td></td>
    <td></td>
  </tr>
</table>

