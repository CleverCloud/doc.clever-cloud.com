---
layout: page
title: File System Buckets
---
## FS Buckets: keep and manage data files.

The Git deployment prevents you to keep generated data files between
deployments. To avoid the loss of your data, you need a mounted,
persistent filesystem. That's what the FS Buckets are for.

### Create a bucket
1. Select in the headbar the appropriate organisation.
2. Click on the "Services" tab in you organisation : <img class="thumbnail img_doc" src="/img/services.png">
4. You can now choose FS Buckets among the available services : <img class="thumbnail img_doc" src="/img/mysql.png">
5. Clicking on the service's button will create it. You will receive the new bucket's credentials by email a few minutes later.
6. Under **Installed** you will find your running services. A tagging system allows you to easily identify services for differents apps or purposes.
<div>
<a href="/img/screenshot-services.png" target="_blank"><img class="thumbnail img_doc" src="/img/screenshot-services.png"></a>
</div>

### Configure your application

To configure your application to use buckets, use the
`clevercloud/buckets.json` file. (The `buckets.json` file in the
`clevercloud` folder at the root of your application.)

The `buckets.json` file must have the following structure:

{% highlight javascript %}
[
	{
 		"bucket" : "{bucketId}",
		"folder" : "/myFolder"
	}
]
{% endhighlight %}


It's a json array containing objects with two fields:

**bucket**
: The bucket id. You receive it by mail, or can get it in your
"services" tab.

**folder**
: The folder you want the bucket to be mounted in. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application.  If the folder already exists, it is
overriden by the bucket. You can also access the bucket using the
*/app/myFolder* path.
