---
title: File System Buckets
position: 3
---
## FS Buckets: keep and manage data files.

The Git deployment prevents you to keep generated data files between
deployments. To avoid the loss of your data, you need a mounted,
persistent filesystem. That's what the FS Buckets are for.

### Creating a FS Bucket
1. Select in the headbar the appropriate organisation.
2. Click on the "Services" tab in you organisation : <figure class="cc-content-img"><a href="/assets/images/intro-services1.png"><img src="/assets/images/intro-services1.png"></a></figure><figcaption>Adding Services in Clever Cloud.</figcaption>
4. You can now choose FS Buckets among the available services.
5. Clicking on the service's button will create it. You will receive the new bucket's credentials by email a few minutes later.
6. Under **Installed** you will find your running services. A tagging system allows you to easily identify services for differents apps or purposes.
<figure class="cc-content-img"><a href="/assets/images/intro-services2.png"><img src="/assets/images/intro-services2.png"></a></figure>
  <figcaption>
    Services Management in Clever Cloud.
</figcaption>

### Configuring your application

To configure your application to use buckets, use the
`clevercloud/buckets.json` file. The `buckets.json` file in the `clevercloud` folder must be located at the root of your application.

The `buckets.json` file must have the following structure:

```javascript
[
	{
 		"bucket" : "{bucketId}",
		"folder" : "/myFolder"
	}
]
```


It's a json array containing objects with two fields:

* **Bucket**
: The bucket id. You receive it by mail, or can get it in your
"services" tab.

* **Folder**
: The folder you want the bucket to be mounted in. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application.  
If the folder already exists, it is
overriden by the bucket. 
You can also access the bucket using the
*/app/myFolder* path.

### Video demo
<p>
<iframe style="width:640px" height="360" src="http://www.youtube.com/embed/6rJ8zQqIhUw?rel=0&autohide=1&showinfo=0" frameborder="0" controls="0"  allowfullscreen="allowfullscreen"> </iframe>  
</p>