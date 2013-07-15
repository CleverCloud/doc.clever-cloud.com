---
title: File System Buckets
position: 3
---
## FS Buckets: keep and manage data files.

The Git deployment does not allow to keep generated data files between deployments. To avoid the loss of your data, you have to mount a persistent filesystem. That is what FS Buckets are for.

### Create a FS Bucket
1. Select in the headbar the appropriate organisation.
2. Click on the "Services" tab in your organisation : <figure class="cc-content-img"><a href="/assets/images/intro-services1.png"><img src="/assets/images/intro-services1.png"></a></figure><figcaption>Adding Services in Clever Cloud.</figcaption>
4. You can now choose FS Buckets among the available services.
5. Click on the service's button to create it. You will receive the new bucket's credentials by email a few seconds later.
6. Under **Installed** you will find your running services. A tagging system allows you to easily identify services for differents apps or purposes.
<figure class="cc-content-img"><a href="/assets/images/intro-services2.png"><img src="/assets/images/intro-services2.png"></a></figure>
  <figcaption>
    Services Management in Clever Cloud.
</figcaption>

### Configure your application

To configure your application to use buckets, use the
`clevercloud/buckets.json` file. The `buckets.json` file in the `clevercloud` folder must be located at the root of your application.

The `buckets.json` file must contain the following structure:

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
: The bucket id which begins with `bucket_`. You receive it by email.

* **Folder**
: The folder you want the bucket to be mounted in. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application or via */app/myFolder*.  

<div class="alert alert-hot-problems">
  <h5>Note about target folder</h5>
  <p>
    If the folder already exists and is not empty nor is a directory, the mount of your bucket will be ignored.
  </p>
</div>

### Video demo
<p>
<iframe style="width:640px" height="360" src="http://www.youtube.com/embed/6rJ8zQqIhUw?rel=0&autohide=1&showinfo=0" frameborder="0" controls="0"  allowfullscreen="allowfullscreen"> </iframe>  
</p>
