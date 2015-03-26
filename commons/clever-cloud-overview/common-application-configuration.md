---
title: Common apps configuration
position: 5
---

#  Configuration items available on every instance type.

Each instance type (php,java,python,go,ruby…) has its own configuration. However, some of
these configuration items can be applied to any instance.


## FS Buckets

If you provisioned a FS Bucket add-on, you need to configure it in your repository.
The file you need to achieve that is `clevercloud/buckets.json`.

The `clevercloud` folder must be located at the root of your application.

The `buckets.json` file must contain the following structure:

```javascript
[
  {
    "bucket" : "bucketId",
    "folder" : "/myFolder"
  }
]
```

It's a json array containing objects with two fields:

* **bucket**
: The bucket id you received by email which begins with `bucket_`.

* **folder**
: The folder you want the bucket to be mounted in. Using the example
*myFolder*, you can access your buckets via the *myFolder* folder at
the root of your application or via */app/myFolder*.

<div class="alert alert-hot-problems">
  <h5>Important note about target folder</h5>
  <p>The folder must not exists in your repository (or it needs to be empty). Otherwise, the mount of your bucket will be ignored.
  </p>
  <p>You can mount the same bucket in different folders, but they will share the same content, so it's not the solution. You should prefer to mount the bucket in only one folder and then manage multiple subfolders in it.
  </p>
</div>


## Private SSH key

If your company manages its own artifacts in a private repository (like, you can only
access them via git+ssh or sftp), and you need a private key to connect to the server, you
can commit them in your application's Clever Cloud repository and then add the
`clevercloud/ssh.json`.


The content of this file is pretty straight-forward:

```javascript
{
    "privateKeyFile": "path/to/file"
}
```

The `privateKeyFile` field must be a path to a SSH private key. The path must be relative
to the root of your repository. e.g. if your private key file is in the `clevercloud`
folder and is named `my_key`, the `privateKeyFile` field will be `"clevercloud/my_key"`.


That key will be installed as `~/.ssh/id_rsa` before the start of the build. So the
dependency manager will use it to fetch libs only accessible by ssh.

## Post deploy hook

If you need to perform additional actions after your app is started, you can define a
"postDeploy" hook in your `<instance_type>.json` file (e.g. if you are deploying a ruby app,
put it in your `ruby.json` file):

```javascript
{
  "hooks": {
    "postDeploy": "./clevercloud/myScript"
  }
}
```

The `postDeploy` field must contain the path to a script, relative to the root of your
repository. The script file **MUST** have execution right set.

In the example above, the script is a file named `myScript` in the `clevercloud` folder.
That is, right besides the `<instance_type>.json` file.
