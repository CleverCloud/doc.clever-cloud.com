---
layout: page

title: Node dependencies
tags:
- Javascript
- Node.js

---

# Describing package.json

For every Node.js application you want to deploy on the Clever Cloud, you **HAVE&nbsp;TO**
provide a `package.json` file at the root of your project’s directory, even if your app has no dependencies.

If you already are a Node.js guru, you probably won’t have to change anything to that
file. Just check the required fields below.

The `package.json` file should look like the following:

{% highlight javascript%}
    {
        "name" : "myapp",
        "version" : "0.1.0",
        "main" : "myapp.js",
        "scripts" : {
            "start" : "node myapp.js"
        },
        "engines" : {
            "node" : ">=0.6"
        }
    }
{% endhighlight %}

## The required fields

The following table describes each of the fields formerly mentioned.

<table id="nodedeps" class="table table-bordered table-striped">
	<thead>
		<tr>
			<th>Usage</th>
			<th>Field</th>
			<th>Description</th>
		</tr>
	</thead>
	<tbody>
		<tr>
		<td><span class="label label-important">Required</span></td>
		<td>name</td>
		<td>Name of your application. You need to fill this field.</td>
		</tr>
		<tr>
		<td><span class="label label-important">Required</span></td>
		<td>version</td>
		<td>Version of you application. You need to fill this field.</td>
		</tr>
		<tr>
			<td class="cc-depusage" rowspan="2"><span class="label label-important">At least one</span></td>
			<td>main</td>
			<td>This field allows you to specify the file you want to run. It should
			be the relative path of the file starting at the project's root. It's used prior to the next one.</td>
		</tr>
		<tr>
			<td>scripts.start</td>
			<td>This field provides a command line to run. It is required if the <code>main</code> one is missing. You need one
			of <code>scripts.start</code> and <code>main</code>. If both exist, we use the <code>main</code> one.</td>
		</tr>
		<tr>
			<td class="cc-depusage" ><span class="label label-inverse">Optional</span></td>
			<td>engines.node</td>
			<td>Sets the node engine version you app runs with. Any ">=" version will lead to
			run the application with the latest local version. Any "A.B.x" version will lead
			to run the application with le latest "A.B" local version. If this field is
			missing, we use the greatest local version.</td>
		</tr>
	</tbody>
</table>

## More information

The `package.json` file is commonly used for dependency management by npm. A lot
of information can be put in that file. You can find more help about the `package.json` file at <a href="http://package.json.nodejitsu.com/">http://package.json.nodejitsu.com/</a>.
