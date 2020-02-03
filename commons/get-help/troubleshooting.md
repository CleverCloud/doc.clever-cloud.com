---
title: Troubleshooting
position: 4
shortdesc: Common issues and errors you may have
tags:
- support
---

## Git and File Persistence

> "Why are my files disappearing after an application restart?”

Clever Cloud use Git to transfer your code and application's assets from your local host to your scaler. If your application writes files on the local file system, those files are not committed: so you can't save these files from a instance to an other.

For most of Cloud providers, the use of the file system is not a good practice. But we know it could be sometimes pretty useful. That's why we provide an on-demand file system, easily pluggable to your app. In that case, your files will not be stored on the Git file system, but on a clustered file system, dedicated to it, accessible via FTP. This is the FS Bucket add-on.

Setup FS Bucket for your app is kinda straightforward, because it consist in add a simple Json file at the root of your application.

The Json contains two parameters: the ID of the FS Bucket, and the path you want to use as a FS.

*Related page: [File System buckets](/doc/addons/clever-cloud-addons/#fs-buckets-file-system-with-persistance/).*


## Error about Empty Repository

> “Logs are telling me \"warning: You appear to have cloned an empty repository\"”

This usually means that you created an application and asked to start it in the console without having pushed to it before.

*You are not working on the master branch and pushed your branch to Clever Cloud*

Clever Cloud uses the master branch to deploy your application but this does not mean that you cannot use another one on your computer.
The only thing that differs if you use another branch than the master one on your computer is that you need to explicitly tell that Clever Cloud needs to consider your branch as the master one.

```bash
$ git push <cleverRemote> <yourSpcificBranchName>:master
```

If you called the Clever Cloud repository "clever" and your local branch is "production", this becomes

```bash
$ git push clever production:master
```

*Related page: [Deploy an application](/doc/clever-cloud-overview/add-application/).*

## Error about not being a git repository

> “I get errors telling me that it's not a git repository, what does it mean?

This can mean several things, depending on the exact error message

*fatal: Not a git repository (or any of the parent directories)*

This means that the folder in which you are is not a git repository.
Please refer to our "git deployment" documentation for how to set it up properly.

*fatal: 'clever' does not appear to be a git repository*

"clever" is a name used in our examples to represent the Clever Cloud servers.
In order to be able to use the same name for yourself, you will add to create it like this:

```bash
$ git remote add clever <your-git-deployment-url>
```

*Related page: [Git deployment](/doc/clever-cloud-overview/add-application/)*

## Fail to push to a repository

It might be because your SSH agent is not properly configured. Please check [the SSH page](/doc/admin-console/ssh-keys#check-your-ssh-configuration)

## Node failed to deploy

> “My Node.js application is unable to start. Logs are not helping”

This kind of silent error may be due to the server port your have setup in your application. Make sure your app is listening on 8080.
Most of the time, this simple line could do the trick in your main JS file:

```javascript
// Listen on port 8080
server.listen(8080);
```

*Related page: [Deploy Node.js apps](/doc/nodejs/nodejs/).*

## My GitHub organization is not listed on the app creation page

GitHub does not give us access to organizations created or joined *after* you've linked your GitHub account to
Clever Cloud (which is a good thing). So you need to let the Clever Cloud API
access it. You can do that on <https://github.com/settings/applications>.
