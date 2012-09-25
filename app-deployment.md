---
layout: page

id: app_deployment
---

# App deployment

On this page will follow a tutorial on how to deploy your application on the Clever Cloud.  
After you created an app in the [console](https://console.clever-cloud.com), we will
give you a git address like the following:
<img class="thumbnail img_doc" src="/img/newapp6.png">

Then, go into your application repository. You only have three commands to execute.    
If you didn't already track your app with git, start by doing a

     $ git init

Then, git remote the address we gave you:

    $ git remote add clever git+ssh://gitolite@git.clever-cloud.com/app_<app_id>.git

The last step is to push your application. Simply execute:

    $ git push clever master

If you send a Java app, and want to use Maven, Ant or SBT, don't forget to include the cc_conf.json. For more details, check the [deployment configuration page](/app-configuration/cc-conf.html)
