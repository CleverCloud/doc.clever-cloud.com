---
layout: page

title: Git deployment
tags:
- Java
- Scala
---
# Git Deployment
*You will need git on your computer to deploy via this tool. Here is the official website of Git to get more informations&nbsp;: <a href="http://git-scm.com">git-scm.com</a>*

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following message&nbsp;:
<img class="thumbnail img_doc" src="/img/newapp6.png">
1. Click on "close". The "General information" page give you your git deployement URL. Copy it in your clipboard.
2. On your computer, go into your application repository. 
If you didn't already track your app with git, start by typing:  

    	$ git init

3. Then, use the "git remote" command to add the deploy URL :

		$ git remote add clever git+ssh://gitolite@git.clever-cloud.com/app_<your_app_id>.git

4. The last step is to push your application&nbsp;:

		$ git push clever master

If you send a Java app, and want to use Maven, Ant or SBT, don't forget to include the cc_conf.json. For more details, check the [deployment configuration page](/app-configuration/cc-conf.html)
