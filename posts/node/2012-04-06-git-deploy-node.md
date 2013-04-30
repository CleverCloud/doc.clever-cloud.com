---
layout: page
title: Git deployment
---
# Git Deployment
*You will need git on your computer to deploy via this tool. Here is the official website of Git to get more informations&nbsp;: <a href="http://git-scm.com">git-scm.com</a>*
## Create a Node app in the console
In order to host a node app on Clever Cloud, you will need to declare this new app by following [these steps](/create-an-app/).

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following message&nbsp;:
<img class="thumbnail img_doc" src="/img/newapp6node.png">

## Git and deployement process
1. Click on "close". The "General information" page gives you your git deployment URL which is of the form ``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``. Copy it in your clipboard.
2. On your computer, go into your application repository. 
If you didn't already track your app with git, start by typing:  

    	$ git init
    	$ git add .
		$ git commit -m "init"

3. Then, use the "git remote" command to add the deploy URL:

		$ git remote add <name> <your-git-deployment-url>

4. The last step is to push your application:

		$ git push <name> master

If you send a Node.js app, do not forget the [package.json](/node-dependencies) file. 
