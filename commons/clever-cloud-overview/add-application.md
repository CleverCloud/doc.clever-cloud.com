---
title: Deploy an application
position: 4
---

## Deploy an application

Application deployment has two main steps: application creation and application deployment.

### Create an application

1. Create a new app by clicking on the **Add an App** button, in the headbar. 
2. Enter your application's name and description and click "Next".
<figure class="cc-content-img">
  <img src="/assets/images/appjavawar.png"/>
</figure>
3. Then select the language/framework:  <figure class="cc-content-img"><img src="/assets/images/javawarapp.png"></figure>
3. *Optional:* in case of PHP or static applications, you can choose between FTP and Git deployment
4. Check that the information are correct and validate: <figure class="cc-content-img"><img src="/assets/images/appcreationreviewjavawar.png"></figure>
5. *Optional*: <a href="/databases-and-services/add-service/">add a database or service</a>



### Git Deployment
*To deploy via Git, you need it installed on your machine. You can find more information on Git website: <a href="http://git-scm.com">git-scm.com</a>*  

Follow these steps to deploy your application:

1. Get the git deployment url in the application information page: ``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``.  
2. In your terminal, go to your application repository. If you do not already track your app with git, start by typing:

	```bash
	$ git init
	$ git add .
	$ git commit -m "first commit"
	```

3. Then, link your local repository to Clever Cloud by providing the Git remote url:

	```bash
	$ git remote add <remote-name> <your-git-deployment-url>
	```

4. Push your application to Clever Cloud:

	```bash
	$ git push <remote-name> <branch-name>:master
	```

  <div class="alert alert-hot-problems">
    <h4>Warning:</h4>
    <p>The remote branch on Clever Cloud is <strong>ALWAYS</strong> master. If your local branch is not "master", use this syntax:</p>
    <pre>git push < name > yourbranch:master</pre>
  </div>
  
  Checkout your application <b>logs</b> in the dashboard to <b>monitor the deployment</b>.

### FTP Deployment

It is possible to deploy via FTP with PHP and static applications.  

To deploy via FTP, you need an FTP software installed on your machine. Filezilla is one of them.

When you have chosen to deploy your application via FTP, you have received associated FTP credentials by email. Enter the host, username and password in your FTP software and deploy the files.

More documentation about Filezilla: <a href="https://wiki.filezilla-project.org/FileZilla_Client_Tutorial_(en)" target="_blank">https://wiki.filezilla-project.org/FileZilla_Client_Tutorial_(en)</a>