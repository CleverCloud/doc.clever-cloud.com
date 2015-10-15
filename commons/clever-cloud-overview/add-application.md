---
title: Deploying an application
position: 4
---

# Deploying an application

There is two steps in this section: application creation (requiring actions from
dashboard) and application deployment (requiring actions from git for your FTP access).

## Create an application

 1. First, select the proper organization you want to add to application to. Then,
 click on the **Add an application** button, in the **Organization Manager** panel.
 This starts the application creation wizard. If you account has been linked to
 GitHub, you can select a repository from your GitHub account.

 2. Then select the language or the framework you need:
 <figure class="cc-content-img"><img src="/assets/images/select-lang.png"></figure>

 3. *Optional:* in case of PHP or static applications, you can choose between FTP
 and Git deployment.

 4. In the next step, you will be prompted to select your scaling configuration.
 If you need help to configure your scaling options, please refer to
 [the dedicated section](/clever-cloud-overview/scaling/).

 <figure class="cc-content-img">
   <img src="/assets/images/select-scalab.png"/>
 </figure>

 5. Enter the name and description of you app.

 6. *Optional*: [add an add-on](/addons/clever-cloud-addons/) to your app.

## Git Deployment

*To deploy via Git, you need it installed on your machine. You can find more
information on Git website: [git-scm.com](http://git-scm.com)*

*Note that during the deployment, the .git folder is automatically deleted to
avoid security problems. If you need to know which version is used on the server
please use the `COMMIT_ID` [environment variable](/admin-console/environment-variables/).*

Follow these steps to deploy your application:

 1. Get the git deployment url in the application information page:
 ``git+ssh://git@push.clever-cloud.com/<your_app_id>.git``.

 2. In your terminal, go to your application repository. If you do not already
 track your app with git, start by typing:

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
   <p>You can only push to the <strong>master</strong> branch for deployment.
   Trying to push in another branch will trigger an error.</p>
   <p>In order to push to <strong>master</strong> from a non-master local branch, use this syntax:</p>
   <pre>git push &lt; name &gt; yourbranch:master</pre>
 </div>

 Checkout your application **logs** in the dashboard to **monitor the deployment**.

 <div class="alert alert-hot-problems">
   <h4>Troubleshooting:</h4>
   <p>If the remote ask you for a password right after a git push attempt, this is may due to a SSH Key misconfiguration.
   <br>Add your SSH key in your profile here:
   <a href="https://console.clever-cloud.com/users/me/ssh-keys">https://console.clever-cloud.com/users/me/ssh-keys</a></p>
   <p>The full tutorial about adding SSH key is here: <a href="/doc/admin-console/ssh-keys/">Adding SSH keys</a> </p>
 </div>

### Automatic Deployment with GitHub

If you want to enable automatic deployment, it’s easy: in your application
information tab, if you have created your application from a GitHub one, you will
see a new field: a secret for your GitHub application’s settings. Here are the
steps to trigger a deploy on Clever Cloud from a push to GitHub:

 1. Grab this secret and go to the GitHub repository settings.

 2. Click on “add a service” and select “Clever Cloud” from the list.

 3. Paste the secret

From now, a push to your GitHub repo will also trigger a Clever Cloud deployment.

## FTP Deployment

It is possible to deploy via FTP with PHP and static applications.  

To deploy via FTP, you need an FTP software installed on your machine. Filezilla
is one of them.

When you have chosen to deploy your application via FTP, a <a
href="/addons/clever-cloud-addons/#fs-buckets-file-system-with-persistence">FS
Bucket</a> has been created with an ID matching your application's ID.
You will find the FTP credentials in the configuration tab of this
particular FS Bucket.

[More documentation about Filezilla.](https://wiki.filezilla-project.org/FileZilla_Client_Tutorial_(en))


<div class="alert alert-hot-problems">
<h4>Warning:</h4>
<p>An FTP application is automatically started once the application is created,
even if no code has been sent.</p>
</div>


<div class="alert alert-hot-problems">
<h4>Our advice</h4>
<p>FTP deployment is ok for small websites but not for large ones. We strongly
recommend you to use <b>Git</b> deployment for <b>large PHP websites</b>.</p>
</div>


