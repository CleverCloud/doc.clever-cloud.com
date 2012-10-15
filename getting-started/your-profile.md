---
layout: page

id: your_profile
parent: getting_started
next: create_an_app
prev: create_an_account
---

Your profile
============
The **Profile** section is composed of four tabs&nbsp;: *Personal informations* *Passwords*, *SSH keys* an *account deletion*. 

Personal informations
---------------------
<img class="thumbnail img_doc" src="/img/profil.png">  
When you click on the **Profile** section's button, you are directed on your informations by default. 
Here, you can fill all the fields to complete your profile.

Change your password
--------------------

Into **Profile** the *Password* tab allow you to change your password.  
Simply enter your old password, then the new one and confirm it. Your new password is setted !

SSH keys
------------
SSH keys are used to establish a secure connection between your computer and Clever Cloud.  
Adding a SSH key to your Clever Cloud's account is mandatory to deploy via Git.  
Please note that SSH keys are unique to one user.  
####How to get a SSH key?
* Open a terminal app on linux/mac — Git Bash on Windows — to check if you have existing SSH keys.
* If not, generate a new SSH key with the following command&nbsp;:
<pre>ssh-keygen -t rsa -C "your_email@youremail.com"</pre>
* Then add your SSH key to Clever Cloud, by copying the content of the file:
<pre>~/.ssh/id_rsa.pub</pre>

####Declaring your public SSH key on Clever Cloud
1. To add a SSH key, click on Profile > SSH keys.  
2. In the first field, pass a name for your SSH keys, and in the the second, paste your public SSH key.<img class="thumbnail img_doc" src="/img/ssh1.png">  
3. Then, the SSH key is added on the main frame. You can delete it if needed or add another one.<img class="thumbnail img_doc" src="/img/ssh2.png">  
4. Once you have declared your SSH key and created an app, a unique Git repository will be generated fo your project.  

A push from your computer to this repository will deploy your app in SSH, using your public key.
