---
layout: page
title: Profile
---

## Your profile

*The Profile section is composed of four tabs&nbsp;: Personal informations, Passwords, SSH keys an account deletion*. 

### Personal informations
When you click on the **Profile** section's button, you are directed on your personal informations by default.  
Here, you can fill all the fields to complete your profile.
<img  src="/img/profil.png">

### SSH keys
SSH keys are used to establish a secure connection between your computer and Clever Cloud.  
Adding a SSH key to your Clever Cloud's account is mandatory to deploy via Git.  
Please note that SSH keys are unique to one user.
####Step 1: Look for existing SSH keys
We will use the Terminal on linux or Mac for this operation.  
This could be Git Bash on Windows (you can download it here: [http://msysgit.github.com](http://msysgit.github.com)).
Open up the command line and run:  

```java
$ cd ~/.ssh/
// This command will check if there is a directory named ".ssh" in your user directory
```

####Step 2: Generate a new SSH key
To generate a new SSH key, enter the code below. We want the default settings so when asked to enter a file in which to save the key, just press enter:

```java
$ ssh-keygen -t rsa -C "your_email@youremail.com"
```
Creates a new ssh key using the provided email

```java
Generating public/private rsa key pair.
Enter file in which to save the key (/your_home_path/.ssh/id_rsa):
//Now you should enter a passphrase.
Enter passphrase (empty for no passphrase): [Type a passphrase]
Enter same passphrase again: [Type passphrase again]
```

Which should give you something like this:

```haskell
Your identification has been saved in /your_home_path/.ssh/id_rsa.
Your public key has been saved in /your_home_path/.ssh/id_rsa.pub.
The key fingerprint is:
01:0e:e5:2d:ab:98:d6:17:a1:6d:f0:68:9f:d0:a2:db your_email@youremail.com
```

####Step 3: Declaring your public SSH key on Clever Cloud
To add a SSH key, click on Profile > SSH keys.<img  src="/img/ssh0.png">  

In the first field, pass a name for your SSH keys, and in the the second, paste your public SSH key.<img  src="/img/ssh1.png">  

Then, the SSH key is added on the main frame. You can delete it if needed or add another one.<img  src="/img/ssh2.png">  

Once you have declared your SSH key and created an app, a unique Git repository will be generated on the server for your project. 
A push from your computer to this repository will deploy your app in SSH, using your public key.

The commands required to push an application are also documented:  
Check our [Java tutorial](/java/git-deploy.html) for an example of git deployment.
