---
title: SSH Keys
position: 1
---

## SSH keys

SSH keys are used to establish a secure connection between your computer and Clever Cloud.  
You must add a SSH key to your Clever Cloud's account to deploy via Git.  
A user can have multiple SSH keys.

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
  <p>It's important to not add the same SSH key to more than one account. Otherwise, you'll get an error in the console.</p>
</div>

### How to add your SSH key on Clever Cloud?

#### Find your SSH key

You can already have an SSH key and so do not need to generate a new one. To check if you have one, follow the steps:  

1. Wether you use Mac, Linux or Windows, open your Terminal application. On Windows, it is provided by this software: [http://msysgit.github.com](http://msysgit.github.com)
2. Run ``` $ cd ~/.ssh/``` in your Terminal.
3. If it is found, you do not need to generate a new one, simply switch to the following "Add you key on Clever Cloud"!

<br/>

#### Create your SSH key

1.  In your Terminal, enter the following bash line:

    ```bash
    $ ssh-keygen -t rsa -C "your_email@youremail.com"
    ```  
    <br/>
    This command creates a new ssh key using the provided email, so that the owner of the key can be identified.

2.  When asked, enter a passphrase:

    ```bash
    Generating public/private rsa key pair.
    Enter file in which to save the key (/your_home_path/.ssh/id_rsa):
    //Now you should enter a passphrase.
    Enter passphrase (empty for no passphrase): [Type a passphrase]
    Enter same passphrase again: [Type passphrase again]
    ```

    <div>Which should give you something like this:</div>

    ```bash
    Your identification has been saved in /your_home_path/.ssh/id_rsa.
    Your public key has been saved in /your_home_path/.ssh/id_rsa.pub.
    The key fingerprint is:
    01:0e:e5:2d:ab:98:d6:17:a1:6d:f0:68:9f:d0:a2:db your_email@youremail.com
    ```

<br/>

#### Add your SSH key on Clever Cloud

To declare your public SSH key on Clever Cloud, click on your account, then on the left menu "Profile", and "ssh keys".

Add the key by entering a name and the public SSH key. The key is the entire container of the *.pub file.
<br><figure class="cc-content-img"><a href="/assets/images/ssh1.png"><img src="/assets/images/ssh1.png"></a></figure>
  <figcaption>
    The SSH Key manager.
  </figcaption>
<br>

You can delete it if needed or add another one.  


**Rememeber**: your public SSH key should be setup on only one account.  

If you see "*access denied*" or "*password:*", your SSH key may be invalid. 

<i class="icon-question-sign"></i> **Need help about SSH keys?**  
Contact us at <support@clever-cloud.com> or you can [read more about SSH Keys](http://git-scm.com/book/en/Git-on-the-Server-Generating-Your-SSH-Public-Key).