---
title: Apps Management
position: 1
---

## Apps Management

### Custom domain Names
When creating an app, you have two choices for domain names:

* Using a **cleverapps.io** sub-domain for testing purpose or a quick deploy.
* Or/and using a custom domain name.

**Choosing a custom domain name require a DNS configuration on the domain's registrar.**

After your app creation, go to the **Domain Names** section and enter your domain name. You should note that sub-domains are supported: 
<figure class="cc-content-imglarge"><img  src="/assets/images/domain1.png"></figure>

#### DNS Configuration

Then you have to create a DNS record for this custom domain name.

You can use a **CNAME** record pointing on <pre>domain.clever-cloud.com.</pre>

But if you have other records on the same level *(eg. your root domain)* you **MUST\*** use a **A** record pointing on <pre>62.210.121.146</pre>
<figure class="cc-content-imglarge">
  <img  src="/assets/images/domain2.png">
</figure>
*\* Please refer to the section 2.4 "CNAME record" of <a href="http://tools.ietf.org/html/rfc1912">RFC 1912</a>*


### Logs

The Clever Cloud console gives you the ability to get logs from your apps.

To retrive an app's logs, select your app in the headbar.
Then, click on the *Logs* tab in the left-pane:

<figure class="cc-content-img">
  <img style=" margin: auto; display: block" src="/assets/images/logs.png"/>
  <figcaption>
    App's left-pane.
  </figcaption>
</figure>

### SSH keys

SSH keys are used to establish a secure connection between your computer and Clever Cloud.  
Adding a SSH key to your Clever Cloud's account is mandatory to deploy via Git.  
Please note that SSH keys are unique to one user.

1. First we will use the Terminal on linux or Mac for this operation.  
This could be Git Bash on Windows (you can download it here: [http://msysgit.github.com](http://msysgit.github.com)).
Open up the command line and run:
``` $ cd ~/.ssh/```  
This command will check if there is a directory named ".ssh" in your user directory.

2. If needed, generate a new SSH key. To do this, enter the code below. We want the default settings so when asked to enter a file in which to save the key, just press enter:

```java
$ ssh-keygen -t rsa -C "your_email@youremail.com"
```
The command above creates a new ssh key using the provided email, used to quiclky identify the user of this key.

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

3. 
To declare your public SSH key on Clever Cloud, click on My account and go to SSH keys, located in the left pane.
<figure class="cc-content-img">
  <img style=" margin: auto; display: block" src="/assets/images/logs.png"/>
  <figcaption>
    App's left-pane.
  </figcaption>
</figure>


In the first field, pass a name for your SSH keys, and in the the second, paste your public SSH key.
<br><figure class="cc-content-img"><a href="/assets/images/ssh1.png"><img src="/assets/images/ssh1.png"></a></figure>
  <figcaption>
    The SSH Key manager.
  </figcaption>
<br>

You can delete it if needed or add another one.  

Once you have declared your SSH key and created an app, a unique Git repository will be generated on the server for your project. 
A push from your computer to this repository will deploy your app via SSH, using your public key.


### Start, stop and delete
### Cold start
### Scalability management