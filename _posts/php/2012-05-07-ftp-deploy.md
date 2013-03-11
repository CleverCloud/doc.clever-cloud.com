---
layout: page

title: FTP deployment
tags:
- PHP
---
# FTP Deployment

After you created an app in the [console](https://console.clever-cloud.com), the console prompt you the following screen&nbsp;:
<img class="thumbnail img_doc" src="/img/newapp6.png">

1. Your FTP credentials are sent by email.
2. Use a FTP client with FTPS compliance, able to negotiate using the AUTH TLS method.  
If your client does not handle this secure mode, the server will refuse the connection.
3. The credentials are like:  

		host = ftpes://upload.clever-cloud.com/
		username = userxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		password = ***************

<div class="alert alert-hot-problems">
  <h4>Important:</h4>
  <p>Unlike git, FTP deployment starts a instance as soon as you create it.</p>
  <p>So drops consumption will be effective immediately after your FTP credentials will be sent.</p>
</div>