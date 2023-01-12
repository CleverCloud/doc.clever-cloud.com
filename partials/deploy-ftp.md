## FTP Deployment

Make sure you have [Filezilla](https://filezilla-project.org/) or an other FTP software installed in your machine.

When you have chosen to deploy your application via FTP at the application creation, a free [FS Bucket]({{< ref "deploy/addon/fs-bucket.md" >}}) has been created with an ID matching your application's ID. You will find the FTP credentials in the configuration tab of this particular FS Bucket.

Just follow the instructions of your FTP Software to send code to Clever Cloud.

{{< alert "warning" "Warning:" >}}
<p>An FTP application is automatically started once the application is created, even if no code has been sent.</p>
{{< /alert >}}

Refer to the [quickstart]({{< ref "/getting-started/quickstart.html" >}}) for more details on FTP deployments.

### Transfer files programmatically

Install [rclone](https://rclone.org/).

Then create a config and use it as a target to transfer a directory:

```bash
rclone config create remote ftp host ${FTP_HOST} user ${FTP_USER} pass ${FTP_PASSWORD}
rclone sync -v ${PWD}/my_local_directory remote:
```
