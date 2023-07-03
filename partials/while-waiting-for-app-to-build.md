While waiting for your app to build, it can be the perfect time to discover
how you can see your build/deployment logs on Clever Cloud.

<details open="true">
<summary>Using the web console</summary>

1. Go to [console.clever-cloud.com](https://console.clever-cloud.com/)
2. Select your personal space or organization in the leftmost sidebar
3. Select your example Swift application in the main sidebar
4. Select "Logs" in the secondary sidebar

</details>

<details>
<summary>Using our CLI</summary>

To see an application's logs using our [CLI][cli], you have two options:

- The simple way

  ```bash
  clever logs
  ```

- The advanced way

  ```bash
  clever ssh
  journalctl -efa -u bas
  ```

For more information about SSH access using the CLI, see [SSH access with Clever Tools][ssh-access].

</details>

[cli]: <{{< ref "/getting-started/cli.md" >}}>
[ssh-access]: <{{< ref "/reference/clever-tools/ssh-access.md" >}}> "SSH access with Clever Tools | Clever Cloud Documentation"
