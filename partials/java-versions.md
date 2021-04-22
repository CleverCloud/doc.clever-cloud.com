## Available Java versions

Simply set the environment variable **CC_JAVA_VERSION** to the version you want

Accepted values are `7`, `8`, `11`, `16` or `graalvm-ce` (for GraalVM 21.0.0.2, based on OpenJDK 11.0).

We follow the public Java [roadmap](https://www.oracle.com/java/technologies/java-se-support-roadmap.html) by supporting both LTS and latest non-LTS versions.

{{< alert "info" "Default version" >}}
    <p>We are using Java version 11 by default.</p>
    <p>New applications will have the **CC_JAVA_VERSION** environment variable set to 11.</p>
{{< /alert >}}
