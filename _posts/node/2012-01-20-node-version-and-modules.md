---
layout: page

title: Modules & versions
tags:
- Javascript
- Node.js

---

# Node.js supported versions

The Clever Cloud can virtually run any version of node >= 0.6 and any module. We did not tried
lesser (pre-npm) versions.

## Pre-installed versions and modules

We currently host the following Node.js versions, with these modules already installed:

**You can use any version of node you need and any modules you need. Preinstall version is only for faster deployment process.**

<table class="table table-bordered table-striped">
	<thead>
		<tr>
			<th>Available Node Versions</th>
			<th>Pre-installed modules</th>
		</tr>
	</thead>
	<tbody>
		<tr><td>v0.10.5 </td><td>socket.io</td></tr>
		<tr><td>v0.10.4 </td><td>express  </td></tr>
		<tr><td>v0.10.3 </td><td>async    </td></tr>
		<tr><td>v0.10.2 </td><td>mysql    </td></tr>
		<tr><td>v0.10.1 </td><td>pg       </td></tr>
		<tr><td>v0.10.0 </td><td>couchbase</td></tr>
		<tr><td>v0.9.12 </td><td>         </td></tr>
		<tr><td>v0.9.11 </td><td>         </td></tr>
		<tr><td>v0.9.10 </td><td>         </td></tr>
		<tr><td>v0.9.9  </td><td>         </td></tr>
		<tr><td>v0.9.8  </td><td>         </td></tr>
		<tr><td>v0.8.18 </td><td>         </td></tr>
		<tr><td>v0.8.19 </td><td>         </td></tr>
		<tr><td>v0.8.20 </td><td>         </td></tr>
		<tr><td>v0.8.21 </td><td>         </td></tr>
		<tr><td>v0.7.12 </td><td>         </td></tr>
		<tr><td>v0.6.21 </td><td>         </td></tr>
	</tbody>
</table>


We will add the new versions as they go out. They will all be documented in this section.

## Defining "pre-installed"

The above table describes the versions and modules pre-installed for these versions.
These modules are available at deploy time, without the need to download and
install them.

If you use modules that are not pre-installed, we will just get them with npm
(provided they are in the npm repository), and install it before we start your
application. The deploy will then be a little longer, due to probable
compilation of some of these modules.

If you think more modules are commonly used and should be pre-installed, do not
hesitate to contact us at <mailto:support@clever-cloud.com>
