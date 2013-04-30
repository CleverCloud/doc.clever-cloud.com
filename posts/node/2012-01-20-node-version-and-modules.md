---
layout: page
title: Modules & versions
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
		<tr><td>v0.10.3 </td><td>socket.io</td></tr>
		<tr><td>v0.10.2 </td><td>express  </td></tr>
		<tr><td>v0.10.1 </td><td>async    </td></tr>
		<tr><td>v0.10.0 </td><td>mysql    </td></tr>
		<tr><td>v0.9.3  </td><td>pg       </td></tr>
		<tr><td>v0.9.2  </td><td>         </td></tr>
		<tr><td>v0.9.1  </td><td>         </td></tr>
		<tr><td>v0.9.0  </td><td>         </td></tr>
		<tr><td>v0.8.9  </td><td>         </td></tr>
		<tr><td>v0.8.8  </td><td>         </td></tr>
		<tr><td>v0.8.16 </td><td>         </td></tr>
		<tr><td>v0.8.15 </td><td>         </td></tr>
		<tr><td>v0.8.14 </td><td>         </td></tr>
		<tr><td>v0.8.13 </td><td>         </td></tr>
		<tr><td>v0.8.12 </td><td>         </td></tr>
		<tr><td>v0.8.11 </td><td>         </td></tr>
		<tr><td>v0.8.10 </td><td>         </td></tr>
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
