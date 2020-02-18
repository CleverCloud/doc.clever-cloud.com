---
title: Deploying a WordPress website
shortdesc: This article shows you how to deploy a WordPress-based website on Clever Cloud.
tags:
- php
- wordpress
- immutable
---

## Deploy the immutable way using Bedrock

We strongly recommend you use a forked version of [this repo from zileo-net](https://github.com/zileo-net/clevercloud-wordpress) wich is preconfigured for Clever Cloud.

### Initial deployment

It will assume your GitHub account is linked to your CleverCloud account. If not, you'll just have to do the same steps but cloning and pushing the project yourself to CleverCloud.

1. Fork this [repository](https://github.com/zileo-net/clevercloud-wordpress)
2. Log in to your CleverCloud console
3. Create a new application, by selecting this project fork as a *PHP* one
4. Add one *MySQL database* add-on
5. On next page, edit the environment variables in expert mode and paste the env salts generated [https://cdn.roots.io/salts.html](https://cdn.roots.io/salts.html)(copy and paste what's under env format)
6. Add these environment variables (do not forget to replace <your-domain.tld> with the right value)
```
WP_ENV='production'
WP_HOME='https://<your-domain.tld>'
WP_SITEURL='https://<your-domain.tld>/wp'
```
7. While your app starts, create a *Cellar S3 storage* add-on, and link it to your application
8. On the add-on configuration page, create one bucket
9. Go back in your application configuration and add the environment variable `CELLAR_ADDON_BUCKET` with the name of your bucket
10. Apply changes by restarting your application
11. Don't forget to set up your domain name as configured for `WP_HOME` (or one `*.cleverapps.io` for testing purpose)
12. You'll then have access to the installation page of WordPress
13. After installed, go to your plugins home page and activate `S3 Uploads`

**Important note :** At this time, your WordPress installation is not capable of sending any emails. Follow  [documentation for sending emails in PHP apps](https://www.clever-cloud.com/doc/php/php-apps/#sending-emails) to configure your SMTP server, of activate and configure the `Mailgun` plugin installed by default.

### Differences with Bedrock

For those who want or need to go deeper regarding Bedrock, here are the small differences between [this fork](https://github.com/zileo-net/clevercloud-wordpress) (based on version __1.12.8__) and a standard Bedrock install.
- You don't need any `.env` file for your environment variables, it can be useful if you want to run your WordPress locally
- `config/application.php` has been modified to directly use MySQL and Cellar environment variables shared by CleverCloud
- Plugin `humanmade/s3-uploads` added by default to use S3 storage for media files
- `web/app/mu-plugins/s3-uploads-filter.php` have been added to use a Cellar endpoint in place of an AWS one
- `.htaccess` have been included by default

## Installing themes and plugins

Your WordPress installation is now fully managed by _composer_ and [WordPress Packagist](https://wpackagist.org). So to install themes or plugins, you'll have to add them to the `composer.json` file, and commit. The dependencies will be fetched by _composer_ during CleverCloud rebuild of your project.

**Important note :** Pay attention to how you define your [dependencies with composer](https://getcomposer.org/doc/01-basic-usage.md#installing-dependencies), being strict, or having them automatically updated if needed when it rebuilds.
The stricter way would even be to locally `composer update` your project and commit your own `composer.lock` file.

### Performance plugins

<div class="alert alert-hot-problems">
<h4>Warning:</h4>
 <p>We recommend you to <strong>not</strong> use performance plugins like W3 Total Cache or JetPack as they are
 intended to be used on a shared hosting server.<br />
 We noticed performances problems when performance plugins are enabled and we recommend to use [Varnish](/doc/php/varnish/) and [Redis](/doc/addons/redis/) if you need performance optimisations on Clever Cloud.</p>
</div>

## Keeping WP updated

As for themes and plugins, keeping WordPress updated must be done by the dependencies way. That means you'll have to change the WordPress version in your `composer.json` file and commit. Once restarted, if you are connected as administrator, a page will propose you to do the database update, if any.

## Optimise and speed-up your WordPress

There are multiple ways to optimise your WordPress and speed-up its response time.
We provide different tools and software to help you in this task as [Varnish](/doc/php/varnish/) for the HTTP cache,
and [Redis](/doc/addons/redis/) for the object caching.

### HTTP Cache with Varnish

Enabling [Varnish](/doc/tools/varnish/) for your application is very simple. All instances of PHP provide
[Varnish](/doc/tools/varnish/), you just have to configure your application to use it.

1. To use Varnish in your application, you have to create a `varnish.vcl` file in the `clevercloud` folder of
your application. If this folder doesn't exist, create it in the **root** of your project.

2. Copy [this code](https://raw.githubusercontent.com/CleverCloud/varnish-examples/master/wordpress.vcl) into the
`varnish.vcl` file you just created. It'll configure Varnish to work with your WordPress.

3. To properly purge the Varnish cache of your application when a post is created/updated, a comment is posted, ...
we recommend you to install the [Varnish HTTP Purge](https://wordpress.org/plugins/varnish-http-purge/) plugin to
your WordPress. It'll purge the Varnish cache for you and give you the possibility to purge it manually.

If you need to manually purge the Varnish cache, the plugin provides a **Purge Varnish cache** button on the top bar
of your website.

### Object cache with Redis

[Redis](/doc/addons/redis/) offers you a good way to speed-up your application by caching some of the objects of your
application, as the result of SQL queries of your application, improving the response time.

To enable [Redis](/doc/addons/redis/) for your WordPress, you need to disable other Object Cache and Data Cache of your
application (as those provided by *W3 Total Cache* for example). Make sure they aren't enabled to avoid conflicts and
performance problems.

1. [Create a Redis add-on](/doc/addons/clever-cloud-addons/) for your application.

2. Add the following lines to your `wp-config.php` file. Make sure they are **before** the
`require_once(ABSPATH . 'wp-settings.php');` line, otherwise the Redis connexion will not work for your application and
your application will return only white pages!
```php
define('WP_CACHE_KEY_SALT', 'tvm_');
define('WP_REDIS_CLIENT', 'pecl');
define('WP_REDIS_HOST', getenv('REDIS_HOST'));
define('WP_REDIS_PORT', getenv('REDIS_PORT'));
define('WP_REDIS_PASSWORD', getenv('REDIS_PASSWORD'));
```

3. Download [this file](http://plugins.svn.wordpress.org/redis-cache/trunk/includes/object-cache.php)
who manages the connexion with Redis and WordPress and moves it to your `/wp-content` folder. The file **must** be named
`object-cache.php`.

4. Redis should now work with your WordPress.

### SSL Configuration

Since your website is behind a reverse proxy managed by Clever Cloud, you need to detect specific headers like **X_FORWARDED_PROTO** or **HTTP_X_FORWARDED_PROTO** to enable SSL. To do so edit `wp-config.php` and add the following code above the last `require_once` call.
```php
if (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO'] == 'https') {
    $_SERVER['HTTPS'] = 'on';
} elseif (isset($_SERVER['X_FORWARDED_PROTO']) && $_SERVER['X_FORWARDED_PROTO'] == 'https') {
    $_SERVER['HTTPS'] = 'on';  
}
```

### Using a CDN as Cloudflare with SSL (avoid infinite loops)

As with ###SSL configuration, you need to detect detect specific headers like **X_FORWARDED_PROTO** or **HTTP_X_FORWARDED_PROTO** to enable SSL. In this case, the chained proxies might concatenate those headers. As a result, headers values can look like 
```php [HTTP_X_FORWARDED_PROTO] => https, https```
The previous code snippet would not enable SSL on the Clever Cloud application, resulting in mixed content or infinite loop.

In order to avoid this, you'll need to add the following code snippet to your `wp-config.php` file (it replaces the previous code snippet). Add the following code above the last `require_once` call or at the beginning of the `wp-config.php` file.

To do so edit `wp-config.php` and add the following code above the last `require_once` call.
```php
function check_proto_set_ssl($forwarded_protocols){
	$secure = 'off';
	if ( strstr($forwarded_protocols , ",") ) {
		$previous = null;
		foreach ( explode(",", $forwarded_protocols) as $value ) {
			if ( $previous ) {
				trim($value) == $previous && trim($value) == 'https' ? $secure = 'on' : $secure = 'off';
			}
			$previous = trim($value);
		}
		$_SERVER["HTTPS"] = $secure;
	}else{
		$forwarded_protocols == 'https' ? $_SERVER["HTTPS"] = 'on' : $_SERVER["HTTPS"] = $secure = 'off';
	}
}

if (isset($_SERVER['HTTP_X_FORWARDED_PROTO'])) {
	check_proto_set_ssl($_SERVER['HTTP_X_FORWARDED_PROTO']);
} elseif (isset($_SERVER['X_FORWARDED_PROTO'])) {
	check_proto_set_ssl($_SERVER['X_FORWARDED_PROTO']);
}
```

Special thanks to [jlannoy](https://github.com/jlannoy) for his work wich really helped on updating this section.
