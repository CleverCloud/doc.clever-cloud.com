---
layout: page

id: php
parent: app_configuration
prev: java_and_scala
next: ruby
---
#PHP

For now, only the deployment via FTP is available. 
Git deployment for PHP will be available soon.

## Deployment via FTP 
**Important** : Git is not recomended for PHP applications. Configuration files could be created by some frameworks while they are in production, so Git could not allow you to retrieve them.

To deploy via a FTP client, see details here: <a href="/app-deployment/ftp-deploy.html">FTP Deploy</a>.  

## PHP version and modules

{% highlight bash%}
PHP Version => 5.3.16

System => Linux sn-fr110-0116 3.6.0-rc4 #8 SMP Wed Sep 19 02:48:27 CEST 2012 x86_64
Build Date => Aug 30 2012 12:03:43
Configure Command =>  './configure'  '--prefix=/usr' '--host=x86_64-pc-linux-gnu' '--build=x86_64-pc-linux-gnu' '--mandir=/usr/share/man' '--infodir=/usr/share/info' '--datadir=/usr/share' '--sysconfdir=/etc' '--localstatedir=/var/lib' '--disable-dependency-tracking' '--disable-silent-rules' '--enable-fast-install' '--libdir=/usr/lib64' '--libdir=/usr/share/php' '--localstatedir=/var' '--enable-bcmath' '--enable-calendar' '--enable-cli' '--enable-ctype' '--enable-dba' '--enable-flatfile' '--enable-hash' '--enable-inifile' '--enable-intl' '--enable-json' '--enable-magic-quotes' '--enable-mbstring' '--enable-mbregex' '--enable-mbregex-backtrack' '--enable-pcntl' '--enable-phar' '--enable-posix' '--enable-session' '--enable-shmop' '--enable-sockets' '--enable-sysvmsg' '--enable-sysvsem' '--enable-sysvshm' '--enable-tokenizer' '--enable-zend-multibyte' '--enable-zip' '--disable-embed' '--disable-embedded-mysqli' '--disable-safe-mode' '--with-bz2' '--with-config-file-path=/etc/php' '--with-config-file-scan-dir=/etc/php' '--with-exec-dir=/usr/bin' '--with-gettext' '--with-gmp' '--with-iconv' '--with-icu-dir=/usr' '--with-layout=GNU' '--with-zlib' '--with-zlib-dir=/usr' '--without-adabas' '--without-aolserver' '--without-apache-hooks' '--without-apache-hooks-static' '--without-apxs' '--without-birdstep' '--without-caudium' '--without-cdb' '--without-continuity' '--without-custom-odbc' '--without-dbmaker' '--without-empress' '--without-empress-bcs' '--without-enchant' '--without-esoob' '--without-mhash' '--without-ibm-db2' '--without-interbase' '--without-iodbc' '--without-isapi' '--without-milter' '--without-mm' '--without-ndbm' '--without-nsapi' '--without-ODBCRouter' '--without-oci8' '--without-pdo-firebird' '--without-pdo-oci' '--without-phttpd' '--without-pi3web' '--without-qdbm' '--without-roxen' '--without-sapdb' '--without-snmp' '--without-solid' '--without-sybase-ct' '--without-thttpd' '--without-tux' '--without-webjames' '--disable-debug' '--enable-fpm' '--enable-ipv6' '--disable-exif' '--enable-fileinfo' '--enable-filter' '--disable-ftp' '--enable-gd-native-ttf' '--enable-sqlite-utf8' '--enable-dom' '--enable-libxml' '--enable-simplexml' '--enable-soap' '--enable-wddx' '--enable-xml' '--enable-xmlreader' '--enable-xmlwriter' '--without-apxs2' '--with-pear' '--with-curl' '--without-curlwrappers' '--without-db4' '--with-gd=/usr' '--with-jpeg-dir=/usr' '--with-freetype-dir=/usr' '--with-png-dir=/usr' '--with-t1lib=/usr' '--with-xpm-dir=/usr' '--without-imap' '--without-gdbm' '--without-kerberos' '--without-ldap' '--without-ldap-sasl' '--without-libedit' '--with-mcrypt' '--without-mssql' '--with-mysql=mysqlnd' '--with-mysql-sock=/run/mysqld/mysqld.sock' '--with-mysqli=mysqlnd' '--with-mysql-sock=/run/mysqld/mysqld.sock' '--with-pcre-dir=/usr' '--with-pcre-regex=/usr' '--with-pdo-mysql=mysqlnd' '--with-mysql-sock=/run/mysqld/mysqld.sock' '--without-pdo-dblib' '--without-pdo-odbc' '--with-pdo-pgsql=/usr' '--with-pdo-sqlite=/usr' '--with-sqlite3=/usr' '--with-pgsql=/usr' '--without-pspell' '--without-readline' '--without-recode' '--with-imap-ssl' '--with-openssl' '--without-tidy' '--without-unixODBC' '--with-xmlrpc' '--with-xsl'

Server API => Command Line Interface
Virtual Directory Support => disabled
Configuration File (php.ini) Path => /etc/php
Loaded Configuration File => /etc/php/php.ini
Scan this dir for additional .ini files => /etc/php
Additional .ini files parsed => /etc/php/._cfg0000_php.ini,
/etc/php/php.ini

PHP API => 20090626
PHP Extension => 20090626
Zend Extension => 220090626
Zend Extension Build => API220090626,NTS
PHP Extension Build => API20090626,NTS
Debug Build => no
Thread Safety => disabled
Zend Memory Manager => enabled
Zend Multibyte Support => enabled
IPv6 Support => enabled

Registered PHP Streams => https, ftps, compress.zlib, compress.bzip2, php, file, glob, data, http, ftp, phar, zip  
Registered Stream Socket Transports => tcp, udp, unix, udg, ssl, sslv3, sslv2, tls
Registered Stream Filters => zlib.*, bzip2.*, convert.iconv.*, mcrypt.*, mdecrypt.*, string.rot13, string.toupper, string.tolower, string.strip_tags, convert.*, consumed, dechunk
{% endhighlight %}
