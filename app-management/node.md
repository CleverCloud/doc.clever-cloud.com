Node JS
=======

To host a Node.JS application on Clever Cloud, you must include a package.json file at the root of your project. This file will contain the following fields:

* engine, node : The minimal version of Node.JS on which the application works.  
* main : The path to the entry point of the application.  
* dependencies: The dependencies used in this project. This point is not mandatory if the project use no external dependencies.  

Here you can find an example of a complete package.json file :

    {
      "name": "http-server",
      "preferGlobal": "true",
      "version": "0.3.0",
      "author": "Nodejitsu <support@nodejitsu.com>",
      "description": "a simple zero-configuration command-line http server",
      "contributors": [ 
        {
          "name": "Marak Squires",
          "email": "marak@nodejitsu.com"
        } 
      ],
      "bin": {
        "http-server": "./bin/http-server"
      },
      "scripts": {
        "start": "node ./bin/http-server",
        "test": "vows --spec --isolate"
      },
      "main": "./lib/http-server",
      "repository": {
        "type": "git",
        "url": "https://github.com/nodejitsu/http-server.git"
      },
      "keywords": [
        "cli",
        "http",
        "server"
      ],
      "dependencies" : {
        "colors"   :  "*",
        "flatiron" :  "0.1.x",
        "optimist" :  "0.2.x",
        "union"    :  "0.1.x",
        "ecstatic" :  "0.1.x"
      },
      "noAnalyze": true,
      "devDependencies": {
        "vows"    :  "0.5.x",
        "request" :  "2.1.x"
      },
      "bundleDependencies": [
      ],
      "license": "MIT",
      "engine": {
        "node": ">=0.4"
      }
    }


For more informations about the fields and the syntax of the package.json file, you can check out this [cheatsheet[(http://package.json.jit.su/).

We only support project in Node >=0.6

If you use a database, don't forget to change your configuration with the informations given in the admin panel.
