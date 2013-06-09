


PREREQUISITES
=============

1. Haskell Platform [http://www.haskell.org/platform/](http://www.haskell.org/platform/).
3. Hakyll version 4.2.2.0
2. Less compiler [http://lesscss.org/](http://lesscss.org/)

WRITING A PAGE
==============
Create a new .md file in on of these folder:
* commons (each sub-folder is a topic and contain articles)
* runtimes (contains every language supported)


BUILD FOR PRODUCTION
====================

1. Checkout production branch.
4. Run `make preview` for running a local preview


CONTRIBUTING
============

CONTENT
-------
This documentation is open for content contribution.
Any helpful pull-request will be merged. For instance:
1. Text of schematic content improvements
2. Grammar or orthograph improvements
3. Any revelant pieces of infos from our mail support


CSS - HTML
----------
Every css class used for design purposes must follow this naming convention:
1. Use the `cc_` or `cc-` prefix
2. `__`  is used to define a component part,  `--` is a variant
For instance, this is a style that is specific to the close button of the info alert box only:
  `cc_alert--info__close`

Authors
=======
* [@crmfrsh](http://twitter.com/crmfrsh)
* [@waxzce](http://twitter.com/waxzce)
* [@R_Foucault‎](http://twitter.com/R_Foucault‎)
* [@cnivolle](http://twitter.com/cnivolle)