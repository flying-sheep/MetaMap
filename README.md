Getting started
===============
You can start by running the following.

```r
shiny::runApp('R')
```
If you are using a custom directory, instead of "R", make sure to include the following files and directories:
* R/global.R
* R/ui.R
* R/server.R
* R/methods.R
* R/www

Package
=======

This app can also be built as a package.

Call `MetaMap::launch()`.
