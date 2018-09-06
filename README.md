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
* R/plots.R
* R/www

Package
=======

This app can also be built as a package.

Call `MetaMap::launch()`.

Bookmarks
=========
To save bookmarks, click on the bookmark button in the top right.

You can either copy the full URL or create a prettier URL mapping:

For the latter, create `inst/data/bookmarks.json` containing `{"Study1": "af689dc..."}`
with names of your choice and IDs taken from `_state_id=af689dc...` in the URL the button gave you.

Now you can link to http://your.metamap.instance/?example=Study1

Note
=======
The krona plot will not work on windows.
