#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

is_in_package <- function() !identical(environment(is_in_package), .GlobalEnv)

if (!is_in_package()) {
  library(data.table)
  library(rlang)
  library(purrr)
  library(tidyr)
  library(plyr)
  library(magrittr)
  library(dplyr)
  library(stringr)
  library(phyloseq)
  if (packageVersion("ggplot2") < "3.0.0")
    stop("Old ggplot2 version! Please install package:ggplot2 3.0.0!")
  if (packageVersion("plotly") < "4.7.1.9000") stop(
    "Please install the development version of plotly!\n",
    "Last working version: 4.7.1.9000.\n",
    "Run devtools::install_github('ropensci/plotly', force = TRUE)"
  )
  library(ggplot2)
  library(shiny)
  library(htmltools)
  library(plotly)
  library(utils)
  library(shinyjs)
  library(shinythemes)
  library(DT)
  library(grid)
  require(psadd)
  require(DESeq2)
}

pkg_file <- function(path = ".", validate = TRUE) {
  resolved <-
    if (is_in_package())
      system.file(path, package = "MetaMap", mustWork = TRUE)
    else
      file.path("../inst", path)
  if (validate && !file.exists(resolved))
    stop('File ', resolved, ' does not exist. Full path: ', normalizePath(resolved))
  resolved
}

qstudy.name <- "Query by study"
qmetafeature.name <- "Query by metafeature"
ssamples.name <- "Subset samples"
smf.name <- "Subset metafeatures"
dimred.name <- "Dimension reduction"
da.name <- "Diversity analysis"
de.name <- "Differential expression"
mc.name <- "Metafeature correlation"
ma.name <- "Metafeature abundance"
tbc.name <- "Taxonomy bar chart"
sankey.name <- "Sankey diagram"
krona.name <- "Krona chart"

enableBookmarking('server')

# structure of the bookmark file: {"Name": "382e2cdc6e855a84"}
bookmark_file <- pkg_file("data/bookmarks.json")
bookmarks <- if (file.exists(bookmark_file)) {
  jsonlite::read_json(bookmark_file)
} else {
  message('Info: Bookmark file ', bookmark_file, ' not found.')
  NULL
}

bookmark_redirect <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  path <- URLdecode(req$PATH_INFO)

  if (is.null(path))
    return(shiny:::httpResponse(400, content = "<h1>Bad Request</h1>"))
  if (!grepl('.*/bookmark/\\w+', path))
    return(NULL)

  bookmark <- sub('.*/bookmark/(\\w+)', '\\1', path)
  state_id <- bookmarks[[bookmark]]

  bm_url <- function(sid) paste0('../../?_state_id_=', sid)

  if (is.null(state_id)) {
    bm_html <- sprintf("<li><a href='%s'>%s</a>", bm_url(bookmarks), names(bookmarks))
    return(shiny:::httpResponse(404, content = sprintf(
      "<h1>404 Bookmark not found</h1>
      No bookmark with name %s<br>
      Available bookmarks: <ul>%s</ul>",
      bookmark, paste(bm_html, collapse = '\n')
    )))
  }

  shiny:::httpResponse(302, headers = list(Location = bm_url(state_id)))
}

shiny:::handlerManager$addHandler(bookmark_redirect, '/bookmark/')
