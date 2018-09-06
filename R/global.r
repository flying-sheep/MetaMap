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
