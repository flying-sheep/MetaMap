#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

is_in_package <-
  function()
    ! identical(environment(is_in_package), .GlobalEnv)

if (!is_in_package()) {
  library(data.table)
  library(rlang)
  library(purrr)
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(stringr)
  library(phyloseq)
  # devtools::install_github('tidyverse/ggplot2@2b5b88dcc8ef73ec37459cd2b107b60693154884')
  library(ggplot2)
  library(shiny)
  library(htmltools)
  library(plotly)
  library(utils)
  library(shinyjs)
  library(shinythemes)
  library(DT)
  library(V8)
  library(grid)
  require(psadd)
  require(DESeq2)
}

pkg_file <- function(path = ".") {
  if (is_in_package())
    system.file(path, package = "MetaMap", mustWork = T)
  else
    file.path("../inst", path)
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
