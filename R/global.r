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
  library(ggplot2)
  library(shiny)
  library(htmltools)
  library(plotly)
  library(utils)
  library(shinyjs)
}

pkg_file <- function(path = ".") {
  if (is_in_package())
    system.file(path, package = "MetaMap", mustWork = T)
  else
    file.path("../inst", path)
}

initData <- function() {
    DIR <<- pkg_file("data")
    MAX_SAMPLES <<- 250

    STUDIES <<- list.files(file.path(DIR, 'studies')) %>%
      str_split_fixed("\\.", n = 2) %>% .[, 1]

    # print(file.path(DIR, 'studies'))
    # print(list.files(file.path(DIR, 'studies')) %>%
    #         str_split_fixed("\\.", n = 2) %>% .[, 1])
    # print(STUDIES)

    load(file.path(DIR, 'study_info.RData'))

    # only show studies that exist in the data/studies directory
    study_info <<- subset(study_info, study %in% STUDIES)

    # add links redirecting to the sra website for each study
    study_info$link <<-
      with(
        study_info,
        paste0(
          "<a href='https://trace.ncbi.nlm.nih.gov/Traces/sra/?study=",
          study,
          "'>",
          study,
          "</a>"
        )
      )
}
