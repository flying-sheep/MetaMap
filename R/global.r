#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#' @import data.table
#' @import rlang
#' @import purr
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import phyloseq
#' @import microbiome
#' @import ggplot2
#' @import shiny
#' @import htmltools
#' @import plotly
#' @import utils
#' @import shinyjs

DIR <- "data"
MAX_SAMPLES <- 150

load(file.path(DIR, 'study_info.RData'))
