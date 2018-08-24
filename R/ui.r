#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# include global.r
navbar <- function() navbarPage(
  theme = shinytheme("flatly"),
  position = "fixed-top",
  fluid = TRUE,
  # inverse = TRUE,
  title = div(
    id = "title-section",
    actionButton("back_button", "", icon = icon("arrow-left", "fa-2x")),
    "MetaMap"
  ),
  id = 'dataset',
  header = fluidRow(column(11, htmlOutput('help')), column(
    1,
    uiOutput("reload_button")
  ),
  style = "margin-bottom:20px"),
  tabPanel(
    "Overview",
    htmlOutput("overviewText"),
    img(src = 'PipelineImage.png', align = "center")
  ),
  navbarMenu(
    "Query",
    tabPanel(
      qmetafeature.name,
      uiOutput("mfInput"),
      plotlyOutput("mfPlot"),
      HTML(
        '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
      ),
      htmlOutput("mfName"),
      DT::dataTableOutput("mfTable")
    ),
    tabPanel(qstudy.name,
             DT::dataTableOutput("mystudies"))
  ),
  tabPanel(
    uiOutput("study_title"),
    tableOutput("studyinfo"),
    HTML(
      '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
    ),
    checkboxGroupInput(
      "check_file",
      label = "Select files",
      choices = c("OTU Counts", "Sample info", "Feature info")
    ),
    downloadButton("download_samples", "Download", class = "btn-primary")
  ),
  navbarMenu(
    "Customize Data",
    tabPanel(
      ssamples.name,
      sidebarLayout(
        sidebarPanel(
          style = "margin-top:73px; position:fixed; width: inherit; overflow-y:auto; max-height:50%; z-index:1",
          h4("Subset samples"),
          radioButtons("ss_radio_button", "Action", choices = c("Keep", "Exclude")),
          uiOutput("ss_text"),
          actionButton(
            "ss_apply_button",
            "Apply",
            width = "100%",
            class = "btn-primary"
          ),
          HTML(
            '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
          ),
          h4("Define Groups"),
          radioButtons("groups_button", "Groups", choices = c("Group_0")),
          textInput("group_name", "Group Name", width = "100%"),
          actionButton(
            "groupName_button",
            "Change Name",
            width = "100%",
            class = "btn-primary"
          ),
          actionButton(
            "sel_button",
            "Select samples",
            width = "100%",
            class = "btn-primary",
            style = "margin-bottom: 5px; margin-top: 5px"
          ),
          actionButton(
            "addGroup_button",
            "Add Group",
            width = "100%",
            class = "btn-primary"
          ),
          width = 2
        ),
        mainPanel(
          DT::dataTableOutput("mysamples", width = "100%"),
          width = 10,
          style = "padding-left:30px"
        )
      ),
      style = "margin-bottom:100px;"
    ),
    tabPanel(smf.name,
             sidebarLayout(
               sidebarPanel(
                 style = "margin-top:73px; position: fixed; width: inherit;",
                 h4("Subset metafeatures"),
                 radioButtons("sm_radio_button", "Action", choices = c("Keep", "Exclude")),
                 uiOutput("sm_text"),
                 actionButton(
                   "sm_apply_button",
                   "Apply",
                   width = "100%",
                   class = "btn-primary"
                 ),
                 width = 2
               ),
               mainPanel(
                 DT::dataTableOutput("taxa_table", width = "100%"),
                 width = 10,
                 style = "padding-left:30px"
               )
             ),
             style = "margin-bottom:100px;")
  ),
  navbarMenu(
    "Analysis",
    tabPanel(
      dimred.name,
      fluidRow(class = "control-bar",
               column(4, offset = 4, uiOutput("attribute_dr"))),
      tags$div(style = "margin-bottom:100px;",
               plotlyOutput("dimred"))
    ),
    tabPanel(
      da.name,
      fluidRow(class = "control-bar",
               column(4, offset = 4,
                      uiOutput("attribute_da"))),
      tags$div(
        style = "margin-bottom:100px;",
        uiOutput('diversity_stats'),
        plotlyOutput("diversity")
      )
    ),
    tabPanel(
      de.name,
      fluidRow(
        class = "control-bar",
        column(2, offset = 2, uiOutput('select_species_diff')),
        column(2, uiOutput("attribute_de")),
        column(2, uiOutput('de_conds')),
        column(2, uiOutput('de_button', width = "100%"), style = "padding-top:19px")
      ),
      plotlyOutput("de_boxplot"),
      tags$div(
        style = "margin-bottom:100px;",
        condition = "output.cond" ,
        tabPanel("Global",
                 DT::dataTableOutput("deseq_table")),
        plotlyOutput("de_plot", height = "600px")
      )
    ),
    tabPanel(
      mc.name,
      fluidRow(
        class = "control-bar",
        column(2, offset = 3, uiOutput("mf_mc")),
        column(2, uiOutput(("level_mc"))),
        column(
          2,
          actionButton("mc_apply_button", "Analyze",  class = "btn-primary"),
          style = "padding-top:19px"
        )
      ),
      DT::dataTableOutput("cor_table"),
      plotOutput("cor_plot")
    ),
    tabPanel(
      ma.name,
      fluidRow(
        class = "control-bar",
        column(2, offset = 3, uiOutput("level_ma")),
        column(2, uiOutput("attribute_ma")),
        column(
          2,
          selectInput(
            "top_n_ma",
            label = "Top N",
            choices = 1:50,
            selected = 10
          )
        )
      ),
      plotlyOutput("top_species_plot")
      # ,uiOutput('select_species_abundance'),
      # plotOutput("abundances_plot")
    ),
    #attribute, level, relative = TRUE
    tabPanel(
      tbc.name,
      fluidRow(
        class = "control-bar",
        column(4, offset = 1, uiOutput("attribute_tbc")),
        column(4, uiOutput("level_tbc")),
        column(2, uiOutput('tbc_button'), style = "padding-top:19px")
      ),
      conditionalPanel(condition = "output.cond1",
                       tabsetPanel(
                         id = "tbc_panel",
                         # ntaxa_plot is normalized
                         tabPanel("Relative proportion", plotlyOutput("ntaxa_plot")),
                         tabPanel("Absolute counts", plotlyOutput("taxa_plot"))
                       ))
    ),
    tabPanel(
      sankey.name,
      div(
        class = "control-bar",
        fluidRow(
          column(1, offset = 2, selectInput('sankey_source', 'Source', c())),
          column(1, selectInput('sankey_target', 'Target', c())),
          column(2, uiOutput('attribute_sankey')),
          column(2, uiOutput('sankey_condition')),
          column(
            1,
            style = "padding-top:19px",
            actionButton(
              "sankey_apply_button",
              label = "Apply",
              class =
                "btn-primary",
              width = "100%"
            )
          ),
          column(
            1,
            style = "padding-top:19px",
            actionButton(
              "sankey_reset_button",
              label = "Reset",
              class = "btn-primary",
              width = "100%"
            )
          ),
          column(
            1,
            style = "padding-top:19px",
            conditionalPanel(
              condition = "output.sankey_cond",
              actionButton(
                "sankey_undo_button",
                HTML("<b>Undo</b>")
                ,
                class = "btn-primary",
                width = "100%"
              )
            )
          )
        )
      ),
      fluidRow(id = "sankey-div",
               column(12, align = "center", uiOutput("sankey.ui"))),
      style = "margin-bottom:100px;"
    ),
    tabPanel(
      krona.name,
      fluidRow(
        class = "control-bar",
        column(3, offset = 4, uiOutput('attribute_krona')),
        column(
          1,
          actionButton("krona_apply_button", label = "Plot", class = "btn-primary"),
          style = "padding-top:19px"
        )
      ),
      fluidRow(column(
        12,
        uiOutput("krona_iframe", style = "margin-bottom:1500px")
      ))
    )
  )
)

# Add github link to the navbar
add_gh <- function(nav) {
  nav[[3]][[1]]$children[[1]] %<>% htmltools::tagAppendChild(
    HTML("<a id='github-btn'href='https://github.com/gtsitsiridis/MetaMap' target='_blank'><i class='fa fa-github'></i></a>"))
  nav
}


page <- function(gh) fluidPage(
  titlePanel(title = "", windowTitle = "MetaMap"),
  if (gh) add_gh(navbar()) else navbar()
)

#' @export
ui <- function(request) {
  addResourcePath("www", pkg_file("shiny/www"))
  tagList(
    page(gh = FALSE),
    tags$link(rel = "stylesheet", href = "www/style.css"),
    # add contextmenu on plots
    tags$nav(tags$ul(
      tags$li(
        downloadLink(
          "ggplot_link",
          label = HTML(as.character(tags$i(class = "fa fa-download")), "Download ggplot"),
          class = "context-menu__link",
          `plot-action` = "download ggplot"
        ),
        class = "context-menu__item"
      ),
      class = "context-menu__items"
    ),
    class = "context-menu"),
    useShinyjs(),
    extendShinyjs(script = "www/contextmenu.js"),
    extendShinyjs(script = "www/general.js")
  )
}
