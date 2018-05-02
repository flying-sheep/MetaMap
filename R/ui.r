#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# include global.r

page <- fluidPage(
  titlePanel(title = "", windowTitle = "MetaMap"),
  navbarPage(
    theme = shinytheme("flatly"),
    position = "fixed-top",
    fluid = T,
    # inverse = TRUE,
    title = div(actionButton(
      "back_button", "", icon = icon("arrow-left", "fa-2x")
    ), "MetaMap"),
    id = 'dataset',
    tabPanel(
      "Overview",
      htmlOutput("overviewText"),
      img(src = 'PipelineImage.png', align = "center")
    ),
    navbarMenu(
      "Query",
      tabPanel(
        "Query by metafeature",
        htmlOutput("mfHelp"),
        uiOutput("mfInput"),
        plotlyOutput("mfPlot"),
        HTML(
          '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
        ),
        htmlOutput("mfName"),
        DT::dataTableOutput("mfTable")
      ),
      tabPanel(
        "Query by study",
        htmlOutput("queryHelp") ,
        DT::dataTableOutput("mystudies")
      )
    ),
    tabPanel(uiOutput("study_title"),
             tableOutput("studyinfo")),
    tabPanel(
      'Define sample grouping',
      htmlOutput("sampleHelp"),
      sidebarLayout(
        sidebarPanel(
          # style = "position:fixed;width:inherit;",
          radioButtons("groups_button", "Groups", choices = c("Group_0")),
          textInput("group_name", "Group Name", width = "100%"),
          actionButton(
            "groupName_button",
            "Change Name",
            width = "100%",
            class = "btn-primary"
          ),
          HTML('<hr style="height:0px;border:none;"/>'),
          actionButton(
            "sel_button",
            "Select samples",
            width = "100%",
            class = "btn-primary"
          ),
          HTML('<hr style="height:0px;border:none;"/>'),
          actionButton(
            "addGroup_button",
            "Add Group",
            width = "100%",
            class = "btn-primary"
          ),
          HTML(
            '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
          ),
          checkboxGroupInput(
            "check_file",
            label = "Select files",
            choices = c("OTU Counts", "Sample info", "Feature info")
          ),
          downloadButton("download_samples", "Download", class = "btn-primary"),
          tags$style(HTML("
                          #download_samples {
                          width: 100%;
                          }
                          ")),
          width = 2
          ),
        mainPanel(DT::dataTableOutput("mysamples", width = "100%"), width = 10)
          )
          ),
    navbarMenu(
      "Analysis",
      tabPanel(
        "Dimension reduction",
        htmlOutput("dmHelp"),
        uiOutput("attribute_dr"),
        tags$div(style = "margin-bottom:100px;",
                 plotlyOutput("dimred"))
      ),
      tabPanel(
        "Diversity analysis",
        htmlOutput("daHelp"),
        uiOutput("attribute_da"),
        tags$div(
          style = "margin-bottom:100px;",
          uiOutput('diversity_stats'),
          plotlyOutput("diversity")
        )
      ),
      tabPanel(
        "Differential expression",
        htmlOutput("deHelp"),
        fluidRow(column(4, uiOutput(
          'select_species_diff'
        )), column(4, uiOutput("attribute_de"))),
        plotlyOutput("de_boxplot"),
        fluidRow(
          column(4, uiOutput('select_cond1')),
          column(4, uiOutput('select_cond2')),
          column(4, offset = 3, uiOutput('de_button'))
        ),
        tags$div(
          style = "margin-bottom:100px;",
          conditionalPanel(
            condition = "output.cond" ,
            tabsetPanel(id = "de_panel",
                        tabPanel(
                          "Global",
                          DT::dataTableOutput("deseq_table")
                        )),
            plotlyOutput("de_plot", height = "600px")
          )
        )
      ),
      tabPanel(
        "Metafeature Abundance",
        htmlOutput("maHelp"),
        uiOutput("attribute_ma"),
        plotlyOutput("top_species_plot")
        # ,uiOutput('select_species_abundance'),
        # plotOutput("abundances_plot")
      ),
      #attribute, level, relative = T
      tabPanel(
        "Taxonomy Bar Chart",
        htmlOutput("tbcHelp"),
        fluidRow(
          column(4, uiOutput("attribute_tbc")),
          column(4, uiOutput("level_tbc")),
          column(4, offset = 3, uiOutput('tbc_button'))
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
        "Sankey Diagram",
        htmlOutput("sankeyHelp"),
        div(id = "sankey-div", uiOutput("sankey.ui")),
        HTML(
          '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
        ),
        fluidRow(column(
          4, selectInput('sankey_source', 'Source', c()), offset = 2
        ),
        column(
          4, selectInput('sankey_target', 'Target', c())
        )),
        fluidRow(column(4, uiOutput('attribute_sankey'), offset = 2),
                 column(4, uiOutput('sankey_condition'))),
        fluidRow(
          column(
            4,
            actionButton("sankey_apply_button", label = "Apply", class =
                           "btn-primary"),
            offset = 2
          ),
          column(
            1,
            actionButton("sankey_reset_button", label = "Reset", class = "btn-primary")
          ),
          column(
            2,
            conditionalPanel(
              condition = "output.sankey_cond",
              actionButton("sankey_undo_button", HTML("<b>Undo</b>")
                           , class = "btn-primary")
            )
          ),
          style = "margin-bottom:100px;"
        )
      ),
        tabPanel(
        "Krona plot",
	htmlOutput("kronaHelp"),
	fluidRow(column(2, uiOutput('attribute_krona')), column(2, actionButton("krona_apply_button", label="Plot", class="btn-primary"))),
	fluidRow(column(12,
	uiOutput("krona_iframe", style="margin-bottom:1500px")
       )))
      )
    )
    )

#' @export
ui <- function() {
  # addResourcePath("www", pkg_file("shiny/www"))
  tagList(
    page,
    includeCSS("www/style.css"),
    # add contextmenu on plots
    tags$nav(tags$ul(
      tags$li(
        downloadLink("ggplot_link",
                     label=HTML(as.character(tags$i(class="fa fa-download")),"Download ggplot"),
                     class="context-menu__link", `plot-action`="download ggplot"),
        class = "context-menu__item"),
      class = "context-menu__items"),
    class = "context-menu"),

    #downloadLink("downloadHelper", ""),
    useShinyjs(),
    extendShinyjs(script = "www/contextmenu.js"),
    extendShinyjs(script = "www/general.js")
    )
}
