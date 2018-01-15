#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# include global.r

resetClickCode <-
  "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-sankey', 'null'); }"

#' @export
ui <- function() {
  navbarPage(
    fluid = T,
    inverse = TRUE,
    title = 'MetaMap',
    id = 'dataset',
    tabPanel(
      "Overview",
      htmlOutput("overviewText"),
      img(src='www/PipelineImage.png', align = "center")
    ),
    tabPanel(
      "Query studies",
      htmlOutput("queryHelp") ,
      DT::dataTableOutput("mystudies")
    ),
    tabPanel(
      'Sample Selection',
      htmlOutput("sampleHelp"),
      sidebarLayout(
        sidebarPanel(
          # div(style = "height:200px;", h2("Groups")),
          radioButtons("groups_button", "Groups", choices = c("Group_0")),
          textInput("group_name", "Group Name"),
          actionButton("groupName_button", "Change Name"),
          actionButton("sel_button", "Select samples"),
          actionButton("addGroup_button", "Add Group"),
          HTML(
            '<hr style="height:1px;border:none;color:#333;background-color:#333;"/>'
          ),
          checkboxGroupInput(
            "check_file",
            label = "Select files",
            choices = c("OTU Counts", "Sample info", "Feature info")
          ),
          downloadButton("download_samples", "Download"),
          width = 2
        ),
        mainPanel(DT::dataTableOutput("mysamples"), width = 10)
      )
    ),
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
          tabsetPanel(
            id = "de_panel",
            tabPanel(
              "Global",
              DT::dataTableOutput("deseq_table"),
              plotlyOutput("de_plot", height = "600px")
            ),
            tabPanel("Local", DT::dataTableOutput("deseq_table_subset"))
          )
        )
      )
    ),
    tabPanel(
      "Metafeature Abundance",
      htmlOutput("maHelp"),
      uiOutput("attribute_ma"),
      plotOutput("top_species_plot")
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
      conditionalPanel(
        condition = "output.cond1",
        tabsetPanel(
          id = "tbc_panel",
          # ntaxa_plot is normalized
          tabPanel("Relative proportion", plotOutput("ntaxa_plot")),
	  tabPanel("Absolute counts", plotOutput("taxa_plot"))
        )
      )
    ),
    tabPanel("Sankey Diagram", sidebarLayout(
      sidebarPanel(
        selectInput('sankey_source', 'Source', c()),
        selectInput('sankey_target', 'Target', c()),
        fluidRow(column(4, uiOutput(
          'attribute_sankey'
        )),
        column(4, uiOutput('sankey_condition'))),
        fluidRow(
          column(4, actionButton("sankey_apply_button", label = "Apply")),
          column(4,  actionButton("sankey_reset_button", label = "Reset")),
          column(
            4,
            conditionalPanel(condition = "output.sankey_cond", actionButton("sankey_undo_button", "Undo"))
          )
        )
      ),
      mainPanel(plotlyOutput("sankey_plot"))
    )),
    # as long as there are still some bugs
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    useShinyjs(),
    extendShinyjs(text = resetClickCode)
  )
}
