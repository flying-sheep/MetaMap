# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!is_in_package()) {
  source("methods.r")
  source("plots.r")
}

empty <- "none"


#' @export
server <-
  function(input,
           output,
           session,
           DIR = pkg_file("data"),
           MAX_SAMPLES = 1000,
           DESEQ_PARALLEL = FALSE) {
    ####################
    ### Prepare data ###
    ####################

    STUDIES <- list.files(file.path(DIR, 'studies')) %>%
      str_split_fixed("\\.", n = 2) %>% .[, 1]

    load(file.path(DIR, 'study_info.RData'))

    # only show studies that exist in the data/studies directory
    study_info <- subset(study_info, study %in% STUDIES)

    # add links redirecting to the sra website for each study
    study_info$link <-
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

    #########################
    ###  Reactive values  ###
    #########################
    tabs <- reactiveValues(last = NULL,
                           current = NULL)

    values <-
      reactiveValues(
        study = NULL,
        phylo = NULL,
        selection = NULL,
        de_table = NULL,
        attributes = NULL,
        mf_tbl = NULL,
        mf_selected = NULL,
        mf_studies = NULL,
        species_diff = NULL,
        cor_table = NULL,
        mf_mc = NULL
      )

    plots <- reactiveValues(
      mfPlot = NULL,
      dimred = NULL,
      diversity = NULL,
      de_boxplot = NULL,
      de_plot = NULL,
      top_species_plot = NULL,
      taxa_plot = NULL,
      ntaxa_plot = NULL,
      cor_plot = NULL,
      download = NULL
    )

    sankey <-
      reactiveValues(
        sankey_links = NULL,
        args = list(),
        newArgs = list(
          source = "Kingdom",
          target = "Phylum",
          level_filter = NULL,
          source_filter = NULL
        ),
        args_history = list(),
        undo = FALSE,
        cond = empty,
        attribute = empty
      )

    ##########################
    ###  Helper functions  ###
    ##########################

    ### Reset values
    resetPlots <- function() {
      plots$cor_plot <- NULL
      plots$mfPlot <- NULL
      plots$dimred <- NULL
      plots$diversity <- NULL
      plots$de_plot <- NULL
      plots$de_boxplot <- NULL
      plots$taxa_plot <- NULL
      plots$ntaxa_plot <- NULL
      plots$top_species_plot <- NULL
      sankey$attribute <- empty
      sankey$cond <- empty
      js$writeKrona("")
      output$taxa_plot <- renderPlotly({
        NULL
      })
      output$ntaxa_plot <- renderPlotly({
        NULL
      })
      output$cond1 <- reactive({
        FALSE
      })
    }

    resetValues <- function() {
      values$phylo <- NULL
      values$selection <- NULL
      values$de_table <- NULL
      values$attributes <- NULL
      values$species_diff <- NULL
      values$cor_table <- NULL
      values$mf_mc <- NULL
    }

    resetWidgets <- function() {
      updateSelectInput(session, 'attribute_dr', 'Color by', "")
      updateSelectInput(session, 'attribute_da', 'Color by', "")
      updateSelectInput(session, 'attribute_ma', 'Color by', "")
      updateRadioButtons(session, "groups_button", "Groups", choices = c("Group_0"))
      updateSelectInput(
        session,
        "top_n_ma",
        label = "Top N",
        choices = 1:50,
        selected = 10
      )
    }

    ###################
    ###  Html Text  ###
    ###################

    output$overviewText <-
      renderUI(
        HTML(
          '<h1 style="color: #5e9ca0;"><span style="color: #000000;">MetaMap - exploring the unexplored</span></h1>
          <p><span style="color: #000000;">This interactive web tool facilitates exploration of the MetaMap resource (<a href="https://www.biorxiv.org/content/early/2018/02/22/269092">Simon et al. bioRxiv</a>). In this large scale analysis raw human RNA-seq data from over 400 studies relevant to human disease were screened for microbial and viral reads. The data were generated using a two-step alignment pipeline outlined below:</span></p>
          <h2 style="color: #2e6c80;">&nbsp;</h2>
          <p>&nbsp;</p>'
        )
        )

    help <- list()

    help[[qstudy.name]] <-
      HTML(
        '<p style="text-align: center"><strong>Query studies by using the search box at the top right. Click on the row to select the study.</strong></p>'
      )
    help[[ssamples.name]] <-
      HTML(
        '<p style="text-align: center"><strong>Subset Samples: To keep or remove groups from the analysis select <em>Keep</em> or <em>Exclude</em> respectively. Next use the textfield to select the group and press <em>Apply</em> to make the changes. You should select the group using the following format: <em>Attribute</em> / <em>Group</em>.</strong></p>
        <p style="text-align: center"><strong>Define groups: To group the samples first click on <em>Add group</em>. Next select samples by clicking on the rows in the table. To assign selected samples to the group click on <em>Select samples</em>. To change the group name use the textfield and click on <em>Change Name</em>.</strong></p>'
      )
    help[[smf.name]] <-
      HTML(
        '<p style="text-align: center"><strong>To keep or remove metafeatures from the analysis select <em>Keep</em> or <em>Exclude</em> respectively. Next use the textfield to select the metafeature and press <em>Apply</em> to make the changes. You should select the metafeature using the following format: <em>Classification level</em> / <em>Metafeature</em>.</strong></p>'
      )

    help[[dimred.name]] <-
      HTML(
        '<p style="text-align: center"><strong>Multi-dimensional scaling plot visualized metafeatures counts of samples in reduced dimensions. To change the coloring select sample attribute from <em>Color by</em> drop-down menu.</strong></p>'
      )
    help[[da.name]] <-
      HTML(
        '<p style="text-align: center"><strong>To change the coloring select sample attribute from <em>Color by</em> drop-down menu. The displayed p-value is calculated from an analysis of variance between the diversity values and the selected sample attribute.</strong></p>'
      )
    help[[de.name]] <-
      HTML(
        '<p style="text-align: center"><strong>To plot metafeature expression select a metafeature using the <em>Species</em> textfield. To group samples by attibute select attribute from <em>Select Attribute</em> drop-down menu. To statistically evaluate differential expression of all metafeatures select the conditions you wish to compare by using the <em>Conditions</em> textfield. To run differential expression analysis click on the <em>Analyze</em> button. Please note this may take a couple of minutes depending on the data set size. You can also select points on the resulting volcano-plot to plot the expression of the respective metafeature.</strong></p>'
      )

    help[[mc.name]] <-
      HTML(
        '<p style="text-align: center"><strong>To statistically test the correlation between a given metafeature and all other metafeatures at a specific classification level, use the <em>Select metafeature</em> textfield and the <em>Select Classification level</em> drop-down menu respectively. A correlation table will be given as output. By selecting a metafeature from the table, you can view a scatter plot of the relative abundance of the given metafeatures in each sample.</strong></p>'
      )

    help[[ma.name]] <-
      HTML(
        '<p style="text-align: center"><strong>The barplot shows the mean relative abundance levels of the top 10 metafeatures. To change the amount of metafeatures to show select a number from the <em>Top N</em> drop-down menu. To change the grouping select sample attribute from <em>Color by</em> drop-down menu. To change the classification level of the metafeatures use the <em>Select Classification Level</em> drop-down menu.</strong></p>'
      )
    help[[tbc.name]] <-
      HTML(
        '<p style="text-align: center"><strong>To plot the Taxonomy Bar Chart click on <em>Generate</em>. To group samples together select sample attribute from <em>Select Grouping</em> drop-down menu. To change the coloring select a classification level from <em>Select Classification Level</em> drop-down menu. You can toggle between displaying absolute sequence abundances (by clicking the <em>Value</em> button) or normalized/proportional abundances (by clicking the <em>%</em> button).</strong></p>'
      )
    help[[qmetafeature.name]] <-
      HTML(
        '<p style="text-align: center"><strong>The scatter plot shows frequency of detection and maximal metafeature abundance across all studies on X and Y axes, respectively. The user can select a metafeature by 1) searching for the species name in the text field or 2) clicking on a data point in the plot. After selection of metafeature studies detecting the selected metafeature are listed below the plot. By clicking on the row in the list study is selected for further analysis.</strong></p>'
      )
    help[[sankey.name]] <-
      HTML(
        '<p style="text-align: center"><strong>Sankey diagram shows the average metafeature abundance across samples or selected grouping. The user can "walk" through the Sankey tree by 1) clicking on the graph or 2) selecting <em>Source</em> and <em>Target</em> phylogenetic levels and clicking on <em>Apply</em>.</strong></p>'
      )

    output$help <- renderUI({
      tab <- input$dataset
      help[[tab]]
    })

    output$study_title <- renderText({
      values$study
    })

    ###############
    ###  Query  ###
    ###############

    ### Query by study
    observeEvent(input$dataset, {
      if (all(!(
        input$dataset %in% c("Overview", "Query by metafeature", "Query by study")
      ), is.null(values$study))) {
        showModal(
          modalDialog(
            title = "Important message",
            'Please choose a study in the "Query studies" tab!',
            easyClose = TRUE
          )
        )
      }
    })

    showColumns <- c('link', 'study_abstract', "sample_size")

    mystudiesProxy <-  DT::dataTableProxy("mystudies")

    output$mystudies <- DT::renderDataTable({
      selected <- NULL
      if (!is.null(isolate(values$study)))
        selected <- which(study_info$study == isolate(values$study))
      DT::datatable(
        study_info[, showColumns],
        options = list(
          autoWidth = TRUE,
          pageLength = 5,
          scrollX = TRUE,
          searchHighlight = TRUE
        ),
        rownames = FALSE,
        selection = list(mode = 'single', selected = selected),
        escape = FALSE
      )
    })

    observeEvent(input$mystudies_rows_selected, {
      row <- input$mystudies_rows_selected
      values$study <- study_info$study[row]
    })

    ### Query by metafeature
    output$mfInput <- renderUI({
      if (is.null(values$mf_tbl)) {
        if (file.exists(file.path(DIR, 'metafeatures_table.RData'))) {
          load(file.path(DIR, 'metafeatures_table.RData'))
          mf_tbl <- mf_tbl[, STUDIES]
          mf_tbl <-
            mf_tbl[which(apply(mf_tbl, 1, function(x)
              ! all(is.na(x)))), ]
          values$mf_tbl <- mf_tbl
        } else
          withProgress(session = session, value = 0.5, {
            setProgress(message = 'Calculation in progress')
            values$mf_tbl <- mfMeans(study_info, STUDIES, DIR)
          })
      }
      selectInput('mfInput',
                  '',
                  c(Metafeature =  "", setNames(
                    rownames(values$mf_tbl), rownames(values$mf_tbl)
                  )))
    })

    observeEvent(input$mfInput, {
      mf_tbl <- isolate(values$mf_tbl)
      selected <- input$mfInput
      if (any(is.null(mf_tbl), is.null(selected)))
        return(NULL)
      withProgress(session = session, value = 0.5, {
        setProgress(message = 'Plotting')
        p <- mfPlot(mf_tbl)
        isolate(plots$mfPlot <- p)
        isolate(values$mf_selected <- rep(FALSE, nrow(mf_tbl)))
        isolate(names(values$mf_selected) <-
                  rownames(mf_tbl))
        if (selected != "") {
          p$data$Selected <- FALSE
          p$data[selected, "Selected"] <- TRUE
          p <- p + aes(color = Selected) +
            scale_color_manual(values = c("black", "red"))
          isolate(values$mf_selected[selected] <- TRUE)

          metafeature <- selected

          mf_tbl <- as.data.frame(mf_tbl)
          abundances <- mf_tbl[metafeature, ]
          abundances <- abundances[which(!is.na(abundances))]
          inds <- which(study_info$study %in% names(abundances))
          df <- study_info[inds, showColumns]
          df$`Relative Abundance` <-
            as.numeric(abundances[study_info$study[inds]])
          df <- df[order(df$`Relative Abundance`, decreasing = TRUE),]

          output$mfName <-
            renderUI(HTML(
              paste0(
                '<p style="text-align: center"><strong>',
                metafeature,
                "</strong></p>"
              )
            ))
          values$mf_studies <- study_info[rownames(df), "study"]
          output$mfTable <- DT::renderDataTable({
            DT::datatable(
              df,
              options = list(
                autoWidth = TRUE,
                pageLength = 5,
                scrollX = TRUE,
                searchHighlight = TRUE
              ),
              selection = 'single',
              escape = FALSE,
              rownames = FALSE
            )
          })
        } else {
          values$mf_studies <- NULL
          output$mfName <- renderUI(NULL)
        }
        output$mfPlot <- renderPlotly({
          ggplotly(
            p,
            tooltip = c(
              "Metafeature",
              "mfCoverage",
              "maxRelAbundance",
              "maxStudy"
            ),
            source = "mf"
          )
        })
      })
    })

    observeEvent(event_data("plotly_click", source = "mf"), {
      event <- event_data("plotly_click", source = "mf")
      if (is.null(event)) {
        return(NULL)
      }
      isolate(mf_tbl <- as.data.frame(values$mf_tbl))
      selected <- event$curveNumber == 1
      isolate(mf_tbl <-
                mf_tbl[which(values$mf_selected == selected), ])

      row <- event$pointNumber + 1
      metafeature <- rownames(mf_tbl)[row]

      updateSelectInput(session,
                        'mfInput',
                        "",
                        c(Metafeature =  "", setNames(
                          rownames(values$mf_tbl), rownames(values$mf_tbl)
                        )),
                        selected = metafeature)
    })

    observeEvent(input$mfTable_rows_selected, {
      row <- input$mfTable_rows_selected
      study <- values$mf_studies[row]
      values$study <- study
      DT::selectRows(mystudiesProxy, which(study_info$study == study))
    })

    ### Load selected study and reset
    observeEvent(values$study, {
      resetPlots()
      resetValues()
      resetWidgets()

      study <- values$study

      output$studyinfo <- renderTable({
        df <-
          study_info[which(study_info$study == study), c(showColumns, "study_type", "study_alias")] %>% t
        rownames(df) <-
          c("Study:",
            "Study abstract:",
            "Sample size:",
            "Study type:",
            "Study alias:")
        rownames(df) <- paste0("<b>", rownames(df), "</b>")
        df
      }, rownames = TRUE, colnames = FALSE, sanitize.text.function = function(x)
        x)

      # load phylo from .RData file
      cls <-
        class(try(loadPhylo(study, DIR, environment())))
      if (cls == "try-error")
      {
        values$phylo <- NULL
        values$attributes <- NULL
        showModal(
          modalDialog(
            title = "Important message",
            'The data for this study is missing! Please choose another study!',
            easyClose = TRUE
          )
        )
        return()
      } else{
        # Remove NA's from metaSRA annotation
        phylo@sam_data$metaSRA.Sample.Type <-
          as.character(phylo@sam_data$metaSRA.Sample.Type)
        phylo@sam_data[is.na(phylo@sam_data)] <- "Unknown"
        phylo@sam_data$metaSRA.Infection.Status <- NULL
      }
      values$phylo <- phylo
      values$attributes <-  sapply(1:length(sample_data(phylo)),
                                   function(x) {
                                     if (nrow(unique(sample_data(phylo)[, x])) >= 2)
                                       return(colnames(phylo@sam_data)[x])
                                   }) %>% unlist
      values$attributes <-
        values$attributes[!values$attributes %in% c("Total.Reads")]
      # for debug
      # assign("phylo", phylo, globalenv())
    })

    observeEvent(values$phylo, {
      if (is.null(values$phylo))
        return(NULL)
      if (length(unique(values$phylo@sam_data$Selection)) >= 2)
        values$attributes <- c(values$attributes, "Selection")
    })

    ##################
    ###  Analysis  ###
    ##################

    ### Diversity analysis
    output$diversity_stats <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      textOutput("diversity_stats")
    })

    output$attribute_da <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        values$attributes
      selectInput('attribute_da', 'Color by', attributes, multiple = TRUE, selected = attributes[1])
    })

    output$diversity <- renderPlotly({
      input$reload_button
      phylo <- isolate(values$phylo)
      if (any(is.null(phylo),
              is.null(input$attribute_da),
              input$attribute_da == ""))
        return(NULL)
      attribute <- input$attribute_da
      if (input$attribute_da == empty)
        attribute <- NULL
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        if(length(attribute) > 1){
          dt <- data.frame(phylo@sam_data)
          col.name <- paste(attribute, collapse = "__")
          dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
          attribute <- col.name
          phylo@sam_data <- sample_data(dt)
        }
        p <- plot_alpha(phylo, attribute)
        isolate(plots$diversity <- p)
        p
      })
    })


    output$diversity_stats <- renderUI({
      phylo <- isolate(values$phylo)
      if (any(is.null(phylo), is.null(input$attribute_da)))
        return("")
      attribute <- input$attribute_da
      if (input$attribute_da == empty)
        return("")
      if(length(attribute) > 1){
        dt <- data.frame(phylo@sam_data)
        col.name <- paste(attribute, collapse = "__")
        dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
        attribute <- col.name
        phylo@sam_data <- sample_data(dt)
      }
      pval <- try(diversity_test(phylo, attribute), silent = TRUE)
      if (class(pval) == "try-error")
        return()
      HTML(paste0("<center><b><p>ACE p-value: ", round(pval[1], 10),
                  "</p><p>Shannon p-value: ", round(pval[2], 10), "</p></b></center>"))
    })

    ### Differential expression
    output$attribute_de <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        values$attributes
      selectInput('attribute_de', 'Select Attribute', attributes, multiple = TRUE, selected = attributes[1])
    })

    output$de_conds <- renderUI({
      phylo <- values$phylo
      attribute <- input$attribute_de
      if (any(is.null(phylo), is.null(input$attribute_de)))
        return(NULL)
      if(length(attribute) > 1){
        dt <- data.frame(phylo@sam_data)
        col.name <- paste(attribute, collapse = "__")
        dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
        attribute <- col.name
        phylo@sam_data <- sample_data(dt)
      }
      conditions <-
        unique(phylo@sam_data[, attribute])
      selectInput(
        'de_conds',
        label = 'Conditions',
        multiple = TRUE,
        choices = c(Conditions = '', conditions)
      )
    })

    output$de_button <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      actionButton('de_button', "Analyze", class = "btn-primary")
    })

    observeEvent(input$select_species_diff, {
      values$species_diff <- input$select_species_diff
    })

    output$cond <- reactive({
      !is.null(values$de_table)
    })

    outputOptions(output, "cond", suspendWhenHidden = FALSE)

    observeEvent(input$de_button, {
      if (any(is.null(values$phylo))) {
        return(NULL)
      }
      de_conds <- input$de_conds
      if (any(de_conds == empty, is.null(de_conds))) {
        showModal(
          modalDialog(
            title = "Important message",
            'There are not enough conditions to compare! Choose another study, or create your own groups at the section "Sample Selection."',
            easyClose = TRUE
          )
        )
        return(NULL)
      }

      if (study_info[study_info$study == values$study, "sample_size"] > MAX_SAMPLES) {
        showModal(modalDialog(
          title = "Important message",
          paste(
            "Please select a study with less than",
            MAX_SAMPLES,
            "samples!"
          ),
          easyClose = TRUE
        ))
        return(NULL)
      }

      withProgress(session = session , value = 0.2, {
        setProgress(message = "Performing Differential Expression Analysis", detail = 'This may take a while...')
        conds <- input$de_conds
        phylo <- values$phylo
        attribute <- input$attribute_de
        if(length(attribute) > 1){
          dt <- data.frame(phylo@sam_data)
          col.name <- paste(attribute, collapse = "__")
          dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
          attribute <- col.name
          phylo@sam_data <- sample_data(dt)
        }
        de_table <- try(deseq2_table(phylo,
                                     attribute,
                                     conds,
                                     parallel = DESEQ_PARALLEL))
        if (class(de_table) == "try-error") {
          showModal(
            modalDialog(
              title = "Important message",
              "An error has occured!",
              easyClose = TRUE
            )
          )
          return(NULL)
        } else {
          values$de_table <- de_table
        }
        incProgress(0.5, detail = "Plotting volcano plot")
        output$deseq_table <- DT::renderDataTable({
          if (is.null(values$de_table))
            return(NULL)
          DT::datatable(
            values$de_table %>% as.data.frame,
            extensions = "Buttons",
            options = list(
              pageLength = 50,
              scrollX = TRUE,
              scrollY = "500px",
              searchHighlight = TRUE,
              dom = '<"top"Bf>rt<"bottom"lip><"clear">',
              buttons = list('print',
                             list(
                               extend =  "csv",
                               title = paste0(values$study,"_de")
                             ), list(
                               extend =  "pdf",
                               title = paste0(values$study,"_de")
                             ))
            ),
            selection = 'none',
            rownames = taxids2names(phylo, rownames(values$de_table))
          )
        })

        output$de_plot <- renderPlotly({
          if (is.null(values$de_table))
            return(NULL)
          p <- try(plot_volcano(values$phylo@tax_table,
                                values$de_table))
          if (class(p) == "try-error") {
            showModal(
              modalDialog(
                title = "Important message",
                "Can't generate the volcano plot!",
                easyClose = TRUE
              )
            )
          } else{
            p$data$selected <- rep(FALSE, nrow(p$data))
            if (all(!is.null(values$species_diff),
                    values$species_diff != "")) {
              # print(taxids2names(values$phylo, values$species_diff))
              p$data[which(p$data$Species %in% taxids2names(values$phylo, values$species_diff)), "selected"] <-
                TRUE
              p <-  p + aes(color = selected) +
                scale_color_manual(values = c("black", "red"))
            }
            isolate(plots$de_plot <- p)
            ggplotly(p,
                     tooltip = c("Species", "x", "y"),
                     source = "de_plot")
          }
        })
      })
      # output$de_stats <- renderTable(rownames = TRUE, digits = 5, {
      #   if(any(is.null(input$select_species_diff), input$select_species_diff == ""))
      #     return(NULL)
      #   # assign("ids", input$select_species_diff, global_env())
      #   de_glm_df(values$phylo, input$select_species_diff, input$attribute_de, input$select_cond1, input$select_cond2)
      # })
    })

    observeEvent(event_data("plotly_click", source = "de_plot"), {
      phylo <- values$phylo
      event <- event_data("plotly_click", source = "de_plot")
      de_table <- values$de_table
      selected_rows <-
        which(rownames(de_table) %in% values$species_diff)
      if (length(selected_rows) != 0) {
        de_table <-
          if (event$curveNumber == 0)
            de_table[-selected_rows,]
        else
          de_table[selected_rows,]
      }
      species <- de_table[event$pointNumber + 1,]
      values$species_diff <-
        unique(c(values$species_diff, rownames(species)))
      updateSelectInput(
        session,
        'select_species_diff',
        label = '',
        choices = c(Species = '', setNames(taxa_names(phylo), phylo@tax_table[, "Species"])),
        selected = values$species_diff
      )
      # print(event$pointNumber + 1)
      # print(species)
      # print(taxids2names(values$phylo, rownames(species)))
    })

    output$de_boxplot <- renderPlotly({
      species_diff <- values$species_diff
      conds <- input$de_conds
      attribute <- isolate(input$attribute_de)
      phylo <- isolate(values$phylo)
      if(length(attribute) > 1){
        dt <- data.frame(phylo@sam_data)
        col.name <- paste(attribute, collapse = "__")
        dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
        attribute <- col.name
        phylo@sam_data <- sample_data(dt)
      }
      if (any(is.null(phylo),
              is.null(species_diff),
              species_diff == ""))
        return(NULL)
      # group by attribute only if specific conditions are also given
      if (!is.null(conds)) {
        environment(subset_samples) <- environment()
        phylo <-
          subset_samples(phylo, unlist(phylo@sam_data[, attribute]) %in% conds)
      } else{
        attribute = "All"
      }
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        p <- plot_diff(phylo, species_diff, attribute)
        isolate(plots$de_boxplot <- p)
        p
      })
    })

    ### Dimension reduction
    output$attribute_dr <- renderUI({
      if (is.null(values$phylo))
        NULL
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        values$attributes
      selectInput('attribute_dr', 'Color by', attributes, multiple = TRUE, selected = attributes[1])
    })

    output$dimred <- renderPlotly({
      input$reload_button
      phylo <- isolate(values$phylo)
      color <- input$attribute_dr
      if (any(is.null(phylo),
              is.null(color),
              color == ""))
        return(NULL)

      if (study_info[isolate(study_info$study) == isolate(values$study), "sample_size"] > MAX_SAMPLES) {
        showModal(modalDialog(
          title = "Important message",
          paste(
            "Please select a study with less than",
            MAX_SAMPLES,
            "samples!"
          ),
          easyClose = TRUE
        ))
        return(NULL)
      }
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        if(length(color) > 1){
          dt <- data.frame(phylo@sam_data)
          col.name <- paste(color, collapse = "__")
          dt <- do.call(unite, list(dt, col.name, color, remove = FALSE, sep = "__"))
          color <- col.name
          phylo@sam_data <- sample_data(dt)
        }
        if (is.null(isolate(plots$dimred))) {
          p <- plot_mds(phylo, color)
          isolate(plots$dimred <- p)
        } else{
          isolate(plots$dimred$data[, color] <-
                    phylo@sam_data[,color])
          p <-
            isolate(plots$dimred + aes_string(colour = color))
          isolate(plots$dimred <- p)
        }
        p
      })
    })

    ### Metafeature correlation
    output$mf_mc <- renderUI({
      phylo <- values$phylo
      if (is.na(phylo)) {
        return(NULL)
      }
      taxa_table <-
        data.frame(phylo@tax_table, stringsAsFactors = FALSE)
      taxa_table[is.na(taxa_table)] <- "Unknown"
      choices <- lapply(colnames(taxa_table), function(level) {
        tmp <- taxa_table[, level] %>% unlist %>% unique
        paste(level, tmp, sep =  " / ")
      }) %>% unlist

      selectInput(
        'mf_mc',
        multiple = FALSE,
        label = 'Select metafeature of interest',
        choices = c(Metafeatures = '', choices)
      )
    })

    output$level_mc <- renderUI({
      phylo <- values$phylo
      if (is.na(phylo)) {
        return(NULL)
      }

      selectInput(
        'level_mc',
        multiple = FALSE,
        label = 'Select Classification level to test against',
        choices = c(Levels = '', phylo@tax_table %>% colnames)
      )
    })

    observeEvent(input$mc_apply_button, {
      level2 <- isolate(input$level_mc)
      mf <- isolate(input$mf_mc)
      phylo <- isolate(values$phylo)

      if (any(is.null(level2), is.null(phylo), is.null(mf))) {
        showModal(modalDialog(
          titl = "Important Message",
          'Please give some input!',
          easyClose = TRUE
        ))
        return(NULL)
      }

      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        mf <- strsplit(mf, " / ")[[1]]
        values$mf_mc <- mf
        level1 <- mf[1]
        mf <- mf[2]

        cor_table <- try(cor_table(phylo, level1, level2, mf))

        if (class(cor_table) == "try-error") {
          showModal(modalDialog(
            titl = "Important Message",
            'An error has occured!',
            easyClose = TRUE
          ))
          return(NULL)
        }

        cor_table <- cbind(rownames(cor_table), cor_table)
        colnames(cor_table) <-
          c(level2,
            "Spearman's rho",
            "p-value",
            "adjusted p-value(FDR)")

        values$cor_table <- cor_table
      })
    })
    output$cor_table <- DT::renderDataTable({
      cor_table <- values$cor_table
      if(is.null(cor_table)){
        return(NULL)
      }
      DT::datatable(
        cor_table, extensions = "Buttons",
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          scrollY = "500px",
          searchHighlight = TRUE,
          dom = '<"top"Bf>rt<"bottom"lip><"clear">',
          buttons = list('print',
                         list(
                           extend =  "csv",
                           title = paste(values$study, "cor", sep="_")
                         ), list(
                           extend =  "pdf",
                           title = paste(values$study, "cor", sep="_")
                         ))
        ),
        selection = list(mode = 'multiple'),
        rownames = FALSE
      )
    })

    observeEvent(input$cor_table_rows_selected, {
      cor_table <- isolate(values$cor_table)
      row <- input$cor_table_rows_selected
      mfx <- values$mf_mc

      if (any(is.null(cor_table), is.null(row), is.null(mfx))) {
        return(NULL)
      }
      levely <- colnames(cor_table)[1]
      mfys <- rownames(cor_table)[row]
      levelx <- mfx[1]
      mfx = mfx[2]
      ncols <- round(length(mfys) ^ 0.5)
      # print(paste0(ncols * 200, "px"))
      output$cor_plot <-
        renderPlot({
          withProgress(session = session, value = 0.5, {
            setProgress(message = "Calculation in progress")
            ps <-
              lapply(
                mfys,
                plot_xy,
                phylo = values$phylo,
                levelx = levelx,
                mfx = mfx,
                levely = levely
              )
            plots$cor_plot <- ps
            ps$cols = ncols
            p <- do.call(multiplot, ps)
            p
          })
        }, height = ncols * 300)
    })

    ### Metafeature abundance
    output$select_species_diff <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)

      selectInput(
        'select_species_diff',
        multiple = TRUE,
        label = 'Select Metafeatures',
        choices = c(Metafeatures = '', setNames(taxa_names(phylo), phylo@tax_table[, "Species"]))
      )
    })

    output$attribute_ma <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(empty, values$attributes)
      selectInput('attribute_ma', 'Color by', attributes, multiple = TRUE, selected = attributes[1])
    })

    output$level_ma <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      clevels <- colnames(phylo@tax_table)
      selectInput('level_ma',
                  'Select Classification Level',
                  clevels,
                  selected = "Species")
    })

    output$top_species_plot <- renderPlotly({
      input$reload_button
      phylo <- isolate(values$phylo)
      top_n <- as.numeric(input$top_n_ma)
      attribute <- input$attribute_ma
      if(length(attribute) > 1){
        if(empty %in% attribute) {
          attribute <- attribute[-which(attribute == empty)]
        }
        dt <- data.frame(phylo@sam_data)
        col.name <- paste(attribute, collapse = "__")
        dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
        attribute <- col.name
        phylo@sam_data <- sample_data(dt)
      }
      level <- input$level_ma
      test <- level == "Kingdom"
      if (any(is.null(attribute),
              is.null(level),
              is.null(phylo),
              input$attribute_ma == ""))
        return(NULL)
      withProgress(session = session, value = 0.5, {
        attribute <-
          ifelse(attribute == empty, "All", attribute)
        setProgress(message = "Calculation in progress")
        p <-
          plot_top_species(
            phylo,
            attribute = attribute,
            level = level,
            top_n = top_n,
            test = test
          )
        isolate(plots$top_species_plot <- p)
        ggplotly(p, tooltip = c("Mf", "Mean", "Selection", "p.value"))
      })
    })

    ### Taxonomy bar chart
    output$attribute_tbc <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(values$attributes)
      selectInput('attribute_tbc', 'Select Grouping', attributes, multiple = TRUE, selected = "sraID")
    })

    output$level_tbc <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      clevels <- colnames(phylo@tax_table)
      selectInput('level_tbc', 'Select Classification Level', clevels)
    })

    output$tbc_button <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      actionButton('tbc_button', "Generate", class = "btn-primary")
    })

    observeEvent(input$tbc_button, {
      attribute <- isolate(input$attribute_tbc)
      phylo <- isolate(values$phylo)
      level <- isolate(input$level_tbc)
      if (any(is.null(attribute),
              is.null(phylo),
              is.null(level)))
        return(NULL)
      attribute <-
        ifelse(attribute == empty,
               "Sample",
               attribute)

      if (study_info[study_info$study == values$study, "sample_size"] > MAX_SAMPLES) {
        showModal(modalDialog(
          title = "Important message",
          paste(
            "Please select a study with less than",
            MAX_SAMPLES,
            "samples!"
          ),
          easyClose = TRUE
        ))
        return(NULL)
      }

      if(length(attribute) > 1){
        dt <- data.frame(phylo@sam_data)
        col.name <- paste(attribute, collapse = "__")
        dt <- do.call(unite, list(dt, col.name, attribute, remove = FALSE, sep = "__"))
        attribute <- col.name
        phylo@sam_data <- sample_data(dt)
      }

      output$taxa_plot <- renderPlotly({
        withProgress(session = session, value = 0.5, {
          setProgress(message = 'Calculation in progress')
          p <- plot_taxa(phylo, attribute, level, relative = FALSE)
          isolate(plots$taxa_plot <- p)
          p
        })
      })

      output$ntaxa_plot <- renderPlotly({
        withProgress(session = session, value = 0.5, {
          setProgress(message = 'Calculation in progress')
          p <- plot_taxa(phylo, attribute, level, relative = TRUE)
          isolate(plots$ntaxa_plot <- p)
          p
        })
      })
      output$cond1 <- reactive({
        TRUE
      })
      outputOptions(output, "cond1", suspendWhenHidden = FALSE)
    })

    ### Sankey diagram
    output$attribute_sankey <- renderUI({
      if (is.null(values$phylo))
        NULL
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(empty, values$attributes)
      selectInput('attribute_sankey', 'Select attribute', attributes)
    })

    output$sankey_condition <- renderUI({
      phylo <- values$phylo
      if (any(is.null(phylo), is.null(input$attribute_sankey)))
        return(NULL)
      conditions <-
        if (length(values$attributes) == 0L ||
            input$attribute_sankey == empty)
          empty
      else
        unique(phylo@sam_data[, input$attribute_sankey])
      selectInput('sankey_condition', label = 'Select Condition',
                  choices = conditions)
    })

    observeEvent({
      values$phylo
      sankey$args
    }, {
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      levls <- colnames(phylo@tax_table)
      updateSelectInput(session,
                        'sankey_source',
                        'Source',
                        levls,
                        selected = sankey$args$source)
      updateSelectInput(session,
                        'sankey_target',
                        'Target',
                        levls,
                        selected = sankey$args$target)
    })

    observeEvent(values$phylo, {
      sankey$newArgs <-
        list(
          source = "Kingdom",
          target = "Phylum",
          source_filter = NULL,
          level_filter = NULL
        )
      sankey$args = list()
      sankey$args_history <- list()
    })

    observeEvent(input$sankey_apply_button, {
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      levls <- colnames(phylo@tax_table)
      source <- input$sankey_source
      target <- input$sankey_target

      if (source == target ||
          (which(levls == source) > which(levls == target))) {
        showModal(
          modalDialog(
            titl = "Important Message",
            'The "Target" should be a subclass of the "Source".',
            easyClose = TRUE
          )
        )
        return()
      }
      sankey$newArgs <- NULL
      sankey$newArgs <-
        list(
          source = input$sankey_source,
          target = input$sankey_target,
          source_filter = NULL,
          level_filter = NULL
        )
      sankey$cond <- input$sankey_condition
      sankey$attribute <- input$attribute_sankey
    })

    observeEvent(input$sankey_reset_button, {
      sankey$newArgs <- NULL
      sankey$newArgs <-
        list(
          source = "Kingdom",
          target = "Phylum",
          source_filter = NULL,
          level_filter = NULL
        )
      sankey$args = list()
      sankey$args_history <- list()
    })

    output$sankey_cond <- reactive({
      length(sankey$args_history) > 0
    })

    outputOptions(output, "sankey_cond", suspendWhenHidden = FALSE)


    observeEvent(event_data("plotly_click", source = "sankey"), {
      js$resetClick()
      if (sankey$args$target == "Species") {
        showModal(
          modalDialog(
            title = "Important Message",
            'Can not go any deeper than the class "Species"',
            easyClose = TRUE
          )
        )
        return()
      }
      link <-
        event_data("plotly_click", source = "sankey")$pointNumber
      source_filter <-
        sankey$sankey_links[link + 1, "Target"] %>% substr(4, nchar(.))
      level_filter <-
        sankey$sankey_links[link + 1, "Target_level"]
      levls <- colnames(values$phylo@tax_table)
      newSource <- levls[which(levls == sankey$args$source) + 1]
      newTarget <- levls[which(levls == sankey$args$target) + 1]
      sankey$newArgs <-
        list(
          source = newSource,
          target = newTarget,
          level_filter = level_filter,
          source_filter = source_filter
        )
    })

    # Dynamically change the heigth of the sankey plot
    output$sankey.ui <- renderUI({
      links <- sankey$sankey_links
      height <- 500
      if (!is.null(links)) {
        height <- height + (10 * nrow(links))
      }
      plotlyOutput("sankey_plot",
                   height = paste0(height, "px"),
                   width = "1300px")
    })

    output$sankey_plot <- renderPlotly({
      if (is.null(values$phylo))
        return(NULL)
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        # isolate(print(sankey$args_history))
        if (isolate(!sankey$undo) &&
            isolate(length(sankey$args)) != 0 &&
            !isolate(identical(sankey$args, sankey$newArgs))) {
          isolate(sankey$args_history[[length(sankey$args_history) + 1]] <-
                    sankey$args)
        } else{
          # print("test")
          sankey$undo <- FALSE
        }
        newArgs <- sankey$newArgs
        isolate(sankey$args <- newArgs)
        isolate(args <- sankey$args)
        phylo <- values$phylo

        # filter phylo
        attribute <- isolate(sankey$attribute)
        cond <- isolate(sankey$cond)
        if (cond != empty && !is.null(cond)) {
          environment(subset_samples) <- environment()
          phylo <-
            subset_samples(phylo, unlist(phylo@sam_data[, attribute]) %in% cond)
        }
        args$phylo <- phylo
        tmp <- try(do.call(plot_sankey, args))
        if (class(tmp) == "try-error") {
          showModal(
            modalDialog(
              title = "Important message",
              "Can't go through this branch!",
              easyClose = TRUE
            )
          )
          return()
        }
        sankey$sankey_links <- tmp$links
        # print(sankey$args_history)
        tmp$plot
      })
    })

    observeEvent(input$sankey_undo_button, {
      sankey$undo <- TRUE
      sankey$newArgs <-
        sankey$args_history[[length(sankey$args_history)]]
      sankey$args_history <-
        sankey$args_history[-length(sankey$args_history)]
    })

    ### Krona chart
    output$attribute_krona <- renderUI({
      if (is.null(values$phylo))
        NULL
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(empty, values$attributes)
      selectInput('attribute_krona', 'Select attribute', attributes)
    })

    output$krona_iframe <-
      renderUI(
        tags$iframe(
          src = "about:blank",
          id = "krona-file",
          width = "100%",
          frameborder = "0",
          height = "100%",
          scrolling = "yes",
          class = "outer"
        )
      )

    observeEvent(input$krona_apply_button, {
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        attribute <- isolate(input$attribute_krona)
        attribute <-
          ifelse(attribute == "none", "sraID", attribute)
        phylo <- values$phylo
        tax_table(phylo) <- tax_table(phylo)[,-8]
        file <- tempfile()
        phylo@sam_data[, attribute] <-
          make.names(unlist(phylo@sam_data[, attribute]))
        try(plot_krona(phylo, file, attribute, trim = TRUE))
        if (file.exists(paste0(file, ".html"))) {
          input <- sourcetools::read(paste0(file, ".html"))
        } else {
          input <- ""
          showModal(
            modalDialog(
              title = "Important message",
              "Krona could not be plotted for the selected attribute!",
              easyClose = TRUE
            )
          )
        }
        js$writeKrona(input)
      })
    })


    ########################
    ###  Customize data  ###
    ########################

    ### Subset samples
    output$mysamples <- DT::renderDataTable({
      if (is.null(values$phylo)) {
        return(DT::datatable(data.frame(Samples = "Empty"), selection = "none"))
      }
      sam_data <-
        values$phylo@sam_data[, -which(values$phylo@sam_data %>% colnames == "All")]
      DT::datatable(
        data.frame(sam_data),extensions = 'Buttons',
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "100%",
          searchHighlight = TRUE,
          dom = '<"top"Bf>rt<"bottom"lip><"clear">',
          buttons = list('print',
                         list(
            extend =  "csv",
            title = paste0(values$study,"_samples")
          ), list(
            extend =  "pdf",
            title = paste0(values$study,"_samples")
          ))
        ),
        rownames = FALSE,
        selection = list(
          mode = 'multiple',
          target = 'row',
          selected = which(sam_data$Selection == input$groups_button)
        )
      )
    })

    observeEvent(input$ss_apply_button, {
      isolate(phylo <- values$phylo)
      action <- isolate(input$ss_radio_button)
      attributes <- isolate(input$ss_text)

      if (any(is.null(attributes), is.null(action), is.null(phylo))) {
        return(NULL)
      }

      attributes <- strsplit(attributes, " / ")

      fun <- ifelse(action == "Keep", `==`, `!=`)
      fun2 <- ifelse(action == "Keep", any, all)

      sam_data <- data.frame(phylo@sam_data, stringsAsFactors = FALSE)
      sam_data[is.na(sam_data)] <- "Unknown"

      environment(subset_samples) <- environment()
      tmp <- sapply(attributes, function(attribute) {
        cond <- attribute[2]
        attribute <- attribute[1]
        return(fun(sam_data[, attribute], cond))
      }) %>% apply(1, fun2)

      phylo <- try(subset_samples(phylo, tmp))
      if (class(phylo) == "try-error") {
        showModal(modalDialog(
          titl = "Important Message",
          'Empty table. Try again!',
          easyClose = TRUE
        ))
        return(NULL)
      }

      values$phylo <- phylo
      resetPlots()
    })

    output$ss_text <- renderUI({
      phylo <- values$phylo
      attributes <- isolate(values$attributes)
      if (is.na(phylo)) {
        return(NULL)
      }
      sam_data <- data.frame(phylo@sam_data, stringsAsFactors = FALSE)
      sam_data[is.na(sam_data)] <- "Unknown"

      choices <- lapply(attributes, function(attribute) {
        tmp <- sam_data[, attribute] %>% unlist %>% unique
        paste(attribute, tmp, sep =  " / ")
      }) %>% unlist

      selectInput(
        'ss_text',
        multiple = TRUE,
        label = '',
        choices = c(Attribute = '', choices)
      )
    })

    observeEvent(input$sel_button, {
      if (!is.null(input$mysamples_rows_selected)) {
        values$phylo@sam_data[input$mysamples_rows_selected, "Selection"] <-
          input$groups_button
      }
    })

    observeEvent(input$addGroup_button, {
      phylo <- values$phylo
      groups <- unique(phylo@sam_data$Selection)
      newGroup <- paste0("Group", "_", length(groups))
      updateRadioButtons(
        session,
        "groups_button",
        "Groups",
        choices = c(groups, newGroup),
        selected = newGroup
      )
    })

    observeEvent(input$groupName_button, {
      phylo <- values$phylo
      inds <- phylo@sam_data$Selection == input$groups_button
      phylo@sam_data$Selection[inds] <- input$group_name
      groups <-
        union(unique(phylo@sam_data$Selection), input$group_name)
      updateRadioButtons(
        session,
        "groups_button",
        "Groups",
        choices = c(groups),
        selected = input$group_name
      )
      values$phylo <- phylo
    })

    ### Subset metafeatures
    output$taxa_table <- DT::renderDataTable({
      if (is.null(values$phylo)) {
        return(DT::datatable(data.frame(Samples = "Empty"), selection = "none"))
      }
      taxa_table <-
        values$phylo@tax_table %>% data.frame(stringsAsFactors = FALSE)
      taxa_table[is.na(taxa_table)] <- "Unknown"
      DT::datatable(
        taxa_table,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "100%",
          searchHighlight = TRUE
        ),
        rownames = FALSE,
        selection = "none"
      )
    })

    observeEvent(input$sm_apply_button, {
      isolate(phylo <- values$phylo)
      action <- isolate(input$sm_radio_button)
      metafeatures <- isolate(input$sm_text)

      if (any(is.null(metafeatures), is.null(action), is.null(phylo))) {
        return(NULL)
      }

      metafeatures <- strsplit(metafeatures, " / ")
      fun <- ifelse(action == "Keep", `==`, `!=`)
      fun2 <- ifelse(action == "Keep", any, all)

      taxa_table <-
        data.frame(phylo@tax_table, stringsAsFactors = FALSE)
      taxa_table[is.na(taxa_table)] <- "Unknown"

      environment(subset_taxa) <- environment()
      tmp <- sapply(metafeatures, function(metafeature) {
        level <- metafeature[1]
        mf <- metafeature[2]
        fun(taxa_table[, level], mf)
      }) %>% apply(1, fun2)

      phylo <- try(subset_taxa(phylo, tmp))

      if (class(phylo) == "try-error") {
        showModal(modalDialog(
          titl = "Important Message",
          'Empty table. Try again!',
          easyClose = TRUE
        ))
        return(NULL)
      }
      values$phylo <- phylo
      resetPlots()
    })

    output$sm_text <- renderUI({
      phylo <- values$phylo
      if (is.na(phylo)) {
        return(NULL)
      }
      taxa_table <-
        data.frame(phylo@tax_table, stringsAsFactors = FALSE)
      taxa_table[is.na(taxa_table)] <- "Unknown"
      choices <- lapply(colnames(taxa_table), function(level) {
        tmp <- taxa_table[, level] %>% unlist %>% unique
        paste(level, tmp, sep =  " / ")
      }) %>% unlist

      selectInput(
        'sm_text',
        multiple = TRUE,
        label = '',
        choices = c(Metafeatures = '', choices)
      )
    })


    ########################
    ###  Extra features  ###
    ########################

    ### Back button
    observeEvent(input$back_button, {
      last <- tabs$last
      if (is.null(last))
        return()
      updateNavbarPage(session, "dataset", selected = last)
    })

    observeEvent(input$dataset, {
      tabs$last <- tabs$current
      tabs$current <- input$dataset
      if (!is.null(tabs$last))
        enable(selector = "#back_button")
    })

    ### Show analysis tabs on selection
    observe({
      if (!is.null(values$phylo)) {
        shinyjs::show(selector = "#dataset li a[data-value='<div id=\"study_title\" class=\"shiny-html-output\"></div>", anim =
                        FALSE)
        shinyjs::show(selector = "#dataset li a[data-value='Customize Data']", anim =
                        TRUE)
        shinyjs::show(selector = "#dataset li a[data-value='Analysis']", anim = TRUE)
      }
    })

    ### Download plots, right click
    observeEvent(input$right_click, {
      click <- input$right_click
      plot.name <- click$plot

      p <- plots[[plot.name]]

      plots$download <- p
    })

    output$ggplot_link <-
      downloadHandler("ggplot.rds", function(file) {
        saveRDS(plots$download, file = file)
      })

    ### Download data
    output$download_samples <- downloadHandler(
      filename = function() {
        paste0(values$study, "_data.zip")
      },
      content = function(file) {
        phylo <- values$phylo
        if (is.null(phylo)) {
          return()
        }
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL

        checkList <- input$check_file
        files <- c()
        if ("Sample info" %in% checkList) {
          samples_fileName <- paste0(values$study, "_sample_info.csv")
          samples <-
            phylo@sam_data %>% data.frame() %>% .[, c("sraID", unlist(attributes), "Total.Reads")]
          write.csv(samples,
                    samples_fileName,
                    row.names = FALSE,
                    col.names = TRUE)
          files <- c(files, samples_fileName)
        }
        if ("OTU Counts" %in% checkList) {
          counts <- phylo@otu_table %>% data.frame() %>%
          {
            rownames(.) <- phylo@tax_table[rownames(.), "Species"]
            .
          }
          counts_fileName <- paste0(values$study, "_counts.csv")
          write.csv(counts,
                    counts_fileName,
                    row.names = TRUE,
                    col.names = TRUE)
          files <- c(files, counts_fileName)
        }
        if ("Feature info" %in% checkList) {
          taxa <- phylo@tax_table %>% data.frame()
          taxa_fileName <-
            paste0(values$study, "_feature_info.csv")
          write.csv(taxa,
                    taxa_fileName,
                    row.names = FALSE,
                    col.names = TRUE)
          files <- c(files, taxa_fileName)
        }
        zip(file, files)
      }
    )

    observeEvent(input$check_file, {
      if (length(input$check_file) == 0) {
        disable("download_samples")
      } else{
        enable("download_samples")
      }
    }, ignoreNULL = FALSE)

    ### Reload button
    output$reload_button <- renderUI({
      if (is.null(values$phylo)) {
        return(NULL)
      }
      actionButton("reload_button",
                   "",
                   icon = icon("refresh", "fa-2x"),
                   class = "btn-primary")
    })

    observeEvent(input$reload_button, {
      withProgress(session = session, value = 0.5, {
        setProgress(message = 'Calculation in progress')
        study <- isolate(values$study)
        resetPlots()
        resetValues()
        loadPhylo(study, DIR, environment())
        phylo@sam_data$metaSRA.Sample.Type <-
          as.character(phylo@sam_data$metaSRA.Sample.Type)
        phylo@sam_data[is.na(phylo@sam_data)] <- "Unknown"
        phylo@sam_data$metaSRA.Infection.Status <- NULL
        values$phylo <- phylo
        values$attributes <-  sapply(1:length(sample_data(phylo)),
                                     function(x) {
                                       if (nrow(unique(sample_data(phylo)[, x])) >= 2)
                                         return(colnames(phylo@sam_data)[x])
                                     }) %>% unlist
        values$attributes <-
          values$attributes[!values$attributes %in% c("Total.Reads")]
      })
    })
  }
