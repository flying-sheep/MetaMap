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
           MAX_SAMPLES = 250) {
    # Initialize data
    STUDIES <- list.files(file.path(DIR, 'studies')) %>%
      str_split_fixed("\\.", n = 2) %>% .[, 1]

    load(file.path(DIR, 'study_info.RData'))

    # only show studies that exist in the data/studies directory
    study_info <- subset(study_info, study %in% STUDIES)

    #Fix encoding
    study_info$study_abstract <-
      as_native_character(study_info$study_abstract) %>%
      {
        Encoding(.) <- "UTF-8"
        .
      } %>%
      as_native_character %>%
      {
        Encoding(.) <- "UTF-8"
        .
      }

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

    output$overviewText <- renderUI(
      HTML(
        '<h1 style="color: #5e9ca0;"><span style="color: #000000;">MetaMap - exploring the unexplored</span></h1>
        <p><span style="color: #000000;">This interactive webtools enables the exploration of viral and microbial traits extracted from human RNA-seq data of over 500 studies relevant to human disease. The data was generated using a two-step alignment pipeline outlined below:</span></p>
        <h2 style="color: #2e6c80;">&nbsp;</h2>
        <p>&nbsp;</p>'
      )
      )
    output$queryHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>Query studies by using the search box at the top right. Click on the row to select the study.</strong></p>'
        )
      )
    output$sampleHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To group the samples first click on <em>Add group</em>. Next select samples by clicking on the rows in the table. To assign selected samples to the group click on <em>Select samples</em>. To change the group name use the textfield and click on <em>Change Name</em>.</strong></p>'
        )
      )
    output$dmHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To change the coloring select sample attribute from <em>Color by</em> drop-down menu.</strong></p>'
        )
      )
    output$daHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To change the coloring select sample attribute from <em>Color by</em> drop-down menu. The displayed p-value is calculated from an analysis of variance between the diversity values and the selected sample attribute.</strong></p>'
        )
      )
    output$deHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To plot metafeature expression select a metafeature using the <em>Species</em> textfield. To group samples by attibute select attribute from <em>Select Attribute</em> drop-down menu. To statistically evaluate differential expression of all metafeature select two conditions to compare by using the <em>Condition 1</em> and <em>Condition 2</em> drop-down menus. To run differential expression analysis click on the <em>Analyze</em> button. Please note this may take a couple of minutes depending on the data set size.</strong></p>'
        )
      )
    output$maHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To change the coloring select sample attribute from <em>Color by</em> drop-down menu.</strong></p>'
        )
      )
    output$tbcHelp <-
      renderUI(
        HTML(
          '<p style="text-align: center"><strong>To plot the Taxonomy Bar Chart click on <em>Generate</em>. To group samples together select sample attribute from <em>Select Grouping</em> drop-down menu. To change the coloring select a classification level from <em>Select Classification Level</em> drop-down menu. You can toggle between displaying absolute sequence abundances (by clicking the <em>Value</em> button) or normalized/proportional abundances (by clicking the <em>%</em> button).</strong></p>'
        )
      )

    observeEvent(input$dataset, {
      if (all(!(
        input$dataset %in% c("Overview", "Query metafeatures", "Query studies")
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
      selected <- 1
      if (!is.null(isolate(values$study)))
        selected <- which(study_info$study == isolate(values$study))
      DT::datatable(
        study_info[, showColumns],
        options = list(
          autoWidth = TRUE,
          pageLength = 5,
          scrollX = TRUE,
          searchHighlight = T
        ),
        rownames = FALSE,
        selection = list(mode = 'single', selected = selected),
        escape = F
      )
    })

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
        species_diff = NULL
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
        undo = F,
        cond = empty,
        attribute = empty
      )

    observeEvent(input$mystudies_rows_selected, {
      row <- input$mystudies_rows_selected
      values$study <- study_info$study[row]
    })

    observeEvent(values$study, {
      updateSelectInput(session, 'attribute_dr', 'Color by', "")
      updateSelectInput(session, 'attribute_da', 'Color by', "")
      updateSelectInput(session, 'attribute_ma', 'Color by', "")
      values$phylo <- NULL
      values$selection <- NULL
      values$de_table <- NULL
      values$attributes <- NULL
      values$species_diff = NULL
      output$taxa_plot <- renderPlot({
        NULL
      })
      output$ntaxa_plot <- renderPlot({
        NULL
      })
      # output$top_species_plot <- renderPlot({NULL})
      # output$sankey_plot <- renderSankeyNetwork({NULL})
      output$cond1 <- reactive({
        F
      })

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
      }, rownames = T, colnames = F, sanitize.text.function = function(x)
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
      }
      values$phylo <- phylo
      values$attributes <-  sapply(1:length(sample_data(phylo)),
                                   function(x) {
                                     if (nrow(unique(sample_data(phylo)[, x])) >= 2)
                                       return(colnames(phylo@sam_data)[x])
                                   }) %>% unlist
      values$attributes <-
        values$attributes[!values$attributes %in% c("Total.Reads", "sraID")]
      # for debug
      # assign("phylo", phylo, globalenv())
    })

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
      selectInput('attribute_da', 'Color by', attributes)
    })

    output$attribute_de <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(empty, values$attributes)
      selectInput('attribute_de', 'Select Attribute', attributes)
    })

    output$attribute_dr <- renderUI({
      if (is.null(values$phylo))
        NULL
      phylo <- values$phylo
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        values$attributes
      selectInput('attribute_dr', 'Color by', attributes)
    })

    output$select_species_abundance <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      selectInput(
        'select_species_abundance',
        multiple = T,
        label = '',
        choices = c(Species = '', setNames(taxa_names(phylo), phylo@tax_table[, "Species"]))
      )
    })

    output$select_species_diff <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      selectInput(
        'select_species_diff',
        multiple = T,
        label = '',
        choices = c(Species = '', setNames(taxa_names(phylo), phylo@tax_table[, "Species"]))
      )
    })

    observeEvent(input$select_species_diff, {
      values$species_diff <- input$select_species_diff
    })

    output$select_cond1 <- renderUI({
      phylo <- values$phylo
      if (any(is.null(phylo), is.null(input$attribute_de)))
        return(NULL)
      conditions <-
        if (length(values$attributes) == 0L ||
            input$attribute_de == empty)
          empty
      else
        unique(phylo@sam_data[, input$attribute_de])
      selectInput('select_cond1', label = 'Condition 1',
                  choices = conditions)
    })

    output$select_cond2 <- renderUI({
      phylo <- values$phylo
      if (any(is.null(phylo), is.null(input$attribute_de)))
        return(NULL)
      conditions <-
        if (length(values$attributes) == 0L ||
            input$attribute_de == empty)
          empty
      else
        unique(phylo@sam_data[, input$attribute_de])
      selectInput('select_cond2', label = 'Condition 2',
                  choices = conditions)
    })

    output$de_button <- renderUI({
      if (is.null(values$phylo))
        return(NULL)
      actionButton('de_button', "Analyze")
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
      selectInput('attribute_ma', 'Color by', attributes)
    })

    output$attribute_tbc <- renderUI({
      phylo <- values$phylo
      if (is.null(phylo))
        return(NULL)
      attributes <-
        if (length(values$attributes) == 0L)
          empty
      else
        c(empty, values$attributes)
      selectInput('attribute_tbc', 'Select Grouping', attributes)
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
      actionButton('tbc_button', "Generate")
    })

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

    output$mysamples <- DT::renderDataTable({
      if (is.null(values$phylo))
        return(NULL)
      sam_data <-
        values$phylo@sam_data[, -which(values$phylo@sam_data %>% colnames == "All")]
      DT::datatable(
        data.frame(sam_data),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searchHighlight = T
        ),
        rownames = FALSE,
        selection = list(
          mode = 'multiple',
          target = 'row',
          selected = which(sam_data$Selection == input$groups_button)
        )
      )
    })

    output$download_samples <- downloadHandler(
      filename = function() {
        paste0(values$study, "_data.zip")
      },
      content = function(file) {
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
                    row.names = F,
                    col.names = T)
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
                    row.names = T,
                    col.names = T)
          files <- c(files, counts_fileName)
        }
        if ("Feature info" %in% checkList) {
          taxa <- phylo@tax_table %>% data.frame()
          taxa_fileName <- paste0(values$study, "_feature_info.csv")
          write.csv(taxa,
                    taxa_fileName,
                    row.names = F,
                    col.names = T)
          files <- c(files, taxa_fileName)
        }

        zip(file, files)
      }
    )

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
            easyClose = T
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

    observeEvent(input$sel_button, {
      if (!is.null(input$mysamples_rows_selected)) {
        values$phylo@sam_data[input$mysamples_rows_selected, "Selection"] <-
          input$groups_button
      }
    })

    observeEvent(values$phylo, {
      if (is.null(values$phylo))
        return(NULL)
      if (length(unique(values$phylo@sam_data$Selection)) >= 2)
        values$attributes <- c(values$attributes, "Selection")
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

    output$dimred <- renderPlotly({
      phylo <- isolate(values$phylo)
      if (any(is.null(phylo),
              is.null(input$attribute_dr),
              input$attribute_dr == ""))
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
        plot_mds(phylo, input$attribute_dr)
      })
    })

    output$diversity <- renderPlotly({
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
        plot_alpha(phylo, attribute)
      })
    })


    output$diversity_stats <- renderUI({
      phylo <- isolate(values$phylo)
      if (any(is.null(phylo), is.null(input$attribute_da)))
        return("")
      attribute <- input$attribute_da
      if (input$attribute_da == empty)
        return("")
      pval <- diversity_test(phylo, attribute)
      HTML(paste0("<center><b>P value: ", round(pval, 5), "</b></center>"))
    })

    observeEvent(input$de_button, {
      if (any(
        is.null(values$phylo),
        is.null(input$select_cond1),
        is.null(input$select_cond2),
        "" %in% c(input$select_cond1, input$select_cond2)
      ))
        return(NULL)

      if (input$select_cond1 == empty) {
        showModal(
          modalDialog(
            title = "Important message",
            'There are not enough conditions to compare! Choose another study, or create your own groups at the section "Sample Selection."',
            easyClose = TRUE
          )
        )
        return(NULL)
      }

      if (input$select_cond1 == input$select_cond2) {
        showModal(
          modalDialog(
            title = "Important message",
            "The conditions can't have the same value!",
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
        de_table <- try(deseq2_table(values$phylo,
                                     input$attribute_de,
                                     input$select_cond1,
                                     input$select_cond2))
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
            options = list(pageLength = 10, scrollX = TRUE, searchHighlight = T),
            selection = 'none',
            rownames = taxids2names(values$phylo, rownames(values$de_table))
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
            p$data$selected <- rep(F, nrow(p$data))
            if (all(!is.null(values$species_diff),
                    values$species_diff != "")) {
              # print(taxids2names(values$phylo, values$species_diff))
              p$data[which(p$data$Species %in% taxids2names(values$phylo, values$species_diff)), "selected"] <-
                T
              p <-  p + aes(color = selected) +
                scale_color_manual(values = c("black", "red"))
            }
            ggplotly(p,
                     tooltip = c("Species", "x", "y"),
                     source = "de_plot")
          }
        })
      })
      # output$de_stats <- renderTable(rownames = T, digits = 5, {
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


    output$deseq_table_subset <- DT::renderDataTable({
      if (any(
        is.null(values$de_table),
        is.null(values$species_diff),
        values$species_diff == ""
        # is.null(input$select_species_diff),
        # input$select_species_diff == ""
      ))
        return(NULL)
      # print(values$species_diff)
      de_table_subset <- values$de_table[values$species_diff,]
      DT::datatable(
        de_table_subset %>% as.data.frame,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searchHighlight = T
        ),
        selection = 'none',
        rownames = taxids2names(values$phylo, rownames(de_table_subset))
      )
    })


    output$de_boxplot <- renderPlotly({
      phylo <- isolate(values$phylo)
      if (is.null(phylo) ||
          is.null(values$species_diff) ||
          values$species_diff == "")
        # is.null(input$select_species_diff) ||
        #   input$select_species_diff == "")
        return(NULL)
      attribute <-
        ifelse(input$attribute_de == empty, "All", input$attribute_de)
      withProgress(session = session, value = 0.5, {
        setProgress(message = "Calculation in progress")
        plot_diff(phylo, values$species_diff, attribute)
      })
    })

    # output$abundances_plot <- renderPlot({
    #   if(any(is.null(input$attribute_ma), is.null(values$phylo))) return(NULL)
    #   attribute <- ifelse(input$attribute_ma == empty, "All", input$attribute_ma)
    #   plot_abundance(values$phylo, input$select_species_abundance, attribute)
    # })

    output$top_species_plot <- renderPlot({
      phylo <- isolate(values$phylo)
      attribute <- input$attribute_ma
      if (any(is.null(attribute),
              is.null(phylo),
              input$attribute_ma == ""))
        return(NULL)
      withProgress(session = session, value = 0.5, {
        attribute <-
          ifelse(attribute == empty, "All", attribute)
        setProgress(message = "Calculation in progress")
        plot_top_species(phylo, attribute = attribute)
      })
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
          sankey$undo <- F
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
        }
        sankey$sankey_links <- tmp$links
        # print(sankey$args_history)
        tmp$plot
      })
    })

    observeEvent(input$sankey_undo_button, {
      sankey$undo <- T
      sankey$newArgs <-
        sankey$args_history[[length(sankey$args_history)]]
      sankey$args_history <-
        sankey$args_history[-length(sankey$args_history)]
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

      output$taxa_plot <- renderPlot({
        withProgress(session = session, value = 0.5, {
          setProgress(message = 'Calculation in progress')
          plot_taxa(phylo, attribute, level, relative = F)
        })
      })

      output$ntaxa_plot <- renderPlot({
        withProgress(session = session, value = 0.5, {
          setProgress(message = 'Calculation in progress')
          plot_taxa(phylo, attribute, level, relative = T)
        })
      })
      output$cond1 <- reactive({
        T
      })
      outputOptions(output, "cond1", suspendWhenHidden = FALSE)
    })

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
                  'Metafeature',
                  c("", rownames(values$mf_tbl)))
    })

    observeEvent(input$mfInput, {
      mf_tbl <- isolate(values$mf_tbl)
      selected <- input$mfInput
      if (any(is.null(mf_tbl), is.null(selected)))
        return(NULL)
      withProgress(session = session, value = 0.5, {
        setProgress(message = 'Plotting')
        p <- mfPlot(mf_tbl)
        isolate(values$mf_selected <- rep(F, nrow(mf_tbl)))
        isolate(names(values$mf_selected) <-
                  rownames(mf_tbl))
        if (selected != "") {
          p$data$Selected <- F
          p$data[selected, "Selected"] <- T
          p <- p + aes(color = Selected) +
            scale_color_manual(values = c("black", "red"))
          isolate(values$mf_selected[selected] <- T)

          metafeature <- selected

          mf_tbl <- as.data.frame(mf_tbl)
          abundances <- mf_tbl[metafeature, ]
          abundances <- abundances[which(!is.na(abundances))]
          inds <- which(study_info$study %in% names(abundances))
          df <- study_info[inds, showColumns]
          df$`Relative Abundance` <- as.numeric(abundances)
          df <- df[order(df$`Relative Abundance`, decreasing = T),]

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
                searchHighlight = T
              ),
              selection = 'single',
              escape = F,
              rownames = F
            )
          })
        } else {
          values$mf_studies <- NULL
          output$mfName <- renderUI(NULL)
          output$mfTable <- DT::renderDataTable(NULL)
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
                        'Metafeature',
                        c("", rownames(values$mf_tbl)),
                        selected = metafeature)
    })

    observeEvent(input$mfTable_rows_selected, {
      row <- input$mfTable_rows_selected
      study <- values$mf_studies[row]
      values$study <- study
      DT::selectRows(mystudiesProxy, which(study_info$study == study))
    })

    output$cond <- reactive({
      !is.null(values$de_table)
    })

    output$sankey_cond <- reactive({
      length(sankey$args_history) > 0
    })

    outputOptions(output, "sankey_cond", suspendWhenHidden = FALSE)

    outputOptions(output, "cond", suspendWhenHidden = FALSE)

    observeEvent(event_data("plotly_click", source = "sankey"), {
      js$resetClick()
      if (sankey$args$target == "Species") {
        showModal(
          modalDialog(
            title = "Important Message",
            'Can not go any deeper than the class "Species"',
            easyClose = T
          )
        )
        return()
      }
      link <-
        event_data("plotly_click", source = "sankey")$pointNumber
      source_filter <-
        sankey$sankey_links[link + 1, "Target"] %>% substr(4, nchar(.))
      level_filter <- sankey$sankey_links[link + 1, "Target_level"]
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

  }
