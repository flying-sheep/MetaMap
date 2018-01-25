#' @include methods.r

#' @title Transform Data
#'
#' Convert tables to input compatible with the webtool.
#'
#' @param tables A directory containing the tables or or list that contains the following files: study_info, sample_info, feature_info, counts
#' @param output_dir The directory where the transformed data will be saved. The default is inside the package.
#' @param log A logical value indicating whether a log file should be generated with info about the process. The default is \code{FALSE}
#'
#' @export
transformData <-
  function(tables,
           output_dir = pkg_file("data"),
           log = F) {
    if (class(tables) == "character") {
      tmp <- c("study_info", "sample_info", "counts", "feature_info")
      tables <- as.list(file.path(tables, paste0(tmp, ".RData")))
      names(tables) <- tmp
    } else if (class(tables) != "list") {
      message("Invalid argument tables!")
      return()
    }

    # read the data
    load(tables$study_info)
    load(tables$sample_info)
    load(tables$counts)
    load(tables$feature_info)

    dir.create(output_dir, showWarnings = F)
    dir.create(file.path(output_dir, "studies"), showWarnings = F)

    lineage <- generateLineage(feature_info)

    r <- runDG(study_info,
             sample_info,
             counts,
             lineage,
             output_dir,
             log)

    if (log) {
      write.table(r, file.path(output_dir, "errors.txt"), row.names = F)
    }
    #add column sample_size to study_info
    study_info$sample_size <-
      sapply(study_info$study, function(x)
        sum(x == sample_info$study))

    save(study_info, file = file.path(output_dir, "study_info.RData"))


    studies <- list.files(file.path(output_dir, 'studies')) %>%
      str_split_fixed("\\.", n = 2) %>% .[, 1]
    mf_tbl <- mfMeans(study_info, studies, output_dir)

    save(mf_tbl, file =  file.path(output_dir, "metafeatures_table.RData"))
  }


# generate phyloseq objects
runDG <-
  function(study_info,
           sample_info,
           counts,
           lineage,
           output_dir,
           log) {
    env <- environment()
    error <- data.frame()
    lapply(study_info$study, function(study) {
      path <- file.path(output_dir, "studies", paste0(study, ".RData"))
      print(path)
      if (file.exists(path))
        return()
      phylo <-
        try(generatePhylo(study, counts, sample_info, lineage))
      if (class(phylo) == "try-error") {
        error <-
          rbind(error, c(study, geterrmessage()), stringsAsFactors = F)
        if (log)
          assign("error", error, env)
      } else{
        save(phylo, file = path)
      }
      print(phylo)
    })
    try(colnames(error) <- c("Study", "Message"), silent = T)
    error
  }
