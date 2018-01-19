#' @include methods.r

#' Save Data
#'
#' This function generates a phyloseq Object for each study found in the input tables.
#'
#' @param tables A list that contains the following files: study_info, sample_info, feature_info, counts
#' @param output_dir The directory where the transformed data will be saved. The default is inside the package.
#' @param log A logical value indicating whether a log file should be generated with info about the process. The default is \code{FALSE}
#'
#' @export
transformData <- function(tables, output_dir, log = F){
  # read the data
  load(tables$study_info)
  load(tables$sample_info)
  load(tables$counts)
  load(tables$feature_info)

  dir.create(output_dir, showWarnings=F)

  lineage <- generateLineage(feature_info)



}


# generate phyloseq objects
run <- function(study_info, sample_info, counts, lineage, otuput){
  env <- environment()
  error <- data.frame()
  lapply(study_info$study, function(study){
    path <- file.path(OUTPUT_DIR, "studies", paste0(study, ".RData"))
    print(path)
    if(file.exists(path)) return()
    phylo <- try(generatePhylo(study, counts, sample_info, lineage))
    if(class(phylo) == "try-error"){
      error <- rbind(error, c(study, geterrmessage()), stringsAsFactors = F)
      if(LOG) assign("error", error, env)
    } else{
      save(phylo, file = path)
    }
    print(phylo)
  })
  try(colnames(error) <- c("Study", "Message"))
  error
}

r <- run()

if(LOG){
  write.table(r, file.path(OUTPUT_DIR, "errors.txt"), row.names = F)
}

#add column sample_size to study_info
study_info$sample_size <- sapply(study_info$study, function(x) sum(x == sample_info$study))

save(study_info, file = file.path(OUTPUT_DIR, paste0("study_info.RData")))
