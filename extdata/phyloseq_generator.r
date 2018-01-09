DIR <- "C:/Users/Giorgos/OneDrive/Documents/Bachelor Bioinformatik/7. Semester/Bachelorarbeit/Data/v2data/"
OUTPUT_DIR <- "data"
LOG <- T

dir.create(OUTPUT_DIR, showWarnings=F)

library(phyloseq)
library(data.table)
library(plyr)
library(dplyr)
library(stringr)

source("methods.r")

load(paste0(DIR, 'study_info.RData'))
load(paste0(DIR,'sample_info.RData'))
load(paste0(DIR, 'counts.RData'))
load(paste0(DIR, 'feature_info.RData'))

lineage <- generateLineage(feature_info)

# generate phyloseq objects
run <- function(){
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
