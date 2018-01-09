# DIR <- "C:/Users/Giorgos/OneDrive/Documents/Bachelor Bioinformatik/7. Semester/Bachelorarbeit/Data/v2data/"
# DIR <- "."
OUTPUT_DIR <- "DE_tables"
MAX_SAMPLES <- 50
# MAX_SAMPLES <- 1000
LOG <- T

dir.create(OUTPUT_DIR, showWarnings=F)

library(phyloseq)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(magrittr)

source("methods.r")

# load(paste0(DIR, 'study_info.RData'))
# load(paste0(DIR,'sample_info.RData'))
# load(paste0(DIR, 'counts.RData'))
# load(paste0(DIR, 'feature_info.RData'))
# 
# lineage <- generateLineage(feature_info)

run <- function() {
  env <- environment()
  error <- data.frame()
  studies <- sapply(strsplit(list.files("data/studies"), split = "\\."), `[`, 1)
  lapply(studies, function(study){
    # phylo <- try(generatePhylo(study, counts, sample_info, lineage))
    cls <- class(try(load(file.path("data/studies", paste0(study, ".RData")))))
    if(cls == "try-error" || length(sample_names(phylo)) > MAX_SAMPLES) {
      error <- rbind(error, c(study, NA, geterrmessage()), stringsAsFactors = F)
      if(LOG) assign("error", error, env)
      return(NULL)
    }
    attributes <- sample_data(phylo) %>% colnames %>% 
      subset(!. %in% c("sraID", "Total.Reads", "Selection", "All"))
    lapply(attributes, function(attribute){
      file_path <- file.path(OUTPUT_DIR,paste0(study,"_", attribute, ".csv"))
      print(file_path)
      if(file.exists(file_path)) return()
      de_table <- try(deseq2_table(phylo, attribute))
      if(class(de_table) == "try-error"){
        error <- rbind(error, c(study, attribute, geterrmessage()), stringsAsFactors = F)
        if(LOG) assign("error", error, env)
        return(NULL)
      } else {
        de_table %>%
          set_rownames(taxids2names(phylo, rownames(.))) %>%
          write.csv(file_path)
      }
    }) 
  })
  colnames(error) <- c("Study", "Phenotype", "Messsage")
  error
}

r <- run()

if(LOG){
  write.csv(r, file.path(OUTPUT_DIR, "de_log.csv"), row.names = F)
}

