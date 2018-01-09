# DIR <- "C:/Users/Giorgos/OneDrive/Documents/Bachelor Bioinformatik/7. Semester/Bachelorarbeit/Data/v2data/"
# DIR <- "."
OUTPUT_DIR <- "DA_tables"
# MAX_SAMPLES <- 50
MAX_SAMPLES <- 1000
LOG <- T

dir.create(OUTPUT_DIR, showWarnings=F)

library(phyloseq)
library(plyr)
library(dplyr)
library(stringr)

source("methods.r")

# load(paste0(DIR, 'study_info.RData'))
# load(paste0(DIR,'sample_info.RData'))
# load(paste0(DIR, 'counts.RData'))
# load(paste0(DIR, 'feature_info.RData'))

# lineage <- generateLineage(feature_info)

run <- function() {
  env <- environment()
  error <- data.frame()
  studies <- sapply(strsplit(list.files("data/studies"), split = "\\."), `[`, 1)
  res <- do.call(rbind, lapply(studies, function(study){
    # phylo <- try(generatePhylo(study, counts, sample_info, lineage))
    cls <- class(try(load(file.path("data/studies", paste0(study, ".RData")))))
    if(cls == "try-error" || length(sample_names(phylo)) > MAX_SAMPLES) {
      error <- rbind(error, c(study, NA, geterrmessage()), stringsAsFactors = F)
      if(LOG) assign("error", error, env)
      return(NULL)
    }
    attributes <- sample_data(phylo) %>% colnames %>% 
      subset(!. %in% c("sraID", "Total.Reads", "Selection", "All"))
    sapply(attributes, function(attribute){
      print(paste0(study, "_", attribute))
      PVal <- try(diversity_test(phylo, attribute))
      if(class(PVal) == "try-error"){
        error <- rbind(error, c(study, attribute, geterrmessage()), stringsAsFactors = F)
        if(LOG) assign("error", error, env)
        return(NA)
      } else
        return(PVal)
    }) %>% 
      data.frame(Study = rep(study, length(attributes)), Phenotype = names(.), Pvalue = .)
  }))
  colnames(error) <- c("Study", "Phenotype", "Messsage")
  return(list(Result = res, Error = error))
}

r <- run()

r$Result %>%
  .[order(.$Pvalue),] %>% 
  write.csv(file.path(OUTPUT_DIR, "diversity_analysis.csv"), row.names=F)

if(LOG){
  write.csv(r$Error, file.path(OUTPUT_DIR, "da_log.csv"), row.names = F)
}
