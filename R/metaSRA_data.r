#' @include methods.r

# Get a list of samples per organ
# metaSRA.file in json format
getMetaSRA <- function(sample_info, metaSRA.file) {
  sample_info <- sample_info[, c("study", "sample", "run")]

  # get metaSRA annotation of each sample
  metaSRA.dt <- metaSRATable(sample_info, metaSRA.file)
  ontologyInfo.dt <- getOntologyInfo(metaSRA.dt$ontology_term)
  setkey(ontologyInfo.dt, ids)
  metaSRA.dt[, `:=`(label = ontologyInfo.dt[metaSRA.dt$ontology_term, name],
                    ontology = ontologyInfo.dt[metaSRA.dt$ontology_term, ontology])]
  list(metaSRA.dt, ontologyInfo.dt)
  # save(metaSRA.dt, ontologyInfo.dt, file = "Data/MetaSRA_annot.RData")
}

annotateDisease <- function(metaSRA.dt, doid.file) {
  # load and edit doid ontology
  # doid.file <- file.path(dir, "doid.json")
  term.info <- jsonlite::fromJSON(doid.file, flatten = F)
  edges <- term.info$graphs$edges[[1]] %>% as.data.table %>%
    .[pred == "is_a"]
  edges[, pred := NULL]
  names(edges) <- c("from", "to")
  edges[, from := tstrsplit(from, split = "/", keep = 5)]
  edges[, from := gsub(pattern = "_", ":", from)]
  edges[, ontology := tstrsplit(from, ":", keep = 1)]
  edges <- edges[ontology == "DOID"]
  edges$ontology <- NULL
  edges[, to := tstrsplit(to, split = "/", keep = 5)]
  edges[, to := gsub(pattern = "_", ":", to)]
  edges[, ontology := tstrsplit(to, ":", keep = 1)]
  edges <- edges[ontology == "DOID"]
  edges$ontology <- NULL
  nodes <-
    term.info$graphs$nodes[[1]][c("id", "lbl")]  %>% as.data.table %>%
    setNames(c("term", "label"))
  nodes[, term := tstrsplit(term, split = "/", keep = 5)]
  nodes[, term := gsub(pattern = "_", ":", term)]
  nodes[, ontology := tstrsplit(term, ":", keep = 1)]
  nodes <- nodes[ontology == "DOID"]
  nodes$ontology <- NULL
  term.info <- NULL
  setkey(nodes, term)

  # add infection status to each doid term
  diseaseTerms.dt <-
    metaSRA.dt[ontology == "DOID", .(ontology_term, label)] %>% unique
  diseaseTerms.dt[, infection_term := sapply(ontology_term, function(x)
    infectionTerm(x, edges))]
  diseaseTerms.dt[, inf_label := nodes[infection_term, label]]

  # diseased vs healthy runs
  diseaseRun.dt <- metaSRA.dt[, .(study,
                                  sample,
                                  sample_type,
                                  term = .SD[ontology == "DOID", ontology_term],
                                  disease.status = .SD[ontology == "DOID", label]), by = run] %>% unique
  tmp <- tidyr::separate(diseaseRun.dt, term, into = c("A", "term"))
  tmp$A <- NULL
  tmp[, term := as.integer(term)]
  diseaseRun.dt <-
    tmp[order(term, decreasing = T), .SD[1], by = run]
  diseaseRun.dt[, term := ifelse(is.na(term), NA, paste0("DOID:", term))]
  # convert some columns to factors
  diseaseRun.dt <-
    unclass(diseaseRun.dt[, .(sample_type, term, disease.status)]) %>% as.data.frame %>%
    as.data.table %>% {
      cbind(diseaseRun.dt[, 1:3], .)
    }
  # add infection status per run
  diseaseRun.dt <- merge(
    diseaseRun.dt,
    diseaseTerms.dt[, -3],
    by.y = c("ontology_term", "label"),
    by.x = c("term", "disease.status"),
    all.x = T
  )
  diseaseRun.dt[, inf_label := !is.na(inf_label)]
  # reorganize diseaseRun.dt
  setnames(diseaseRun.dt, "inf_label", "infected")
  diseaseRun.dt <-
    diseaseRun.dt[, .(study,
                      sample,
                      run,
                      sample_type,
                      disease.status,
                      infected,
                      term)]
  list(diseaseRun.dt, diseaseTerms.dt)
}

# Find doid terms for infections.
infectionTerm <- function(term, edges) {
  ia.id <- "DOID:0050117"
  iaKids.ids <- edges[to == ia.id, from]
  if (term %in% iaKids.ids)
    return(term)
  tmp <- edges[from == term, to]
  while (length(tmp) != 0) {
    found <- F
    for (x in tmp) {
      if (x %in% iaKids.ids) {
        found <- T
        tmp <- x
        break
      }
    }
    if (found)
      break
    tmp <- edges[from %in% tmp, to]
  }
  if (length(tmp) == 0)
    tmp <- NA
  return(tmp)
}
