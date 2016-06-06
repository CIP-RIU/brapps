get_all_studies <- function(mode = "brapi", crop = "sweetpotato"){
  #globalVariables(c("values", "crop", "mode"))

  fp = file.path(get_base_data(atype = "fieldbook",
                               acrop = crop,
                               mode = mode),
                 "fieldbooks.rda")
  stds = NULL
  try({
    if(file.exists(fp)) {
      stds = readRDS(file = fp)
    }
  })
  if(is.null(stds)){
    stds = brapi::studies()
    saveRDS(stds, fp)
  }
  stds
}
