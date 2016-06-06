get_all_studies <- function(){
  crop = isolate(values$crop)
  mode = isolate(values$mode)
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
