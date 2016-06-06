get_all_studies <- function(){
  fp = file.path(get_base_data(atype = "fieldbook"), "fieldbooks.rda")
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
