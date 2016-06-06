
get_study <- function(year = NULL, id){

  bd = get_base_data(acrop = crop)
  lf = list.files(bd, recursive =  TRUE, full.names = TRUE)
  fn = paste0(id, ".rda")
  fp = lf[which(stringr::str_detect(lf, fn))]
  fp = fp[length(fp)] # use the latest entry TODO check the download tool
  #if(file.exists(stdy)) return(stdy)

  #fp = get_study_path(year, id)
  stdy = NULL
  try({
    if(file.exists(fp)) {
      stdy = readRDS(file = fp)
    }
  })
  if(is.null(stdy)){
    if(can_internet() & !is.null(brapi)){
      stdy = brapi::study_table(id)
      saveRDS(stdy, fp)
    }
  }
  stdy
}
