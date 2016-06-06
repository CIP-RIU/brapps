
get_study <- function(year = NULL, id, crop = "sweetpotato", mode = "brapi"){

  bd = get_base_data(acrop = crop, mode = mode)
  lf = list.files(bd, recursive =  TRUE, full.names = TRUE)
  fn = paste0(id, ".rda")
  fp = lf[which(stringr::str_detect(lf, fn))]
  if(length(fp) == 0) {
    if(length(year) == 0){
      fp = file.path(bd, fn)
    } else {
      fp = file.path(bd, year, fn)
    }
    dn = dirname(fp)
    # print(fp)
    # print(dn)
    if(!dir.exists(dn)) dir.create(dn, recursive = TRUE)

    if(can_internet() ){
      stdy = brapi::study_table(id)
      saveRDS(stdy, fp)
    }
  }
  fp = fp[length(fp)] # use the latest entry TODO check the download tool

  #fp = get_study_path(year, id)
  stdy = NULL
  try({
    if(file.exists(fp)) {
      stdy = readRDS(file = fp)
    }
  })
  if(is.null(stdy)){
    if(can_internet() ){
      stdy = brapi::study_table(id)
      saveRDS(stdy, fp)
    }
  }
  stdy
}
