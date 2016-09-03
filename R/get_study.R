
get_study <- function(year = NULL, id, crop = "sweetpotato", amode = "brapi"){

  # if amode == Local just read in an excel file (assume DC format for the moment)

  bd = get_base_data(acrop = crop, amode = amode)
  #print(paste("bd", bd))
  lf = list.files(bd, recursive =  TRUE, full.names = TRUE)
  #print(paste("lf", lf))
  fn = id
  #print(paste("fn", fn))
  if(!stringr::str_detect(id, ".rda")){
    fn = paste0(fn, ".rda")
  }
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
  if(is.null(stdy) & amode == "brapi"){
    if(can_internet() ){
      stdy = brapi::study_table(id)
      saveRDS(stdy, fp)
    }
  }
  stdy
}
