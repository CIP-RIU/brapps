get_all_studies <- function(amode = "brapi", crop = "sweetpotato",
                            db = "sweetpotatobase", is_server = FALSE){
  #globalVariables(c("values", "crop", "mode"))
  if(amode == "brapi"){
    fp = file.path(fbglobal::get_base_dir(amode = amode, is_server = is_server), crop,
                   paste0("table_studies.rds"))
    stds = NULL
    if(file.exists(fp)){
      stds = try({
         readRDS(fp)
      })
    }

    return(stds)
  }

  fp = file.path(get_base_data(atype = "fieldbook",
                               acrop = crop,
                               amode = amode),
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
