get_base_data <- function(mode = "brapi", acrop = "sweetpotato", atype = "fieldbook"){
  bd = fbglobal::get_base_dir(mode = mode)
  fp = file.path(bd, acrop, atype)
  #print("get base data")
  #rint(fp)
  if(!dir.exists(fp)) dir.create(fp, recursive = TRUE)
  fp
}
