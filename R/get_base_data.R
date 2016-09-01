get_base_data <- function(amode = "brapi", acrop = "sweetpotato", atype = "fieldbooks"){
  bd = fbglobal::get_base_dir(amode = amode)
  fp = file.path(bd, acrop, atype)
  #print("get base data")
  #rint(fp)
  if(!dir.exists(fp)) dir.create(fp, recursive = TRUE)
  fp
}
