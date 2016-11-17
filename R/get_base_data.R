get_base_data <- function(amode = "brapi", acrop = "sweetpotato", atype = "fieldbooks", is_server = FALSE){
  bd = fbglobal::get_base_dir(amode = amode, is_server = is_server)
  fp = file.path(bd, acrop, atype)
  #print("get base data")
  #rint(fp)
  if(!dir.exists(fp)) dir.create(fp, recursive = TRUE)
  fp
}
