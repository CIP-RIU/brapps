
is_server <- function() {
  #return(TRUE)
  if(!(get_os() %in% c("windows", "osx"))) return(TRUE)
  return(FALSE)

}
