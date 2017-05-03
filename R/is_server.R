
is_server <- function() {
  if(!(get_os() %in% c("windows", "osx"))) return(TRUE)
  return(FALSE)
}
