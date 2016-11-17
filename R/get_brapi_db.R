
get_brapi_db <- function(){
  dbName = c("sweetpotatobase", "cassavabase") # Must be unique (also from multi-crop databases)
  dbCrop = c("sweetpotato", "cassava")

  dbUrl = c("sweetpotatobase.org", "cassavabase.org")
  dbBrapi = c("brapi/v1", "brapi/v1")
  dbPort = c(80, 80)
  dbUser = c("", "")
  dbPassword = c("", "")

  as.data.frame(cbind(dbName, dbCrop, dbUrl, dbBrapi, dbPort, dbUser, dbPassword),
                stringsAsFactors = FALSE)

}
