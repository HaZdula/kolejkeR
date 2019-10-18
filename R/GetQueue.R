#' @title function GetQueue
#' 
#' @param nothing, later maybe ID or district name?
#' @export
GetQueue <- function(){
  # change to import from/depends
  library("jsonlite")  
  
  # API key, not required for this database
  #ff2f6b11-4eec-4611-a314-d5406bf253fd
  
  url <- "https://api.um.warszawa.pl/api/action/wsstore_get"
  
  ochota_id <- "624d7e2a-bf45-48d6-ba79-8b512e662d1c"
  
  wwstore_get_link <- function(id){
    url <- "https://api.um.warszawa.pl/api/action/wsstore_get"
    paste(url, "/?id=", id, sep = "")
  }
  
  
  wwstore_get_link(ochota_id)
  
  url_id <- paste(url, "/?id=", ochota_id, sep = "")
  dane <- jsonlite::fromJSON(url_id)
  dane
}
