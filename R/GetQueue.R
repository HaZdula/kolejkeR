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
  
  id_USC_Andersa <- "5d2e698a-9c31-456b-8452-7ce33e7deb94"
  id_UD_Białołęka <- "95fee469-79db-4b4b-9ddc-91d49d1f0f51"
  id_UD_Bielany <- "9c3d5770-57d8-4365-994c-69c5ac4186ee"
  id_UD_Ochota <- "624d7e2a-bf45-48d6-ba79-8b512e662d1c"
  
  wwstore_get_link <- function(id){
    url <- "https://api.um.warszawa.pl/api/action/wsstore_get"
    paste(url, "/?id=", id, sep = "")
  }
  
  
  url_id <- wwstore_get_link(id_UD_Ochota)
  
  #url_id <- paste(url, "/?id=", ochota_id, sep = "")
  dane <- jsonlite::fromJSON(url_id)
  dane
}
