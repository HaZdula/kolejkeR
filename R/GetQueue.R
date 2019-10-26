office_ids <- list(
  USC_Andersa = "5d2e698a-9c31-456b-8452-7ce33e7deb94",
  UD_Bialoleka = "95fee469-79db-4b4b-9ddc-91d49d1f0f51",
  UD_Bielany = "9c3d5770-57d8-4365-994c-69c5ac4186ee",
  UD_Ochota = "624d7e2a-bf45-48d6-ba79-8b512e662d1c"
)

#' @title function GetAvailableOfficeAcronyms
#' @export
GetAvailableOfficeAcronyms <- function(){
  names(office_ids)
}

#' @title function GetQueue
#' 
#' @param office_acronym \code{character} acronym of office in Warsaw
#' @export
GetQueue <- function(office_acronym="UD_Ochota"){
  # change to import from/depends
  
  # API key, not required for this database
  #ff2f6b11-4eec-4611-a314-d5406bf253fd
  
  url <- "https://api.um.warszawa.pl/api/action/wsstore_get"
  district_id = office_ids[[office_acronym]]
  
  if(is.null(district_id)) stop("Unrecognized office acronym!")
  
  wwstore_get_link <- function(id){
    url <- "https://api.um.warszawa.pl/api/action/wsstore_get"
    paste(url, "/?id=", id, sep = "")
  }
  
  
  url_id <- wwstore_get_link(district_id)
  
  #url_id <- paste(url, "/?id=", ochota_id, sep = "")
  dane <- jsonlite::fromJSON(url_id)
  dane
}


