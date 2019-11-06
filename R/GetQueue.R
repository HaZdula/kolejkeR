office_ids <- list(
  USC_Andersa = "5d2e698a-9c31-456b-8452-7ce33e7deb94",
  UD_Bialoleka = "95fee469-79db-4b4b-9ddc-91d49d1f0f51",
  UD_Bielany = "9c3d5770-57d8-4365-994c-69c5ac4186ee",
  UD_Ochota = "624d7e2a-bf45-48d6-ba79-8b512e662d1c",
  USC_Falęcka =  "ef5df1a7-882e-4cc5-815b-78768e985724",
  UD_Wola = "7ef70889-4eb9-4301-a970-92287db23052",
  UD_Żoliborz = "831ef31a-b2a3-4cbb-aaa5-cb90fe05ad8c",
  UD_Bemowo = "b29925fb-edaf-41f5-a2bc-86247501009b",
  UD_Wlochy = "05e32b8b-273b-4684-8dd0-8cd5c04dbb81",
  UD_Ursus = "06396204-e8c1-4139-80c0-a099ee3c448f",
  UD_Targowek = "9b8e3980-2d7d-47b7-8c44-4b5e0ca452fe",
  UD_Srodmiescie_1 = "78f6290a-0a19-482c-9641-8ac06f49c1c2",
  UD_Srodmiescie_2 = "306e1f95-d25c-45ee-a00a-0a91f1b07de0",
  UD_Rembertow = "a0d1cb24-c6bf-4574-b3e0-868fa47f14cc",
  UD_Praga_Polnoc = "1a97b69c-b8e7-4f58-b9de-5554bcc1e2d6",
  UD_Praga_Poludnie = "a4343970-0fde-4b07-abe9-79ca62b188ea",
  UD_Mokotow_1 = "fb3abb79-7c2e-4b83-950d-5c48792855f3",
  UD_Mokotow_2 = "bc83ab5a-0ccc-4e4a-b58d-b821e16df176",
  USC_Smyczkowa = "b03cf70a-cda7-4fc1-86d3-b9257e78033f",
  USC_Klopotowskiego = "66a08fa0-a9d4-4f16-be73-08c6fa0c20d2",
  USC_Falecka = "06b7c4bd-123d-4ea7-8de1-eb749a56560f",
  USC_Sandomierska = "1add8722-fe63-45aa-95c8-cf7f0ddd1fe0"
  # Pozostałe zwracają "Nieautoryzowany dostęp do danych". Chyba bez API-key się nie obejdzie
)

#' @title function GetAvailableOfficeAcronyms
#' @export
GetAvailableOfficeAcronyms <- function(){
  names(office_ids)
}

#' @title GetQueue
#' @description A function, that returns data concerning queues in offices in Warsaw.
#' @param office_acronym \code{character} acronym of office in Warsaw.
#'  You can get a list of possible values using \code{\link[kolejkeR]{GetAvaibleOfficeAcronyms}} function.
#' 
#' @return A \code{list} with one element, a \code{list} result, which contains contains:
#' \itemize{
#' \item date - a date of request, as a \code{character}.
#' \item grupy - a \code{data.frame} with the data concerning the queues.
#' \item time - an hour of the request, as a \code{character}.
#' }
#' @examples 
#' data <- GetQueue(office_acronym = "UD-Bielany")
#' grupy <- data[["result"]][["grupy"]]
#' # What's the mean time of service?
#' mean(as.numeric(grupy[["czasObslugi"]]))
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




