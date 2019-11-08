#' @title Get raw data
#' @description A function, that returns all, processed data.
#' @param office_name \code{character} acronym of office in Warsaw.
#'  You can get a list of possible values using \code{\link[kolejkeR]{get_available_offices}} function.
#' @return A \code{data.frame} with following columns: 
#' \itemize{
#' \item status
#' \item czasObslugi
#' \item lp
#' \item idGrupy
#' \item liczbaCzynnychStan
#' \item nazwaGrupy
#' \item literaGrupy
#' \item liczbaKlwKolejce
#' \item aktualnyNumer
#' }
#' @seealso \code{\link[kolejkeR]{get_waiting_time}} and others for extracting specific data.
#' @export 
get_raw_data <- function(office_name) {
  get_data(office_name)
}


#' @title Get available offices
#' @description A function, that returns available office names to pass down to other methods.
#' @return A \code{character} vector of names of offices in Warsaw
#' @example offices <- get_available_offices()
#' @export 
get_available_offices <- function() {
  office_ids$office
}


#' @title get_available_queues
#' @inheritParams get_raw_data
#' @export 
get_available_queues <- function(office_name) {
  get_data(office_name)$nazwaGrupy
}


#' @title get_waiting_time
#' @inheritParams get_raw_data
#' @param queue_name A \code{character}.
#' @export 
get_waiting_time <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  minutes <- data[data$nazwaGrupy == queue_name, "czasObslugi"]
  
  paste("Waiting time for", queue_name, "is", minutes, "minutes.") 
}


#' @title get_open_counters
#' @export 
get_open_counters <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  counters <- data[data$nazwaGrupy == queue_name, "liczbaCzynnychStan"]
  
  paste("There are ", counters, "open counters for ", queue_name) 
}


#' @title get_current_ticket_number
#' @export 
get_current_ticket_number <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  ticket_number <- data[data$nazwaGrupy == queue_name, "aktualnyNumer"]
  
  paste("Current ticket number is ", ticket_number) 
}

#' @title get_number_of_people
#' @export 
get_number_of_people <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  number_of_people <- data[data$nazwaGrupy == queue_name, "liczbaKlwKolejce"]
  
  paste("There are ", number_of_people, " people in ", queue_name) 
}