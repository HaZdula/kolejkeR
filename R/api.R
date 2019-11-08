#' @title Get raw data
#' @description A function, that returns all, unprocessed data concerning one of the offices in Warsaw.
#' @param office_name \code{character} acronym of office in Warsaw.
#' 
#'  You can get a list of possible values using \code{\link[kolejkeR]{get_available_offices}} function.
#' @return A \code{data.frame} with following columns: 
#' \itemize{
#' \item status - either 0 (queue is not operating) or 1 (queue is operating).
#' \item czasObslugi - expected time of waiting in queue, in minutes. See also: \code{\link[kolejkeR]{get_waiting_time}}.
#' \item lp - ordinal number.
#' \item idGrupy - ID of a queue from \code{nazwaGrupy}.
#' \item liczbaCzynnychStan - amount of opened counters. See also: \code{\link[kolejkeR]{get_open_counters}}.
#' \item nazwaGrupy - a name of a queue. See also: \code{\link[kolejkeR]{get_available_queues}}.
#' \item literaGrupy - a single letter symbolizing a queue name from \code{nazwaGrupy}. 
#' \item liczbaKlwKolejce - amount of people in queue. See also: \code{\link[kolejkeR]{get_number_of_people}}.
#' \item aktualnyNumer - current ticket number. See also: \code{\link[kolejkeR]{get_current_ticket_number}}.
#' }
#' @examples 
#' office <- get_available_offices()[1]
#' get_raw_data(office)
#' @seealso \code{\link[kolejkeR]{get_waiting_time}} and others for extracting specific data.
#' @export 
get_raw_data <- function(office_name) {
  get_data(office_name)
}


#' @title Get available offices
#' @family {getters}
#' @description A function, that returns available office names to pass down to other methods.
#' @return A \code{character} vector of names of offices in Warsaw
#' @examples offices <- get_available_offices()
#' @export 
get_available_offices <- function() {
  office_ids$office
}


#' @title Get available queues
#' @family {getters}
#' @description A function, that returns available queue names to pass down to other methods.
#' @return A \code{character} vector of names of queues in one of the offices in Warsaw.
#' @inheritParams get_raw_data
#' @examples office <- get_available_offices()[1]
#' get_available_queues(office)
#' @export 
get_available_queues <- function(office_name) {
  get_data(office_name)$nazwaGrupy
}


#' @title Get specific data
#' @inheritParams get_raw_data
#' @param queue_name A \code{character} describing a queue we are interested in.
#' 
#' You can get a list of possible values using \code{\link[kolejkeR]{get_available_queues}} function.
#' @description Several functions to get specific data, such as waiting time, open encounters, current ticket number nad
#' amount of people in a specific queue in specified office.
#' @describeIn get_waiting_time Returns expected time to be served.
#' @return A \code{character} in format depending on the called function.
#' 
#' If \code{get_waiting_time} is called: 
#' 
#' "Waiting time for <queue name> is x minutes".
#' @examples office <- get_available_offices()[1]
#' queue <- get_available_queues(office)
#' 
#' get_waiting_time(office, queue)
#' 
#' get_open_encounters(office, queue)
#' 
#' get_current_ticket_number(office, queue)
#' 
#' get_number_of_people(office, queue)
#' @export 
get_waiting_time <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  minutes <- data[data$nazwaGrupy == queue_name, "czasObslugi"]
  
  paste("Waiting time for", queue_name, "is", minutes, "minutes.") 
}


#' @title get_open_counters
#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns amount of opened encounters.
#' @return If \code{get_open_encounters} is called: 
#' 
#' "There are x open encounters for <queue name>".
#' @export 
get_open_counters <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  counters <- data[data$nazwaGrupy == queue_name, "liczbaCzynnychStan"]
  
  paste("There are ", counters, "open counters for ", queue_name) 
}


#' @title get_current_ticket_number
#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns current ticket number.
#' @return If \code{get_current_number} is called: 
#' 
#' "Current ticket number is x"
#' @export 
get_current_ticket_number <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  ticket_number <- data[data$nazwaGrupy == queue_name, "aktualnyNumer"]
  
  paste("Current ticket number is ", ticket_number) 
}

#' @title get_number_of_people
#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns number of people waiting in specified queue.
#' @return If \code{get_number_of_people} is called: 
#' 
#' "There are x people in <queue name>"
#' @export 
get_number_of_people <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  number_of_people <- data[data$nazwaGrupy == queue_name, "liczbaKlwKolejce"]
  
  paste("There are ", number_of_people, " people in ", queue_name) 
}