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
#' @seealso \code{\link[kolejkeR]{get_waiting_time}} and others to extract data directly.
#' @seealso \code{\link[kolejkeR]{get_waiting_time_verbose}} and others for a more verbose output.
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
  office_ids[["office"]]
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
  get_data(office_name)[["nazwaGrupy"]]
}



#' @title Get specific data directly
#' @inheritParams get_raw_data
#' @param queue_name A \code{character} vector describing the queues we are interested in.

#' You can get a list of possible values using \code{\link[kolejkeR]{get_available_queues}} function.
#' @description Several functions to get specific data, such as waiting time, open encounters, current ticket number and
#' amount of people in a set of specific queues in specified office.
#' @describeIn get_waiting_time Returns expected time to be served.
#' @return A \code{character} vector (unless specified diffrently below) of the same length as \code{queue_name}, containing the information dependent on the called function.
#' 
#' If \code{get_waiting_time} is called: A \code{numeric} vector with estimated time of waiting in the queues, in minutes.
#' @examples office <- get_available_offices()[1]
#' queue <- get_available_queues(office)
#' 
#' get_waiting_time(office, queue)
#' 
#' get_open_counters(office, queue)
#' 
#' get_current_ticket_number(office, queue)
#' 
#' get_number_of_people(office, queue)
#' @seealso \code{\link[kolejkeR]{get_waiting_time_verbose}} and others for a more verbose output.
#' @export 
get_waiting_time <- function(office_name, queue_name) {
  
  data <- get_data(office_name)

  if(any(!queue_name %in% data[["nazwaGrupy"]])) stop("Unrecognized queue name!")
  
  minutes <- data[data[["nazwaGrupy"]] == queue_name, "czasObslugi"]
  as.numeric(minutes)
}

#' @title Get specific data verbosely
#' @inheritParams get_raw_data
#' @param queue_name A \code{character} describing a queue we are interested in.

#' You can get a list of possible values using \code{\link[kolejkeR]{get_available_queues}} function.
#' @param language A \code{character}. Only two languages supported: english (\code{"en"}) and polish (\code{"pl"}).
#' @description Several functions to get specific data, such as waiting time, open encounters, current ticket number and
#' amount of people in a set of specified queues in specified office.
#' @describeIn get_waiting_time_verbose Returns expected time to be served.
#' @return A \code{character} vector of the same length as \code{queue_name} with each element in format depending on the called function and the variable \code{language}. Below we assume, that \code{language} variable is default.
#' 
#' If \code{get_waiting_time_verbose} is called: 
#' 
#' "Waiting time for <queue name> is x minutes".
#' @examples office <- get_available_offices()[1]
#' queue <- get_available_queues(office)
#' 
#' get_waiting_time_verbose(office, queue)
#' 
#' get_open_counters_verbose(office, queue)
#' 
#' get_current_ticket_number_verbose(office, queue)
#' 
#' get_number_of_people_verbose(office, queue)
#' @seealso \code{\link[kolejkeR]{get_waiting_time}} and others to extract data directly.
#' @export 
get_waiting_time_verbose <- function(office_name, queue_name, language="en") {
  
  minutes <- get_waiting_time(office_name, queue_name)
  apply(rbind(minutes, queue_name), 2, function(x) {
    as.character(
      glue::glue(texts[[language]][["get_waiting_time"]],
                .envir=list(queue_name=x[2],
                          minutes=x[1],
                          ending=female_endings[as.numeric(x[1]) %% 10 + 1]))
            )
  })
}


#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns amount of opened encounters.
#' @return If \code{get_open_encounters} is called - A \code{numeric} vector with the amounts of opened encounters servicing the queues.
#' @export 
get_open_counters <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  if(any(!queue_name %in% data[["nazwaGrupy"]])) stop("Unrecognized queue name!")
  
  counters <- data[data[["nazwaGrupy"]] == queue_name, "liczbaCzynnychStan"]
  counters
}

#' @inheritParams get_waiting_time_verbose
#' @describeIn get_waiting_time_verbose Returns amount of opened encounters.
#' @return If \code{get_open_encounters_verbose} is called: 
#' 
#' "There are x open encounters for <queue name>".
#' @export 
get_open_counters_verbose <- function(office_name, queue_name, language = "en") {
  
  counters <- get_open_counters(office_name, queue_name)
  apply(rbind(counters, queue_name), 2, function(x) {
    as.character(
      glue::glue(texts[[language]][["get_open_counters"]],
                 .envir=list(queue_name=x[2],
                             counters_literal=counters_to_string()[as.numeric(x[1]) %% 10 + 1],
                             counters=as.character(x[1])))
    )
  })
}

#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns the identifier of the current ticket.
#' @return If \code{get_current_ticket_number} is called - the current ticket identifiers in the queues.
#' @export 
get_current_ticket_number <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  if(any(!queue_name %in% data[["nazwaGrupy"]])) stop("Unrecognized queue name!")
  ticket_number <- data[data[["nazwaGrupy"]] == queue_name, "aktualnyNumer"]
  if(ticket_number == ""){ticket_number <- 0}
  ticket_number
}

#' @inheritParams get_waiting_time_verbose
#' @describeIn get_waiting_time_verbose Returns current ticket number.
#' @return If \code{get_current_number_verbose} is called: 
#' 
#' "Current ticket number is x"
#' @export 
get_current_ticket_number_verbose <- function(office_name, queue_name, language="en") {
  
  ticket_number <- get_current_ticket_number(office_name, queue_name)
  sapply(ticket_number, function(x) {
    as.character(
      glue::glue(texts[[language]][["get_current_ticket_number"]],
                 .envir=list(ticket_number=x))
    )
})
}

#' @inheritParams get_waiting_time
#' @describeIn get_waiting_time Returns amount of people waiting in the queue.
#' @return If \code{get_number_of_people} is called - A \code{numeric} vector with the amounts of people waiting in the queues.
#' @export 
get_number_of_people <- function(office_name, queue_name) {
  
  data <- get_data(office_name)
  
  if(any(!queue_name %in% data[["nazwaGrupy"]])) stop("Unrecognized queue name!")
  
  number_of_people <- data[data[["nazwaGrupy"]] == queue_name, "liczbaKlwKolejce"]
  
  number_of_people
}

#' @inheritParams get_waiting_time_verbose
#' @describeIn get_waiting_time_verbose Returns number of people waiting in specified queue.
#' @return If \code{get_number_of_people_verbose} is called: 
#' 
#' "There are x people in <queue name>"
#' @export 
get_number_of_people_verbose <- function(office_name, queue_name, language = 'en') {
  
  number_of_people <- get_number_of_people(office_name, queue_name)
  apply(rbind(number_of_people, queue_name), 2, function(x) {
    as.character(
      glue::glue(texts[[language]][["get_number_of_people"]],
                 .envir=list(queue_name=x[2],
                             number_of_people=x[1])) 
    )
})
}

