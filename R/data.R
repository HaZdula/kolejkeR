#' Warsaw queues data collected from December 28 to January 22
#'
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{aktualnyNumer}{Number of a served person on a given day}
#'   \item{czasObslugi}{Approximate time of service for one person}
#'   \item{date}{Date returned by the API}
#'   \item{idGrupy}{Identifier of a queue}
#'   \item{liczbaCzynnychStan}{Nr of active serving booths}
#'   \item{liczbaKlwKolejce}{Nr of people waiting in a queue for a moment specified by date and time}
#'   \item{literaGrupy}{Identifier of a queue group}
#'   \item{lp}{Nr of queue in a given office}
#'   \item{name}{Name of the office}
#'   \item{nazwaGrupy}{Descriptive name of the queue}
#'   \item{status}{}
#'   \item{time}{Time returned by the API}
#' }
#' @source \url{https://api.um.warszawa.pl/}
"warsaw_queues"
