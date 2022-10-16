#' LoR-API Handler
#'
#' This function helps you handling the REST API for the LoR API methods
#'
#' #' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' 2022-02-20: As a consequences of very long wait and response for Sea I'm testing a different
#'
#' @param server, a character, must be one of americas,europe,sea or asia,apac
#' @param path a character, string containing the path of the methods of choice. Please refer to the \href{https://developer.riotgames.com/apis}{developer portal}
#' @param api_key a environment variables.
#' @param ... additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' api_call("europe","/lor/ranked/v1/leaderboards/") # not run
#' api_call("europe","/lor/ranked/v1/leaderboards/",timeout(3),times=3,quiet=FALSE)
#' }
api_call <- function(server,path,...,api_key = Sys.getenv("LORAPI_KEY") ) {

	if (api_key=="") stop("Set the LORAPI_key")

	base.url <- lorR::get_path_server(server)
	path <- path

	args <- list(...)
	if ("timeout" %in% names(args)) {
		timeout <- args$timeout
	} else timeout <- 10

	if ("times" %in% names(args)) {
		times <- args$times
	} else times <- 3

	if ("pause_base" %in% names(args)) {
		pause_base <- args$pause_base
	} else pause_base <- 1

	if ("pause_cap" %in% names(args)) {
		pause_cap <- args$pause_cap
	} else pause_cap <- 2

	if ("pause_min" %in% names(args)) {
		pause_min <- args$pause_min
	} else pause_min <- 1

	safeRETRY <- purrr::safely(function(url,...) httr::RETRY(verb = "GET",
																													 url = url,
																													 httr::add_headers("X-Riot-Token" = Sys.getenv("LORAPI_KEY")),
																													 httr::timeout(timeout),
																													 times=times,
																													 pause_base = pause_base,
																													 pause_cap = pause_cap,
																													 pause_min = pause_min,
																													 terminate_on=c(200,400,404,500,429)),
														 otherwise = NULL
	)
	safeRETRY(url=paste0(base.url,path))$result
}
