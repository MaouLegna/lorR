#' LoR-API Handler
#'
#' This function helps you handling the REST API for the LoR API methods
#'
#' #' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' @param server, a character, must be one of "americas", "asia", "europe", "sea"
#' @param path a character, string containing the path of the methods of choice. Please refer to the \href{https://developer.riotgames.com/apis}{developer portal}
#' @param api_key a environment variables.
#' @param ... all arguments accepted by httr::RETRY like: httr::timeout(timeout), times, quiet,
#'
#' @return a response
#' @export
#'
#' @examples
#' \dontrun{
#' api_call("europe","/lor/ranked/v1/leaderboards/") # not run
#' api_call("europe","/lor/ranked/v1/leaderboards/",timeout(3),times=3,quiet=FALSE)
#' }
api_call <- function(server, path, ..., api_key = Sys.getenv("LORAPI_KEY") ) {

	if (api_key=="") stop("Set the LORAPI_key")

	base.url <- lorR::get_path_server(server)
	path <- path

	safeRETRY <- purrr::safely(function(url,...) httr::RETRY(verb = "GET",
																													 url = url,
																													 httr::add_headers("X-Riot-Token" = Sys.getenv("LORAPI_KEY")),
																													 terminate_on=c(200,400,404,500,429)),
														 otherwise = NULL
	)
	safeRETRY(url=paste0(base.url,path))$result
}
