#' Return Leaderboard
#'
#' Return the leaderboard of Master players
#' Wraps the \href{https://developer.riotgames.com/apis#lor-ranked-v1/GET_getLeaderboards}{leaderboard} api method.
#' The leaderboard is updated once an hour.
#'
#' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' Method RATE LIMITS
#' *X-Method-Rate-Limit": "30:10,500:600 - Developer Key*
#' *X-Method-Rate-Limit": "500:10,30000:600 - Production Key*
#'
#' @param server, a character, must be one of americas,europe or sea.
#' @param names a logical, is TRUE return only the a vector of players names.
#' @param ... additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,
#'
#' @return a data frame n x3 with n equal to the number of current Master players
#' name - gameName of the player
#' rank - rank on the leaderboard starting from 0 to n-1
#' lp - League Points of a player
#' @export
#'
#' @examples
#' \dontrun{
#' # lor_leaderboard("europe",names = TRUE)
#' # lor_leaderboard("americas")
#' }
lor_leaderboard <- function(server,names=FALSE,...) {

	path <- "/lor/ranked/v1/leaderboards/"
	APIcall <- api_call(server = server,path = path,...)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	if (status == 429) { message(glue::glue("Status {status} Wait for {APIcall$headers$`retry-after`}")) }
	if (status %!in% c(200,429)) { warning(glue::glue("Warning - Status {status}")) }
	if (status == 200) {

		res <- jsonlite::fromJSON( httr::content(APIcall, as="text"))

		if ( names == F ) {
			res$players
		} else {
			res$players$name
		}
	}
}
