#' Return Leaderboard Duplicate
#'
#' Return the duplicated players' names
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
#' @param server, a character, must be one of americas,europe,sea or asia,apac
#' @param ... additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,
#'
#' @return a character vector
#' @export
#'
#' @examples
#' \dontrun{
#' lor_leaderboard_dupe("europe")
#' lor_leaderboard_dupe("americas")
#' lor_leaderboard_dupe("asia")
#' }
lor_leaderboard_dupe <- function(server,...) {

	path <- "/lor/ranked/v1/leaderboards/"
	APIcall <- api_call(server = server,path = path,...)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	if (status == 429) { message(glue::glue("Status {status} Wait for {APIcall$headers$`retry-after`}")) }
	if (status %!in% c(200,429)) { warning(glue::glue("Warning - Status {status}")) }
	if (status == 200) {
		res <- jsonlite::fromJSON( httr::content(APIcall, as="text"))$players$name
		if (length(res) != length(unique(res)) ) {
			message(glue::glue("Current number of Master players: {length(res)} but the number of unique names is: {length(unique(res))}!"))
			res[duplicated(res)]
		} else { message(glue::glue("Current number of Master players: {length(res)} with no duplicates!")) }
	}
}
