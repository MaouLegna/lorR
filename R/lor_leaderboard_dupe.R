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
#' @param server a character, must be one of "americas", "asia", "europe", "sea"
#' @param api_key a character, default is an environment variables called LORAPI which must be set-up.
#'
#' @return a character vector
#' @export
#'
#' @examples
#' # lor_leaderboard_dupe("europe") # not run
lor_leaderboard_dupe <- function(server,api_key = Sys.getenv("LORAPI_KEY")) {

	path <- "/lor/ranked/v1/leaderboards/"
	APIcall <- api_call(server = "europe",path = path,httr::timeout(3),times=3,quiet=FALSE)
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
