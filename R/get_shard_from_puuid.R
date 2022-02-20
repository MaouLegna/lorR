#' Recover the activeShard from a PUUID
#'
#' Given the primary key, a PUUID, return the Server of reference of a player and game associated.
#' Wraps the \href{"https://developer.riotgames.com/apis#account-v1/GET_getActiveShard"}{GET_getActiveShard} api method.
#'
#' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' Method RATE LIMITS
#' *1000 requests every 1 minutes - Developer Key*
#' *1000 requests every 1 minutes - Production Key*
#'
#' @param puuid a character, string for PUUID, a string of 42char like RGAPI-XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
#' @param format   format of the output:
#' parsed - as a vector of only the puuid
#' long   - as a tibble with all the elements from the GET request, named column puuid,game,activeShard
#' text   - as the original json from the API request#'
#' @param ... additional paramter for RETRY function, at the moment are timeout, times, pause_base, pause_cap, pause_min,
#'
#' @return depending on the format chosen return the information for the RiotID. When encountering a status code different from 200 the output is NA
#' the game value should be "lor"
#' the activeShard should be one of "americas", "asia", "europe", "sea"
#' @export
#'
#' @examples
#'
#' \dontrun{
#' puuid <- "kJKtE_3i_66edP3lUYSW3wOVxIl5sRKFhsF6IpNIX_RQxYmyBZxG94gNuR4dUe-ofBq_zy5Yll_gSw"
#' get_shard_from_puuid(puuid)
#' get_shard_from_puuid(puuid, format="long")
#' get_shard_from_puuid(puuid, format="text")
#'
#' badPuuid <- "kJKtE_3i_66edP3lUYSW3wOVxIl5sRKFhsF6IpNIX_RQxYmyBZxG94gNuR4dUe-ofBq_zy5Yll_gST"
#' get_shard_from_puuid(badPuuid) # should return a warning
#' }
get_shard_from_puuid <- function(puuid,format="parsed",...) {

	path = glue::glue("/riot/account/v1/active-shards/by-game/lor/by-puuid/{utils::URLencode(puuid, reserved = T)}")

	# the value of the server is not important when using ACCOUNT methods
	APIcall <- lorR::api_call(server = "europe",path = path,...)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	if (status == 429) { message(glue::glue("Status {status} Wait for {APIcall$headers$`retry-after`}")) }
	if (status %!in% c(200,429)) { warning(glue::glue("Status {status} with puuid: {puuid}")) }
	if (status == 200) {
		switch(
			format,
			"parsed" = unlist(httr::content(APIcall, as= "parsed"),use.names = F)[-1],
			"long"   = tibble::as_tibble(jsonlite::fromJSON(httr::content(APIcall, as= "text"))),
			"text"   = httr::content(APIcall, as= "text")
		)
	}
}
