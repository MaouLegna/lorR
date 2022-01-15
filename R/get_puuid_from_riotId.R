#' Recover the PUUID from a riotID
#'
#' Given the secondary key, a RiotID, return the PUUID of a player.
#' Wraps the \href{"https://developer.riotgames.com/apis#account-v1/GET_getByRiotId"}{GET_getByRiotId} api method.
#'
#' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' Method RATE LIMITS
#' *1000 requests every 1 minutes - Developer Key*
#' *1000 requests every 1 minutes - Production Key*
#'
#' @param gameName a character, string for gameName
#' @param tagLine a character, string for tagLine
#' @param format a character, format of the output, must be:
#' parsed - as a vector of only the puuid
#' text   - as the original json from the API request
#'
#' @return depending on the format chosen return the information for the PUUID When encountering a status code different from 200 the output is NA
#' @export
#'
#' @examples
#' \dontrun{
#' lorR::get_puuid_from_riotId(gameName = "MaouLegna",tagLine = "STAT")
#' }
get_puuid_from_riotId <- function(gameName,tagLine,format="parsed") {

	path = glue::glue("/riot/account/v1/accounts/by-riot-id/{utils::URLencode(gameName, reserved = T)}/{utils::URLencode(tagLine, reserved = T)}")
	# the value of the server is not important when using ACCOUNT methods
	APIcall <- api_call(server = "europe",path = path,quiet=FALSE)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	if (status == 429) { message(glue::glue("Status {status} Wait for {APIcall$headers$`retry-after`}")) }
	if (status %!in% c(200,429)) { warning(glue::glue("Status {status} with gameName: {gameName} and tagLine: {tagLine}")) }
	if (status == 200) {
		switch(
			format,
			"parsed" = httr::content(APIcall, as= "parsed")$puuid,
			"text" = httr::content(APIcall, as= "text")
		)
	}
}
