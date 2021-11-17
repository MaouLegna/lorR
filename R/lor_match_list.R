#' Get the Match History
#'
#' Given a server amd primary key, a PUUID, return the Match History of a player (max 20)
#' Wraps the \href{"https://developer.riotgames.com/apis#lor-match-v1/GET_getMatchIdsByPUUID"}{GET_getMatchIdsByPUUID} api method.
#'
#' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' Method RATE LIMITS
#' *200:3600 - 200 requests every 1 hours   - Developer Key*
#' *30:10    - 30 requests every 10 seconds - Production Key* (not fixed ratio for all Production Keys)
#'
#' @param server a character, must be one of "americas", "asia", "europe". "sea" is not yet supported
#' @param puuid a character, string for PUUID, a string of 42char like RGAPI-XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
#' @param maxPause a numeric, in case of a call with status 429, what's the max wait it can take? default is 10s.
#' With a developer key 120 is the suggested.
#' @param wait a logical, if TRUE (the default), if the pause is of less or equal to 10s it waits and repeat the call once
#' @param quiet a logical, Hide errors (FALSE, the default), or display them as they occur?
#' @param format a character, format of the output, must be:
#' parsed - tibble of n games rows
#' text   - as the original json from the API request
#'
#' @return a tibble, contains three variables:
#' * match_id a character,
#' * puuid  a character, string for PUUID, a string of 42char like RGAPI-XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
#' * server a character, should be one of "americas", "asia", "europe"
#' @export
#'
#' @examples
#' \dontrun{
#' server <- "asia"
#' puuid <- "WE83ZhtKkg_nUWQFSVBlCadvrDAuDZag1q8c_VXRvIs-ttdCP7XscSCsK59ddn2WGVRYq5olFpIW5w"
#' lor_match_list(server=server,puuid=puuid)
#' lor_match_list(server=server,puuid=puuid,format="text")
#' }
lor_match_list <- function(server,puuid,maxPause=10,wait=T,quiet=F,format="parsed") {

	match.list <- tibble::tibble(match_id = character(),
															 puuid    = character(),
															 server   = factor(levels = c("europe","asia","americas"))
	)

	# check if the server is an accepted value
	shards <- c("americas","asia","europe")
	if ( server %!in% shards ) { stop(glue::glue("Provide a server value among one of these: {glue::glue_collapse(shards,sep = ',')}"),call. = F) }

	path = glue::glue("/lor/match/v1/matches/by-puuid/{puuid}/ids/")
	APIcall <- lorR::api_call(server = server,path = path,httr::timeout(3),times=3,quiet=FALSE)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	# If 429 try again, once after the pause
	if (status == 429 & wait==T) {
		pause <- APIcall$headers$`retry-after`

		# In case of the 'standard' rate limit
		if ( base::as.numeric(pause)<=maxPause ) {
			# message(glue::glue("The pause is smaller than 10s {check}"))
			message(glue::glue("Status {status} - rate limit exceed - Wait for {pause}"))
			base::system(glue::glue("Sleep {pause}"))

			APIcall <- lorR::api_call(server = server,path = path,httr::timeout(3),times=3,quiet=FALSE)
			status <- httr::status_code(APIcall)
		}
	}

	# If an error
	if ( status != 200 ){
		message(glue::glue("lor_match_list: Status: {status} - Server: {server}\nPuuid: {puuid}"))
	} else {
		# If status 200 get the match Ids.
		if ( format == "parsed" ) {
			matchIds <- httr::content(APIcall, as= "parsed") |> purrr::flatten_chr()
			# Quality checks
			# Empty match history
			if ( length(matchIds)==0 ) {
				message(glue::glue("lor_match_list: Empty list found!
												 Status: {status} - Server: {server}
												 Puuid: {puuid}") )
			}
			# Less than 20 matches
			if ( length(matchIds) > 0 & length(matchIds) < 20 ) {
				message(glue::glue("lor_match_list: Less than 20 Match History found ({length(matchIds)})
												 Status: {status} - Server: {server}
												 Puuid: {puuid}") )
			}

			match.list <- tibble::add_row(match.list,
																		match_id=matchIds,
																		puuid=puuid,
																		server=server)
			match.list

		} else if ( format == "text" ) {
			httr::content(APIcall, as= "text")
		}
	}
}
