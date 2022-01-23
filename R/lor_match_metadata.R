#' Get the Match Metadata
#'
#' Given a server amd a match_id, return the Match Metadata of a match
#' Wraps the \href{"https://developer.riotgames.com/apis#lor-match-v1/GET_getMatch"}{GET_getMatch} api method.
#'
#' Standard RATE LIMITS
#' *20 requests every 1 seconds(s) / 100 requests every 2 minutes(s) - Developer Key*
#' *500 requests every 10 seconds  / 30,000 requests every 10 minutes - Production Key*
#'
#' Method RATE LIMITS
#' *100:3600 - 200 requests every 1 hours   - Developer Key*
#' *30:10    - 30 requests every 10 seconds - Production Key* (not fixed ratio for all Production Keys)
#'
#' @param server a character, must be one of "americas","apac","europe","asia","sea"
#' Note; asia and sea are going to be removed after the 20th of January 2022
#' @param match_id a character, string for match_id, a string of 36char
#' @param maxPause a numeric, in case of a call with status 429, what's the max wait it can take? default is 10s.
#' With a developer key 120 is the suggested.
#' @param wait a logical, if TRUE (the default), if the pause is of less or equal to 10s it waits and repeat the call once
#' @param format a character, format of the output, must be:
#' parsed - tibble of n row for n match
#' long   - tibble of p row for each participant in a match
#' text   - as the original json from the API request
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#' server <-  "europe"
#' match_id <- "44a130ae-12f4-45f8-8a24-0b319265d616"
#  match_id <- "5c52dab6-4a50-491e-afdd-fcf32cadba0c"
#' lor_match_metadata(server=server,match_id=match_id)
#' lor_match_metadata(server=server,match_id=match_id,format="text")
#' lor_match_metadata(server=server,match_id=match_id,format="long")
#' }
lor_match_metadata <- function(server,match_id,maxPause=10,wait=T,format="parsed") {

	# Create the wide tidy table
	LoR.Metadata <- tibble::tibble(match_key = character(),
																 server = character(),
																 data_version = character(),
																 match_id = character(),

																 participants_1 = character(),
																 participants_2 = character(),
																 participants_3 = character(),
																 participants_4 = character(),

																 game_mode = character(),
																 game_type = character(),

																 game_start_time_utc = character(),
																 # game_start_time_utc = as.POSIXct(character(), format = "%Y-%m-%dT%H:%M:%OS" , tz = "UTC"),

																 game_version = character(),

																 puuid_1     = character(),
																 deck_id_1   = character(),
																 deck_code_1 = character(),
																 factions_1  = character(),
																 game_outcome_1  = character(),
																 order_of_play_1 = numeric(),

																 puuid_2     = character(),
																 deck_id_2   = character(),
																 deck_code_2 = character(),
																 factions_2 = character(),
																 game_outcome_2  = character(),
																 order_of_play_2 = numeric(),

																 puuid_3     = character(),
																 deck_id_3   = character(),
																 deck_code_3 = character(),
																 factions_3 = character(),
																 game_outcome_3  = character(),
																 order_of_play_3 = numeric(),

																 puuid_4     = character(),
																 deck_id_4   = character(),
																 deck_code_4 = character(),
																 factions_4 = character(),
																 game_outcome_4  = character(),
																 order_of_play_4 = numeric(),

																 total_turn_count = numeric(),
																 status = numeric()
	);

	shards <- c("americas","apac","europe","asia","sea")
	if ( server %!in% shards ) { stop(glue::glue("Provide a server value among one of these: {glue::glue_collapse(shards,sep = ',')}"),call. = F) }

	path = glue::glue("/lor/match/v1/matches/{match_id}")
	APIcall <- lorR::api_call(server = server,path = path)

	# check if the APIcall wasn't "safely" done
	if (is.null(APIcall)) return(NULL)

	status <- httr::status_code(APIcall)

	# If 429 try again, once after the pause
	if (status == 429 & wait==T) {
		pause <- APIcall$headers$`retry-after`

		# In case of the 'standard' rate limit
		if ( base::as.numeric(pause)<=maxPause ) {
			message(glue::glue("Status {status} - rate limit exceed - Wait for {pause}"))
			base::Sys.sleep(pause)

			APIcall <- lorR::api_call(server = server,path = path)

			# check if the APIcall wasn't "safely" done
			if (is.null(APIcall)) return(NULL)

			status <- httr::status_code(APIcall)
		} else {
			stop(glue::glue("A long pause has appeared: {pause} / something is wrong if you are using a poverful production key!"))
		}
	}

	if ( status != 200 ){
		message(glue::glue("lor_match_metadata: Status: {status} - Server: {server}
											 match_id: {match_id}"))
	}

	switch(
		format,
		"parsed" = {
			LoR.Metadata |>
				tibble::add_row(
					#
					match_key = match_id,
					server = server,
					# content from the API GET()
					APIcall |> assignMatch()
				)
		},
		"long" = {
			res <- LoR.Metadata |>
				tibble::add_row(
					#
					match_key = match_id,
					server = server,
					# content from the API GET()
					APIcall |> assignMatch()
				) |>
				tidyr::pivot_longer(cols = c(dplyr::ends_with("_1"),dplyr::ends_with("_2"),dplyr::ends_with("_3"),dplyr::ends_with("_4") ),
										 names_to = c(".value"),
										 names_pattern = "(.*)_[0-9]"
				)
			if ( status != 200 ) {
				res |>
					dplyr::distinct()
			} else {
				res |>
					dplyr::filter( !is.na(participants) )
			}
		},
		"text"   = APIcall |> httr::content(as= "text")
	)
}
