#' Convert LoR-API call to a wide tibble
#'
#' @param APIcall - a REST API response
#'
#' @return a dataframe with the metadata of a match
#'
assignMatch <- function(APIcall) {

	out <- tryCatch({

		status  <- httr::status_code(APIcall)
		res <- base::data.frame(data_version = character(),
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

														puuid_1     = character(), # PLAYER_1
														deck_id_1   = character(),
														deck_code_1 = character(),
														factions_1  = character(),
														game_outcome_1  = character(),
														order_of_play_1 = numeric(),
														puuid_2     = character(), # PLAYER_2
														deck_id_2   = character(),
														deck_code_2 = character(),
														factions_2 = character(),
														game_outcome_2  = character(),
														order_of_play_2 = numeric(),
														puuid_3     = character(), # PLAYER_3
														deck_id_3   = character(),
														deck_code_3 = character(),
														factions_3 = character(),
														game_outcome_3  = character(),
														order_of_play_3 = numeric(),
														puuid_4     = character(), # PLAYER_4
														deck_id_4   = character(),
														deck_code_4 = character(),
														factions_4 = character(),
														game_outcome_4  = character(),
														order_of_play_4 = numeric(),

														total_turn_count = numeric(),
														status = numeric()
		)

		res <- tibble::add_row(res)
		# base::print(res)

		metadataList <- APIcall$content |> base::rawToChar() |> jsonlite::fromJSON()
		# base::print(metadataList)

		res[1,"status"] <- status

		if (status==200){
			res[1,c("data_version")] <- metadataList$metadata$data_version
			res[1,c("match_id")]     <- metadataList$metadata$match_id

			nPartecipant <- base::NROW(metadataList$metadata$participants)

			res[1,c("participants_1","participants_2","participants_3","participants_4")][,1:nPartecipant] <- metadataList$metadata$participants |> base::as.list()
			res[1,c("game_mode","game_type","game_start_time_utc","game_version")] <- c(metadataList$info$game_mode,metadataList$info$game_type,metadataList$info$game_start_time_utc,metadataList$info$game_version) |>
				base::as.list()
			res[1,"total_turn_count"] <- metadataList$info$total_turn_count

			nPlayer <- base::NROW(metadataList$info$players)

			if (nPlayer >= 1 ) { res[1,c("puuid_1","deck_id_1","deck_code_1","factions_1","game_outcome_1","order_of_play_1")] <- assignPlayer(metadataList$info$players[1,]) |> base::as.list() }
			if (nPlayer >= 2 ) { res[1,c("puuid_2","deck_id_2","deck_code_2","factions_2","game_outcome_2","order_of_play_2")] <- assignPlayer(metadataList$info$players[2,]) |> base::as.list() }
			if (nPlayer >= 3 ) { res[1,c("puuid_3","deck_id_3","deck_code_3","factions_3","game_outcome_3","order_of_play_3")] <- assignPlayer(metadataList$info$players[3,]) |> base::as.list() }
			if (nPlayer == 4 ) { res[1,c("puuid_4","deck_id_4","deck_code_4","factions_4","game_outcome_4","order_of_play_4")] <- assignPlayer(metadataList$info$players[4,]) |> base::as.list() }
		}
		res |> tibble::as_tibble()
	},
	warning=function(w) {
		base::print(w)
		stop("assignMatch: converted from warning",base::conditionMessage(w))
	},
	error=function(e) {
		base::message("assignMatch: Here's the original error message: \n")
		base::print(e)
		base::message(e)
		return(NULL)
	})
	return(out)
}
