#' Convert LoR-API call player field to a wide tibble
#'
#' @param playerMetadata
#'
#' @return
#' @export
#'
#' @examples
#' NA
assignPlayer <- function(playerMetadata) {
	player.res <- base::data.frame(puuid     = character(),
																 deck_id   = character(),
																 deck_code = character(),
																 factions  = character(),
																 game_outcome  = character(),
																 order_of_play = numeric()
																 )
	player.res <- tibble::add_row(player.res)

	regions <- base::paste(base::unlist(playerMetadata$factions),collapse = ",")
	nfactions <- base::length(regions)

	player.res[1,c("puuid","deck_id","deck_code","game_outcome","order_of_play")] <- playerMetadata[-4] |> base::as.list()
	player.res[1,"factions"] <- regions

	player.res |> tibble::as_tibble()
}
