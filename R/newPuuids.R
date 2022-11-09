#' Search for new PUUID
#'
#' Check if there are any new PUUID from several game modes (those not against bot) and return a vector of PUUID that are not included
#' "Constructed","SeasonalTournamentLobby","Bo3ChallengeLobby","StandardGauntletLobby","LastCallQualifierGauntletLobby"
#'
#' @param account_tbl a tidy data frame, containing a column called **puuid**
#' @param match_tbl   a tidy data frame, containing column(s) containng **puuid** and one called **game_mode**
#'
#' @return a vector of character, containing the puuid in the matches dataframe not already present in the account data frame
#' @export
#'
#' @examples
#' ex_account = data.frame(
#' 	gameName = c("Pippo","Pluto"),
#' 	puuid = c("defg","wxyz")
#' )
#'
#' ex_match = data.frame(
#' 	game_mode = c("Constructed","Constructed","AI"),
#' 	puuid_1 = c("abcd","defg","mnop"),
#' 	puuid_2 = c("defg","hijk","bot1"),
#' 	puuid_3 = c("wxyz",NA,NA),
#' 	puuid_4 = c(NA,NA,NA)
#' )
#' newPuuids(account_tbl = ex_account, match_tbl = ex_match)
newPuuids <- function(account_tbl, match_tbl) {
	match_tbl |>
		dplyr::filter(game_mode %in% c("Constructed", "SeasonalTournamentLobby","Bo3ChallengeLobby", "StandardGauntletLobby","LastCallQualifierGauntletLobby") ) |>
		dplyr::select(dplyr::contains("puuid")) |>
		tidyr::pivot_longer(cols = dplyr::contains("puuid"),values_to = "puuid") |>
		tidyr::drop_na() |>
		dplyr::filter(!(puuid %in% account_tbl$puuid) ) |>
		dplyr::pull(puuid )
}
