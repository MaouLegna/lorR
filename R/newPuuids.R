#' Search for new PUUID
#'
#' Check if there are any new PUUID from several game modes (those not against bot) and return a vector of PUUID that are not included
#' "Constructed","SeasonalTournamentLobby","Bo3ChallengeLobby","StandardGauntletLobby","LastCallQualifierGauntletLobby"
#'
#' @param LoR.Account.df a tidy data frame, containing a column called **puuid**
#' @param LoR.Match.df   a tidy data frame, containing column(s) containng **puuid** and one called **game_mode**
#'
#' @return a vector of character, containing the puuid in the matches dataframe not already present in the account data frame
#' @export
#'
#' @examples
#'
#' Example.Account.df = data.frame(
#' 	gameName = c("Pippo","Pluto"),
#' 	puuid = c("defg","wxyz")
#' )
#'
#' Example.Match.df = data.frame(
#' 	game_mode = c("Constructed","Constructed","AI"),
#' 	puuid_1 = c("abcd","defg","mnop"),
#' 	puuid_2 = c("defg","hijk","bot1"),
#' 	puuid_3 = c("wxyz",NA,NA),
#' 	puuid_4 = c(NA,NA,NA)
#' )
#' newPuuids(LoR.Account.df = Example.Account.df, LoR.Match.df = Example.Match.df)
newPuuids <- function(LoR.Account.df, LoR.Match.df) {
	LoR.Match.df |>
		dplyr::filter(game_mode %in% c("Constructed", "SeasonalTournamentLobby","Bo3ChallengeLobby", "StandardGauntletLobby","LastCallQualifierGauntletLobby") ) |>
		dplyr::select(contains("puuid")) |>
		tidyr::pivot_longer(cols = contains("puuid"),values_to = "puuid") |>
		tidyr::drop_na() |>
		dplyr::filter( !(puuid %in% LoR.Account.df$puuid) ) |>
		dplyr::pull( puuid )
}
