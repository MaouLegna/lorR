#' Given the primary key, a puuid, return all the ACCOUNT-V1 info
#' Wraps the \href{"https://developer.riotgames.com/apis#account-v1/GET_getByRiotId"}{GET_getByRiotId} api method and the \href{"https://developer.riotgames.com/apis#account-v1/GET_getActiveShard"}{GET_getActiveShard} api method.
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
#'
#' @examples \dontrun{
#' puuid <- "kJKtE_3i_66edP3lUYSW3wOVxIl5sRKFhsF6IpNIX_RQxYmyBZxG94gNuR4dUe-ofBq_zy5Yll_gSw"
#' get_account_by_puuid(puuid = puuid)
#' }
get_account_by_puuid <- function(puuid) {

	riotdId <- possible_riotId(puuid, "parsed")
	if (is.null(riotdId)) riotdId <- list( "gameName" = NA_character_,
																			 "tagLine"  = NA_character_ )
	shard  <- possible_shard(puuid, "parsed")
	if (is.null(shard)) shard  <- list( "game" = NA_character_,
																			"activeShard"  = NA_character_ )

	list(
		"puuid"    = puuid,
		"gameName" = riotdId$gameName,
		"tagLine"  = riotdId$tagLine,
		"game"        = shard$game,
		"activeShard" = shard$activeShard
	)

}
