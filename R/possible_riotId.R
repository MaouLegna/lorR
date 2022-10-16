#' purrr possible of get_riotId_from_puuid
#' @keywords internal
possible_riotId <- purrr::possibly(.f = get_riotId_from_puuid, otherwise = NULL, quiet = T)
