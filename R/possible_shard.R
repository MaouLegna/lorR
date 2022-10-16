#' purrr possible of get_shard_from_puuid
#' @keywords internal
possible_shard <- purrr::possibly(.f = get_shard_from_puuid, otherwise = NULL, quiet = T)
