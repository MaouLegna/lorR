#' Return the base path for one of Riot's shards
#'
#' @param server a character, must be one of americas,europe,sea,apac,asia
#' In case of apac or asia che request will be redirected to sea. Old games from asia and apac are no longer available
#'
#' @return a character, string containing the url
#'
#' @examples
#' get_path_server("europe")
#' get_path_server("sea")
get_path_server <- function(server) {

	shards <- c("americas","apac","europe","asia","sea")
	if (server %in% c("asia","apac")) server <- "sea"
	# shards <- c("americas","europe","sea")
	if ( server %in% shards ) {
		base.url <- base::sprintf("https://%s.api.riotgames.com",server)
	} else {
		warning(base::paste0("Please provide a server among: ",base::paste0(shards,collapse='/')))
		return(NA)
		}
	base.url
}
