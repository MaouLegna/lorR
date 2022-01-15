#' Return the base path for one of Riot's shards
#'
#' @param server a character, must be one of "americas","apac","europe","asia","sea"
#' Note; asia and sea are going to be removed after the 20th of January 2022
#'
#' @return a character, string containing the url
#' @export
#'
#' @examples
#' get_path_server("europe")
#' get_path_server("apac")
get_path_server <- function(server) {
	shards <- c("americas","apac","europe","asia","sea")
	if ( server %in% shards ) {
		base.url <- base::sprintf("https://%s.api.riotgames.com",server)
	} else {
		warning(base::paste0("Please provide a server among: ",base::paste0(shards,collapse='/')))
		return(NA)
		}
	base.url
}
