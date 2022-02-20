#' Return the base path for one of Riot's shards
#'
#' @param server a character, must be one of americas,europe or sea.
#' Apac and Asia are being removed after the completed Shard merge
#'
#' @return a character, string containing the url
#' @export
#'
#' @examples
#' get_path_server("europe")
#' get_path_server("sea")
get_path_server <- function(server) {
	# shards <- c("americas","apac","europe","asia","sea")
	shards <- c("americas","europe","sea")
	if ( server %in% shards ) {
		base.url <- base::sprintf("https://%s.api.riotgames.com",server)
	} else {
		warning(base::paste0("Please provide a server among: ",base::paste0(shards,collapse='/')))
		return(NA)
		}
	base.url
}
