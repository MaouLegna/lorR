#' Return the base path for one of Riot's shards
#'
#' @param server character - the value should be one among: americas,asia,europe,sea
#'
#' @return character string containing the url
#' @export
#'
#' @examples
#' get_path_server("europe")
#' get_path_server("americas")
get_path_server <- function(server) {
	shards <- c("americas","asia","europe","sea")
	if ( server %in% shards ) {
		base.url <- base::sprintf("https://%s.api.riotgames.com",server)
	} else {
		warning(base::paste0("Please provide a server among: ",base::paste0(shards,collapse='/')))
		return(NA)
		}
	base.url
}
