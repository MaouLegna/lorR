#' Setup the api
#'
#' Create a LORAPI_KEY variable that contains the lor-API key.
#' LORAPI_KEY is used as default value for functions which calls the API
#'
#' @param api_key a character, it should have length 42
#'
#' @return a global variable # invisible
#'
#' @export
#'
#' @examples
#' setAPIkey("RGAPI-01234567-abcd-edfg-hijk-01234567890A")
setAPIkey <- function(api_key){
	Sys.setenv(LORAPI_KEY = api_key)
}
