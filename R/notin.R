#' Negate %in%
#'
#' Negate the result on a 'in' operation
#' @export "%!in%"
#' @examples
#' "a" %!in% c("a","b","c")
`%!in%` <- Negate('%in%')
# `%!in%` <- function(x, y) {
# 	x[!(x %in% y)]
# }


