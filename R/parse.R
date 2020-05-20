#' Parse an API response
#'
#' Return the favicon generation result from the \code{response()} object
#' @param response \code{response()} object
#' @return parsed response
#' @export
parse_response <- function(response) {
  httr::content(response)$favicon_generation_result
}
