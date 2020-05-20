#' @export
parse_response <- function(response) {
  httr::content(response)$favicon_generation_result
}
