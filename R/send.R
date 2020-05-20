#' Send an API request to realfavicongenerator.net
#'
#' Send a preconstructed JSON request (as a list) to the realfavicongenerator.net servers
#' @param json_request list; list to be converted to JSON and sent
#' @return \code{response()} object
#' @export
send_request <- function(json_request) {
  httr::POST("https://realfavicongenerator.net/api/favicon",
             body = json_request,
             encode = "json")
}
