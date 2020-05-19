send_request <- function(json_request) {
  httr::POST("https://realfavicongenerator.net/api/favicon",
             body = json_request,
             encode = "json")
}
