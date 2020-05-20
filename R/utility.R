#' Encode a local favicon image to be passed to the API
#'
#' This function takes a local image path and returns a formatted list expected by the JSON request
#' @param image_path character; path to the image
#' @return list; list containing the embedded image and required extra parameters needed for the JSON request
#' @export
local_image <- function(image_path) {
  image <- readBin(image_path, what = "raw", n = fs::file_info(image_path)$size)

  list(
    type = "inline",
    content = openssl::base64_encode(image)
  )
}

#' Provide an image via url to be passed to the API
#'
#' This function takes a url to an image and returns a formatted list needed by the JSON request
#' @param url character; url to image
#' @return list; list containing the url and required extra parameters needed for the JSON request
#' @export
url_image <- function(url) {
  list(
    type = "url",
    url = url
  )
}


#' Create a list and remove NULLs
#'
#' Useful to remove NULL values when creating the favicon_design
#' @param ... values to be added to list
#' @return list; list without NULL values
#' @export
remove_null_list <- function(...) {
  raw_list <- list(...)
  plyr::compact(raw_list)
}
