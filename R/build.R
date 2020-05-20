#' Build the complete JSON request
#'
#' Builds the final JSON request from its constituent parts.
#' @param api_key character; API key from realfavicongenerator.net
#' @param master_picture list; list produced by the \code{build_master_picture()} function that
#' includes the image or link to image
#' @param favicon_design list; embedded list containing the favicon design for different platforms, created using the
#' \code{build_favicon_design()} function.
#' @param settings list; embedded list containing extra settings for request. Created using
#' the \code{build_settings()} function
#' @param versioning list; embedded list of versioning parameters. Created using the \code{build_versioning()} function
#' @return list; list containing the complete request to be converted to JSON and posted
#' @export
build_json_request <- function(api_key, master_picture, favicon_design, settings, versioning) {
  remove_empty_list(
    "favicon_generation" = list(
      "api_key" = api_key,
      "master_picture" = master_picture,
      "favicon_design" = favicon_design),

    "settings" = settings,
    "versioning" = versioning
  )
}

#' Build the master picture section of the request
#'
#' This helper function creates the 'master_picture' part of the JSON request that includes the image or link to the image
#' This function is used by the \code{build_json_request} function to create the complete JSON request.
#' @param type string; one of "url" for image urls or "local" for local images.
#' @param value string; either the url if type == "url" or path to local image
#' @return list; valid list form for the complete JSON request
#' @export
build_master_picture <- function(type, value) {
  if (type == "url") {
    url_image(url = value)
  } else if (type == "local") {
    local_image(image_path = value)
  } else {
    stop("Type must be one of 'url' or 'local'")
  }
}


#' Build the favicon design section of the request
#'
#' This function helps with the creation of the 'favicon_design' section of the request. In turn, this function is fed by
#' a number of other helper functions designed for each platform.
#' @param ios list; list of ios parameters. Most easily created using the \code{ios_helper()} function.
#' @param windows list; list of ios parameters. Most easily created using the \code{windows_helper()} function.
#' @param firefox_app list; list of ios parameters. Most easily created using the \code{firefox_app_helper()} function.
#' @param android_chrome list; list of ios parameters. Most easily created using the \code{android_chrome_helper()} function.
#' @param safari_pinned_tab list; list of ios parameters. Most easily created using the \code{safari_pinned_tab_helper()} function.
#' @param coast list; list of ios parameters. Most easily created using the \code{coast_helper()} function.
#' @param open_graph list; list of ios parameters. Most easily created using the \code{open_graph_helper()} function.
#' @param yandex_browser list; list of ios parameters. Most easily created using the \code{yandex_browser_helper()} function.
#' @return list; complete list of favicon parameters for each platform.
#' @export
build_favicon_design <- function(ios = ios_helper(), windows = windows_helper(), firefox_app = firefox_app_helper(),
                                 android_chrome = android_chrome_helper(), safari_pinned_tab = safari_pinned_tab_helper(),
                                 coast = coast_helper(), open_graph = open_graph_helper(), yandex_browser = yandex_browser_helper()) {
  remove_null_list(
    "desktop_browser" = list(),
    "ios" = ios,
    "windows" = windows,
    "firefox_app" = firefox_app,
    "android_chrome" = android_chrome,
    "safari_pinned_tab" = safari_pinned_tab,
    "coast" = coast,
    "open_graph" = open_graph,
    "yandex_browser" = yandex_browser

  )
}

#'
build_settings <- function(compression = c("0", "1", "2", "3", "4", "5"),
                           scaling_algorithm = c("Mitchell", "NearestNeighbor", "Cubic", "Bilinear", "Lanczos", "Spline"),
                           error_on_image_too_small = TRUE,
                           readme_file = FALSE,
                           html_code_file = FALSE,
                           use_path_as_is = FALSE) {
  compression <- match.arg(compression)
  scaling_algorithm <- match.arg(scaling_algorithm)

  list(
    compression = compression,
    scaling_algorithm = scaling_algorithm,
    error_on_image_too_small = error_on_image_too_small,
    readme_file = readme_file,
    html_code_file = html_code_file,
    use_path_as_is = use_path_as_is
  )

}

build_versioning <- function(versioning = FALSE,
                             param_name = NULL,
                             param_value = NULL) {
  if (!versioning) {
    list()
  } else {
    remove_null_list(versioning = versioning,
                     param_name = param_name,
                     param_value = param_value)
  }
}
