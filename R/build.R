build_json_request <- function(api_key, master_picture, favicon_design, settings, versioning) {
  list(
    "favicon_generation" = list(
      "api_key" = api_key,
      "master_picture" = master_picture,
      "favicon_design" = favicon_design
    )
  )
}

build_master_picture <- function(type, value) {
  if (type == "url") {
    url_image(url = value)
  } else if (type == "local") {
    local_image(image_path = value)
  } else {
    stop("Type must be one of 'url' or 'local'")
  }
}


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

build_settings <- function(compression = c(0:5),
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
