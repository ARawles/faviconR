build_json_request <- function(api_key, master_picture, favicon_design) {
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


build_favicon_design <- function() {
  list(
    "desktop_browser" = list(),
    "ios" = list(
      "picture_aspect" = "no_change",
      "assets" = list(
        "ios6_and_prior_icons" = FALSE,
        "ios7_and_later_icons" = TRUE,
        "precomposed_icons" = FALSE,
        "declare_only_default_icon" = TRUE
      )
    )
  )
}
