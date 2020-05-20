
#' Create iOS favicon parameters
#'
#' This function helps with the creation of the iOS parameter structure in the JSON request.
#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @param picture_aspect character; one of 'no_change' or 'background_and_margin'.
#' 'no_change' leaves the picture as is, 'background_and_margin' uses the background_color and
#' margin parameters.
#' @param margin character; margin of picture in pixels or as a percentage
#' @param background_color character; background color applied to the background of the Apple Touch icon. Hex value.
#' @param startup_image list; list containing details of startup image to use (created via \code{build_master_picture}).
#' Leave as NULL for no startup image
#' @param app_name character; when defined, will be used by Safari as the default home screen caption instead of page title
#' @param assets list; list of asset parameters. These define which icons to include. Options to be set to TRUE or FALSE
#' are 'ios6_and_prior_icons', 'ios7_and_later_icons', 'precomposed_icons' and 'declare_only_default_icon'.
#' @export
ios_helper <- function(picture_aspect = c("no_change", "background_and_margin"),
                margin = NULL,
                background_colour = NULL,
                startup_image = NULL,
                app_name = NULL,
                assets = list(ios6_and_prior_icons = FALSE,
                              ios7_and_later_icons = TRUE,
                              precomposed_icons = FALSE,
                              declare_only_default_icon = TRUE)) {

  picture_aspect <- match.arg(picture_aspect)

  if (picture_aspect == "background_and_margin" & (is.null(margin) | is.null(background_colour))) {
    error("background and margin parameters must be specified if picture_aspect is 'background_and_margin'")
  }

  remove_null_list(picture_aspect = picture_aspect,
                   margin = margin,
                   background_colour = background_colour,
                   startup_image = startup_image,
                   app_name = app_name,
                   assets = assets
  )
}

#'
#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
windows_helper <- function(picture_aspect = c("no_change", "white_silhouette"),
                    background_colour = list(),
                    assets = list(windows_80_ie_tile = FALSE,
                                  windows_10_ie_11_edge_tiles = list(
                                    small = FALSE,
                                    medium = TRUE,
                                    big = TRUE,
                                    rectange = FALSE
                                  )
                    ),
                    existing_manifest = NULL,
                    on_conflict = c("raise_error", "override", "keep_existing"),
                    app_name = NULL) {

  picture_aspect <- match.arg(picture_aspect)
  on_conflict <- match.arg(on_conflict)

  remove_null_list(picture_aspect = picture_aspect,
                   background_colour = background_colour,
                   assets = assets,
                   existing_manifest = existing_manifest,
                   app_name = app_name)
}


#'
#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
firefox_app_helper <- function(picture_aspect = c("no_change","circle", "rounded_square", "square"),
                        background_colour = NULL,
                        margin = NULL,
                        keep_picture_in_circle = TRUE,
                        circle_inner_margin = NULL,
                        overlay = TRUE,
                        manifest = list(
                          app_name = NULL,
                          app_description = NULL,
                          developer_name = NULL,
                          developer_url = NULL
                        ),
                        existing_manifest = NULL,
                        on_conflict = c("raise_error", "override", "keep_existing")
) {

  picture_aspect <- match.arg(picture_aspect)
  on_conflict <- match.arg(on_conflict)

  if (picture_aspect == "circle" & (is.null(background_colour) | is.null(margin))) {
    error("background_colour and margin parameters must not be null when picture_aspect is 'circle'")
  }

  remove_null_list(picture_aspect = picture_aspect,
                   background_colour = background_colour,
                   margin = margin,
                   keep_picture_in_circle = keep_picture_in_circle,
                   circle_inner_margin = circle_inner_margin,
                   overlay = overlay,
                   manifest = manifest,
                   existing_manifest = existing_manifest)
}


#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
android_chrome_helper <- function(picture_aspect = c("no_change", "background_and_margin", "shadow"),
                           manifest = list(
                             name = "",
                             start_url = NULL,
                             display = c("browser", "standalone"),
                             orientation = c("portrait", "landscape"),
                             theme_color = NULL,
                             existing_manifest = NULL),
                           on_conflict = c("raise_error", "override", "keep_existing"),
                           assets = list(
                             legacy_icon = FALSE,
                             low_resolution_icons = FALSE
                           )
) {
  picture_aspect <- match.arg(picture_aspect)
  on_conflict <- match.arg(on_conflict)
  manifest$display <- match.arg(manifest$display, choices = c("browser", "standalone"))
  if (manifest$display == "standalone") {
    manifest$orientation <- match.arg(manifest$orientation, choices = c("portrait", "landscape"))
  } else {
    manifest$orientation <- NULL
  }

  remove_null_list(picture_aspect = picture_aspect,
                   manifest = manifest,
                   on_conflict = on_conflict,
                   assets = assets)
}


#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
safari_pinned_tab_helper <- function(picture_aspect = c("no_change", "silhouette", "black_and_white"),
                              theme_color = NULL) {
  picture_aspect <- match.arg(picture_aspect)

  remove_null_list(picture_aspect = picture_aspect,
                   theme_color = NULL)
}


#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
coast_helper <- function(picture_aspect = c("no_change", "background_and_margin"),
                  margin = NULL,
                  background_color = NULL) {

  picture_aspect <- match.arg(picture_aspect)

  remove_null_list(picture_aspect = picture_aspect,
                   margin = margin,
                   background_color = background_color)
}

#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
open_graph_helper <- function(picture_aspect = c("no_change", "background_and_margin"),
                       margin = NULL,
                       background_color = NULL,
                       ratio = NULL,
                       site_url = NULL) {
  picture_aspect <- match.arg(picture_aspect)

  remove_null_list(picture_aspect = picture_aspect,
                   margin = margin,
                   background_color = background_color,
                   ratio = ratio,
                   site_url = site_url)
}


#' Further details can be found in the [realfavicongenerator.net API documentation](https://realfavicongenerator.net/api/non_interactive_api)
#' @export
yandex_browser_helper <- function(background_color = NULL,
                           manifest = list(
                             show_title = TRUE,
                             version = 1,
                             existing_manigest = NULL,
                             error_on_override = FALSE)
) {
  remove_null_list(background_color = background_color,
                   manifest = manifest)
}
