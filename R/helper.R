
#' Create iOS favicon parameters
#'
#' This function helps with the creation of the iOS parameter structure in the JSON request.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
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

#' Create Windows favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Windows-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param picture_aspect character; one of 'no_change' or 'white_silhouette'. 'no_change' leaves master picture as is, and 'white_silhouette'
#' adds a white silhouette
#' @param background_color character; background color to apply as hex, or NULL for none
#' @param assets list; embedded list for which icons to generate. List is made up of two entries; 'windows_80_ie_tile' which
#' must be TRUE or FALSE. Second entry must be a list named 'windows_10_ie_11_edge_tiles' containing TRUE or FALSE values
#' for each of 'small', 'medium', 'big', 'rectangle'
#' @param existing_manifest list; if there is an existing browserconfig.xml file then this can be passed via this parameter.
#' @param on_conflict character; must be one of 'raise_error', 'override', or 'keep_existing' that defines what to do if the existing
#' manifest contains entries that the API creates
#' @param app_name character; when provided, this will be the app name used on the home screen caption instead of the page title
#' @export
windows_helper <- function(picture_aspect = c("no_change", "white_silhouette"),
                           background_colour = NULL,
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


#' Create Firefox favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Firefox-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param picture_aspect character; one of 'no_change', 'circle', 'rounded_square' or 'square'. If 'circle' is chosen, background_color,
#' margin, keep_picture_in_circle, circle_inner_margin and overlay parameters can all be used.
#' @param manifest list; list of manifest attributes. Should contain app_name; the app's name, app_description; a description of the app,
#' developer_name; the developer name, developer_url; the developer url, existing_manifest; an existing manifest file that can be passed,
#' and on_conflict; what to do if existing manifest conflicts with created manifest
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
                                 developer_url = NULL,
                                 existing_manifest = NULL,
                                 on_conflict = c("raise_error", "override", "keep_existing")
                               )

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
                   manifest = manifest)
}

#' Create Android Chrome favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Android Chrome-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param picture_aspect character; one of 'no_change', 'background_and_margin' or 'shadow'. If 'background_and_margin', then
#' background_color and margin parameters can be used
#' @param margin character; margin in pixels or percentage. only used if picture_aspect is 'background_and_margin'
#' @param background_color character; hex color. Only used if picture_aspect is 'background_and_margin'
#' @param manifest list; list of manifest details. Should be a named list containing name; the application name
#' (this cannot be NULL), start_url; the page that's actually added to the homescreen, display; one of 'browser' where the
#' page is opened in a new Chrome tab or 'standalone' where the website is treated like a new application, orientation;
#' orientation to force the screen if display is standalone, theme_color; color applied to the app when susing task switcher,
#' existing_manifest; if there an existing manifest, it can be provided here, and on_conflict; what to do when there's a conflict
#' between the provided and generated manifest
#' @param assets list; list containing two TRUE/FALSE values: legacy_icon; whether to include a legacy icon, and low_resolution_icons;
#' whether to include all documented icons or just recommended high res icons
#' @export
android_chrome_helper <- function(picture_aspect = c("no_change", "background_and_margin", "shadow"),
                                  margin = NULL,
                                  background_color = NULL,
                                  manifest = list(
                                    name = "",
                                    start_url = NULL,
                                    display = c("browser", "standalone"),
                                    orientation = c("portrait", "landscape"),
                                    theme_color = NULL,
                                    existing_manifest = NULL,
                                    on_conflict = c("raise_error", "override", "keep_existing")
                                  ),
                                  assets = list(
                                    legacy_icon = FALSE,
                                    low_resolution_icons = FALSE
                                  )
) {
  picture_aspect <- match.arg(picture_aspect)
  on_conflict <- match.arg(on_conflict)
  if (exists("manifest")) {
    manifest$display <- match.arg(manifest$display, choices = c("browser", "standalone"))
    if (manifest$display == "standalone") {
      manifest$orientation <- match.arg(manifest$orientation, choices = c("portrait", "landscape"))
    } else {
      manifest$orientation <- NULL
    }
  }

  remove_null_list(picture_aspect = picture_aspect,
                   margin = margin,
                   background_color = background_color,
                   manifest = manifest,
                   assets = assets)
}

#' Create Safari pinned tab favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Safari-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param picture_aspect character; one of 'no_change', 'silhouette' or 'black_and_white'. If 'black_and_white', use threshold to indicate
#' how to turn colours to black/white. If 'no_change', image must be a SVG file.
#' @param threshold character; value of 0 to 100 which determines how colours are changed to black/white
#' @param theme_color character; hex color value of the theme colour - usually the dominant color of the master picture
#' @export
safari_pinned_tab_helper <- function(picture_aspect = c("no_change", "silhouette", "black_and_white"),
                                     threshold = NULL,
                                     theme_color = NULL) {
  picture_aspect <- match.arg(picture_aspect)

  remove_null_list(picture_aspect = picture_aspect,
                   threshold = threshold,
                   theme_color = NULL)
}

#' Create Cost favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Coast-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param picture_aspect character; one of 'no_change' or 'background_and_margin'. If 'background_and_margin', 'margin' and 'background_color'
#' parameters can be used
#' @param margin character; margin in pixels are as a percentage. Only used if 'picture_aspect' is 'background_and_margin'
#' @param background_color; character; hex color string. Only used if 'picture_aspect' is 'background_and_margin'
#' @export
coast_helper <- function(picture_aspect = c("no_change", "background_and_margin"),
                         margin = NULL,
                         background_color = NULL) {

  picture_aspect <- match.arg(picture_aspect)

  remove_null_list(picture_aspect = picture_aspect,
                   margin = margin,
                   background_color = background_color)
}

#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
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

#' Create Yandex favicon parameters
#'
#' This function helps with the creation of the list structure required to provide Yandex-specific favicon parameters
#' to the API.
#' Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
#' @param background_color character; hex color string - background of icon
#' @param manifest list; list of manifest parameters. Should contain 'show_title'; whether to show the title of the page
#' in the bookmark, 'version'; the manifest version (default 1), 'existing_manifest'; existing manigest can be provided
#'  here, 'error_on_override'; whether to raise an error if the manifests conflict
#' @export
yandex_browser_helper <- function(background_color = NULL,
                                  manifest = list(
                                    show_title = TRUE,
                                    version = 1,
                                    existing_manifest = NULL,
                                    error_on_override = FALSE)
) {
  remove_null_list(background_color = background_color,
                   manifest = manifest)
}
