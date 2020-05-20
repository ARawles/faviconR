#' Generate favicons from an image
#'
#' Using the realfavicongenerator.net API, this function sends a request to the API with the supplied parameters
#' and downloads the result to a provided save location.
#' @param image character; either a URL to an image or a local path. Type is guessed based on whether the string
#' contains "http/s" or "www", but can you can override this with the \code{override_type} parameter.
#' @param save_loc character; path to a save folder to save the returned images.
#' @param api_key character; NULL to use the API set by \code{set_api_key()} or a character string to override
#' with new value
#' @param favicon_design list; the favicon design parameters as an embedded list that will eventually be converted
#' to json. To help with the building of this complex list, use the \code{build_favicon_design()} function.
#' @param settings list; another embedded list of parameters to be passed to the API. To help with this, use the \code{build_settings()} function.
#' @param versioning list; an embedded list of versionining parameters to be sent to the API. Help creating this
#' is provided with the \code{build_versioning()} function.
#' @return path; the path to the saved file is returned invisibly
#' @export
#' @examples
#' \dontrun{
#' generate_favicon(image = "http://test.image.location", save_loc = "Me/MyDownloads",
#' favicon_design = build_favicon_design(coast = NULL), settings = build_settings(),
#' versioning = build_versioning())
#' }
generate_favicon <- function(image, save_loc, api_key = NULL,
                               override_type = NULL, favicon_design = build_favicon_design(),
                               settings = build_settings(), versioning = build_versioning()) {
  if (is.null(api_key)) {
    api_key <- get_api_key()
  }

  if (is.null(override_type)) {
    if (length(grep(pattern = "http", x = image)) > 0 | length(grep(pattern = "www.", x = image)) > 0) {
      type <- "url"
    } else {
      type <- "local"
    }
  } else {
    type <- override_type
  }

  image_type <- tools::file_ext(image)

  if (image_type != "svg" & !is.null(favicon_design$safari_pinned_tab)) {
    warning("File must be an SVG for safari_pinned_tab favicon. Removing safari_pinned_tab section of request.")
    favicon_design$safari_pinned_tab <- NULL
  }

  json_request <- build_json_request(api_key = api_key,
                     master_picture = build_master_picture(
                       type = type,
                       value = image
                     ),
                     favicon_design = favicon_design,
                     settings = settings,
                     versioning = versioning
  )

  resp <- send_request(json_request)

  if (httr::http_error(resp)) {
    stop(paste0("There was an error. Response code was ", httr::status_code(resp)))
  }

  parsed <- parse_response(resp)

  save_file(parsed$favicon$package_url, save_loc)

}
