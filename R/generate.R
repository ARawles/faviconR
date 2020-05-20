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

  json_request <- build_json_request(api_key = api_key,
                     master_picture = build_master_picture(
                       type = type,
                       value = image
                     ),
                     favicon_design = favicon_design
  )

  resp <- send_request(json_request)

  if (httr::http_error(resp)) {
    error(paste0("There was an error. Response code was ", httr::status_code(resp)))
  }

  parsed <- parse_response(resp)

  save_file(parsed$favicon$package_url, save_loc)

}
