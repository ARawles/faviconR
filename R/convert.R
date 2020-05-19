convert_to_favicon <- function(image, save_loc, api_key = NULL, override_type = NULL) {
  if (is.null(api_key)) {
    api_key <- get_api_key()
  }

  if (is.null(override_type)) {
    if (length(grep(pattern = "http", x = image)) > 0 | length(grep(pattern = "www.", x = image)) > 0) {
      type <- "url"
    } else {
      type <- "local"
    }
  }

  json_request <- build_json_request(api_key = api_key,
                     master_picture = build_master_picture(
                       type = type,
                       value = image
                     ),
                     favicon_design = build_favicon_design(

                     )
  )

  resp <- send_request(json_request)

  parsed <- parse_response(resp)

  save_file(parsed$favicon$package_url, save_loc)

}
