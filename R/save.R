#' Save the favicon hosted on the realfavicongenerator.net servers
#'
#' @param url character; url of the favicon
#' @param save_loc character; directory to save the favicon to
#' @return path to file is returned invisibly
#' @export
save_file <- function(url, save_loc) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  result <- httr::GET(url, httr::write_disk(tmp))

  tryCatch({
    saved <- utils::unzip(tmp, exdir = save_loc)
  },
  error = function(e) {
    stop("Your logo file couldn't be processed and may be corrupt.", call. = FALSE)
  })

  invisible(saved)
}
