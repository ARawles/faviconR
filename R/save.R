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
