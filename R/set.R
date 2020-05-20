
#' Set your realfavicongenerator API key globally
#'
#' By setting your API with this function, you don't need to set it each time you make a request.
#' The API can be overridden on specific requests by providing a new key when using the \code{generate_favicon()} function.
#'
#' @param api_key character; API key as a character string. Retrieved from realfavicongenerator.net
#' @export
#' @examples
#' set_api_key("1234abcd")
set_api_key <- function(api_key) {
  Sys.setenv("rfg_api_key" = api_key)
}


#' Unsets the global API key
#' @export
#' @examples
#' unset_api_key()
unset_api_key <- function() {
    Sys.unsetenv("rfg_api_key")
}

#' Retrieve a previously set API key
#'
#' This function returns the value of the API key set at the system environment level.
#' It will error if the key hasn't been set.
#' @export
#' @examples
#' set_api_key("123abc")
#' get_api_key()
get_api_key <- function() {
  if (Sys.getenv("rfg_api_key") == "") {
    stop("API key has not been set. Use `set_api_key()` to globally set your API key.")
  } else {
    Sys.getenv("rfg_api_key")
  }
}
