set_api_key <- function(api_key) {
  Sys.setenv("rfg_api_key" = api_key)
}

unset_api_key <- function() {
    Sys.unsetenv("rfg_api_key")
}

get_api_key <- function() {
  if (Sys.getenv("rfg_api_key") == "") {
    stop("API key has not been set. Use `set_api_key()` to globally set your API key.")
  } else {
    Sys.getenv("rfg_api_key")
  }
}
