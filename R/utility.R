local_image <- function(image_path) {
  image <- readBin(logo_path, what = "raw", n = fs::file_info(image_path)$size)

  list(
    type = "inline",
    content = openssl::base64_encode(image)
  )
}

url_image <- function(url) {
  list(
    type = "url",
    url = url
  )
}
