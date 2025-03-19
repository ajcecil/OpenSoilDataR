#' Check if a File Exists on Server
#'
#' Sends an HTTP HEAD request to check if a file is available at the specified URL.
#'
#' @param url Character. The URL of the file.
#' @return Logical. `TRUE` if the file exists, `FALSE` otherwise.
#' @importFrom httr HEAD status_code
#' @export
file_exists_on_server <- function(url) {
  response <- HEAD(url)
  return(status_code(response) == 200)
}

#' Download with Timeout Handling
#'
#' Attempts to download a file. If `download.file()` fails or takes too long, it falls back to `httr::GET()`.
#'
#' @param url Character. The URL of the file to download.
#' @param destfile Character. The destination file path.
#' @param timeout Numeric. Maximum time (in seconds) before switching methods.
#' @return Downloads the file or throws an error.
#' @importFrom httr GET write_disk status_code timeout
#' @export
download_with_fallback <- function(url, destfile, timeout = 55) {
  tryCatch({
    download.file(url, destfile, mode = "wb", timeout = timeout)
    message("Downloaded successfully: ", destfile)
  }, error = function(e) {
    message("Download timed out or failed for: ", url, " | Switching to httr::GET()")

    response <- GET(url, write_disk(destfile, overwrite = TRUE), timeout(300))

    if (status_code(response) == 200) {
      message("Downloaded successfully with httr::GET(): ", destfile)
    } else {
      message("Failed to download with both methods: ", destfile, " | HTTP status: ", status_code(response))
    }
  })
}
