#' url_exists
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
#' @return
#' @export
#' @examples
#' \dontrun{
#' c("http://google.com","http://atmo-na.org") -> some_urls
#' data.frame(
#'  exists = sapply(some_urls, url_exists, USE.NAMES = FALSE),
#'  some_urls,
#'  stringsAsFactors = FALSE
#' ) %>% dplyr::tbl_df() %>% print()
#' }
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...)
{

  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })

  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, ...)

    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}
