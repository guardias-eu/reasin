#' Get, check and parse HTTP response
#'
#' Get a HTTP response, check it and parse the returned JSON content.
#'
#' @param url A character string representing the URL to fetch data from.
#' @return A tibble containing the parsed JSON data.
#' @noRd
#' @examples
#' url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/getall/skip/0/take/10"
#' get_check_parse(url)
get_check_parse <- function(url) {
  # Make the GET request
  response <- httr::GET(url = url)

  # Check for HTTP errors
  httr::stop_for_status(response)

  # Parse the content (JSON)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(content, flatten = TRUE) %>%
    dplyr::tibble()
}
