#' Get, check and parse HTTP response
#'
#' Get a HTTP response, check it and parse the returned JSON content.
#'
#' @param url A character string representing the URL to fetch data from.
#' @return A tibble data frame containing the parsed JSON data.
#' @noRd
#' @examples
#' url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/getall/skip/0/take/10"
#' get_check_parse(url)
get_check_parse <- function(url) {
  request <- httr2::request(url)
  response <- httr2::req_perform(request)

  # Check for HTTP errors
  httr2::resp_check_status(response)

  # Parse the content (JSON)
  content <- httr2::resp_body_string(response)
  if (!jsonlite::validate(content)) {
    cli::cli_abort(
      "The response content is not valid JSON.",
      class = "reasin_error_api_invalid_json"
    )
  }
  jsonlite::fromJSON(content, flatten = TRUE) %>%
    dplyr::tibble()
}

#' Get, check and parse paginated HTTP response
#'
#' Retrieves data implementing pagination by iterating over `skip` and `take`.
#' This function is useful when the API limits the number of records returned in
#' a single request and it uses `get_check_parse()` internally.
#'
#' @param url A character string representing the base URL to fetch data from.
#' @return A tibble data frame containing the aggregated data from all paginated
#'   requests.
#' @noRd
#' @examples
#' url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/env/TER/skip/{skip}/take/{take}"
#' get_check_parse_paginated(url)
get_check_parse_paginated <- function(url) {
  all_data <- dplyr::tibble()
  skip <- 0
  take <- 1000
  repeat {
    paginated_url <- glue::glue(
      url, "skip/{skip}/take/{take}",
      skip = skip,
      take = take
    )
    data <- get_check_parse(paginated_url)
    if ("." %in% names(data)) {
      if ("Empty" %in% names(data$.)) {
        break # No data anymore
      } else {
        cli::cli_abort(
          "Unexpected response structure: {names(data$.)} column{?s} found.",
          class = "reasin_error_api_unexpected_response"
        )
      }
    }
    all_data <- dplyr::bind_rows(all_data, data)
    skip <- skip + take
  }
  return(all_data)
}
