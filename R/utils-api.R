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
  # Perform the request without throwing an error for HTTP errors at the request
  # stage
  response <- httr2::req_error(request, is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Check for 404 Not Found error and return a specific error
  if (httr2::resp_status(response) == 404) {
    cli::cli_abort(
      "No data found (404 Not Found). Please check the URL: {url}.",
      class = "reasin_error_api_404"
    )
  }
  # Check for other HTTP errors
  if (httr2::resp_is_error(response)) {
    cli::cli_abort(
      "HTTP request failed with status {httr2::resp_status(response)}.",
      class = "reasin_error_api_http"
    )
  }
  # Parse the content (JSON)
  content <- httr2::resp_body_string(response)
  if (!jsonlite::validate(content)) {
    cli::cli_abort(
      "The response content is not valid JSON.",
      class = "reasin_error_api_invalid_json"
    )
  }
  data <- jsonlite::fromJSON(content, flatten = TRUE) |>
    dplyr::tibble()
  if ("jsonlite::fromJSON(content, flatten = TRUE)" %in% names(data)) {
    if (
      "Empty" %in% names(data$`jsonlite::fromJSON(content, flatten = TRUE)`)
    ) {
      # No data: empty tibble returned
      return(dplyr::tibble())
    } else {
      cli::cli_abort(c(
        "Unexpected empty response structure.",
        "i" = "Column{?s} names found: {names(data$`jsonlite::fromJSON(content, flatten = TRUE)`)}",
        class = "reasin_error_api_unexpected_response_empty_result"
      ))
    }
  }
  return(data)
}

#' Get, check and parse paginated HTTP response
#'
#' Retrieves data implementing pagination by iterating over `skip` and `take`.
#' This function is useful when the API limits the number of records returned in
#' a single request and it uses `get_check_parse()` internally.
#'
#' @param url A character string representing the base URL to fetch data from.
#' @param limit Integer. Maximum number of rows to return. Default: `NULL` (return
#'   all rows).
#' @return A tibble data frame containing the aggregated data from all paginated
#'   requests.
#' @noRd
#' @examples
#' get_check_parse_paginated(
#'   "https://easin.jrc.ec.europa.eu/apixg/catxg/env/TER/"
#'  )
get_check_parse_paginated <- function(url, limit = NULL) {
  all_data <- dplyr::tibble()
  skip <- 0L
  take <- 1000L
  if (!is.null(limit) && limit < take) {
    take <- limit
  }
  repeat {
    paginated_url <- glue::glue(
      url, "skip/{skip}/take/{take}",
      skip = skip,
      take = take
    )
    data <- get_check_parse(paginated_url)
    if (nrow(data) == 0) {
      break # No data anymore
    } else {
      all_data <- dplyr::bind_rows(all_data, data)
    }
    # Check if we've reached the limit
    if (!is.null(limit) && nrow(all_data) >= limit) {
      all_data <- dplyr::slice(all_data, 1:limit)
      break
    }
    skip <- skip + take
  }
  return(all_data)
}
