#' Get species from static URL
#'
#' Retrieves a specific set of species from a predefined URL via API. This is t'he
#' case for retrieving all species from the Catalogue Web Service or the s'pecies
#' of Union Concern.
#'
#' @param url A character string representing the URL to fetch species data
#' from.
#' @return A data frame containing the species data retrieved from the s'pecified
#' URL.
#' @keywords internal
#' @noRd
#' @examples
#' get_species_static_url("https://easin.jrc.ec.europa.eu/apixg/catxg/species")
get_species_static_url <- function(url) {
  get_check_parse(url)
}

#' Get species via dynamic URL with query parameters
#'
#' Retrieves species from the EASIN's Catalogue Web Service based on dynamic
#' query parameters. This function is used internally by some subfunctions of
#' `get_species()` when specific filters are applied, e.g. when multiple
#' environments, EASIN IDs or scientific names are provided.
#' @param arg Character with the argument name. One of `"environment"`,
#'   `"easin_id"` or `"term"`.
#' @param values A character vector containing one or more values for the
#' specified argument. If multiple values are passed, iteration and eventually
#'   pagination is handled internally.
#' @param is_pagination A boolean. Is an URL with `skip` and `take` arguments?
#' @return A data frame containing the species data retrieved based on the
#' specified query parameters.
#' @noRd
#' @examples
#' get_species_dynamic_url(
#'   url = "https://easin.jrc.ec.europa.eu/apixg/catxg/easinid/{easin_id}",
#'   arg = "easin_id",
#'   values = c("R12250")
#' )
get_species_dynamic_url <- function(arg, values, is_pagination) {
  valid_args_endpoints <- c(
    "env",
    "easinid",
    "term",
    "concernedms",
    "concernedregions",
    "impact"
  )
  if (!arg %in% valid_args_endpoints) {
    cli::cli_abort(
      "Argument 'arg' must be one of the eindpoints: {valid_args_endpoints}.",
      class = "reasin_error_assignment_invalid"
    )
  }
  if (!is_pagination %in% c(TRUE, FALSE)) {
    cli::cli_abort(
      "Argument 'is_pagination' must be TRUE or FALSE.",
      class = "reasin_error_assignment_invalid"
    )
  }
  data <- purrr::map_df(
    values,
    function(x) {
      url <- glue::glue(
        "https://easin.jrc.ec.europa.eu/apixg/catxg/{arg}/{x}/"
      )
      if (is_pagination == FALSE) {
        get_check_parse(url)
      } else {
        get_check_parse_paginated(url)
      }
    }
  )
  return(data)
}
