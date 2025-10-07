#' Get species from static URL
#'
#' Retrieves a specific set of species from a predefined URL via API. This is t'he
#' case for retrieving all species from the Catalogue Web Service or the s'pecies
#' of Union Concern.
#'
#' @param base_url A character string representing the base URL of the API.
#' @param arg A character vector representing the specific endpoint to be
#'   appended to the base URL. Useful for full taxonomy endpoint. Default: `NULL`.
#' @param value An optional character vector representing a specific value to be
#'   appended to the URL referring to `arg`. Useful for full taxonomy endpoint.
#'   Default: `NULL`.
#' @param is_pagination A boolean. Is an URL with `skip` and `take` arguments?
#' @return A data frame containing the species data retrieved from the specified
#' URL.
#' @noRd
#' @examples
#' get_species_static_url("https://easin.jrc.ec.europa.eu/apixg/catxg/species")
#' get_species_static_url(
#'   "https://easin.jrc.ec.europa.eu/apixg/catxg/",
#'   arg = c("kingdom", "phylum", "class", "order", "family"),
#'   value = c("Animalia", "Arthropoda", "Insecta", "Hymenoptera", "Vespidae")
#' )
get_species_static_url <- function(
    base_url,
    arg = NULL,
    value = NULL,
    is_pagination = FALSE
  ) {
  # arg and value must be both NULL or both filled in
  if (all(is.null(arg), is.null(value)) == FALSE &
      all(!is.null(arg), !is.null(value)) == FALSE) {
    cli::cli_abort(
      "Arguments 'arg' and 'value' must be both NULL or both filled in.",
      class = "reasin_error_assignment_invalid"
    )
  }
  if (!is.null(arg) & !is.null(value)) {
    valid_arg <- ranks() %>% dplyr::pull(rank)
    if (!identical(arg, valid_arg)) {
      cli::cli_abort(
        "If both 'arg' and 'value' are provided, 'arg' must be {valid_arg}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    url <- paste0(
      base_url,
      glue::glue_collapse(glue::glue("{arg}/{value}/"), sep = "")
    )
  } else {
    url <- base_url
  }
  if (is_pagination == TRUE) {
    get_check_parse_paginated(url)
  } else {
    get_check_parse(url)
  }
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
#' @param base_url A character string representing the base URL of the API.
#' @return A data frame containing the species data retrieved based on the
#' specified query parameters.
#' @noRd
#' @examples
#' get_species_dynamic_url(
#'   url = "https://easin.jrc.ec.europa.eu/apixg/catxg/easinid/{easin_id}",
#'   arg = "easin_id",
#'   values = c("R12250")
#' )
get_species_dynamic_url <- function(
    arg,
    values,
    is_pagination,
    base_url = "https://easin.jrc.ec.europa.eu/apixg/catxg/"
  ) {
  valid_args_endpoints <- c(
    "env",
    "easinid",
    "term",
    "concernedms",
    "concernedregions",
    "impact",
    "incountries",
    "status",
    "nativeincountries",
    ranks() %>% dplyr::pull(rank)
  )
  # Check input is valid based on possible values. Return cli abort error if not
  if (!arg %in% valid_args_endpoints) {
    cli::cli_abort(
      "Argument 'arg' must be one of the eindpoints: {valid_args_endpoints}."
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
