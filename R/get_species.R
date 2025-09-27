#' Get species information from the EASIN's Catalogue Web Service
#'
#' This function retrieves species information from the EASIN's
#' [Catalogue](https://easin.jrc.ec.europa.eu/easin/Catalogue). Users can
#' retrieve records by species’ scientific name, environment, impact, taxonomy,
#' Union concern status
#' ([LegalFramework](https://easin.jrc.ec.europa.eu/easin/LegalFramework/Index)). More on [EASIN Web Services](https://easin.jrc.ec.europa.eu/apixg).
#'
#' @details
#' List of supported Outermost region codes (argument `region_code`):
#' - `ES7`: Canary Islands
#' - `FRY1`: Guadeloupe
#' - `FRY2`: Martinique
#' - `FRY3`: French Guiana
#' - `FRY4`: Réunion
#' - `FRY5`: Mayotte
#' - `FRY6`: Saint Martin
#' - `PT2`: Azores
#' - `PT3`: Madeira
#'
#' @param easin_id Integer. EASIN Species ID(s).
#' @param scientific_name Character. Scientific name(s) or part(s) of it. Case
#' insensitive.
#' @param environment Character. Environment type(s): one or more from `"MAR"`,
#'   `"FRW"`, `"TER"`, `"OLI"` to filter species by, marine, freshwater,
#'   terrestrial or oligohaline environments respectively.
#' @param country_code Character. Countries' ISO 3166-1 alpha-2 code(s) to
#'   filter species of Member State concern. Use `countries()` to look up the
#'   list of codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param region_code Character. Species of Outermost regions concern codes as
#'   defined in NUTS (Nomenclature of territorial units for statistics). Use
#'   `regions()` to look up the list of codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param impact Character. Species impact. One of `"hi"` (high) or `"lo"`
#'   (low).
#' @param union_concern Logical. If `TRUE`, returns only species of Union
#'   concern. Only `TRUE` is allowed.
#' @return A tibble data frame containing species information.
#' @export
#' @examples
#' # Get list of all species in the EASIN catalogue
#' get_species()
#'
#' # Get list of all species of Union concern
#' get_species(union_concern = TRUE)
#'
#' # Get info about one or more species by EASIN Species IDs
#' get_species(easin_id = c("R00460", "R12250"))
#'
#' # Get info about one or more species by scientific names or parts of it
#' get_species(scientific_name = c("Aceria ambrosia", "Procambarus"))
#'
#' # Get species by `environment`
#' get_species(environment = c("MAR","EST"))
#'
#' # Get species by `country_code`
#' get_species(country_code = c("AT"))
#'
#' # Get species by `region_code`
#' get_species(region_code = c("ES7", "PT3"))
get_species <- function(
    easin_id = NULL,
    scientific_name = NULL,
    environment = NULL,
    country_code = NULL,
    region_code = NULL,
    impact = NULL,
    union_concern = NULL
) {
  # Build query parameters
  query_params <- list(
    easin_id = easin_id,
    scientific_name = scientific_name,
    environment = environment,
    country_code = country_code,
    region_code = region_code,
    impact = impact,
    union_concern = union_concern
  )

  # Remove NULL parameters via purrr
  query_params <- purrr::compact(query_params)
  # If no parameters are provided, get all species
  if (length(query_params) == 0) {
    return(get_all_species())
  }

  # `query_params` must be not longer than 1
  if (length(query_params) > 1) {
    no_null_args <- names(query_params)
    cli::cli_abort(
      "Pass one or no argument to `get_species()`: {no_null_args} found."
    )
  }

  # Get species by `union_concern`
  if ("union_concern" %in% names(query_params)) {
    union_concern <- query_params$union_concern
    if (!purrr::is_logical(union_concern, n = 1) | union_concern != TRUE) {
      cli::cli_abort("Argument 'union_concern' must be TRUE")
    }
    return(get_union_concern_species())
  }

  # Get species by `easin_id`
  if ("easin_id" %in% names(query_params)) {
    easin_id <- query_params$easin_id
    if (!purrr::is_character(easin_id)) {
      cli::cli_abort(
        "Argument 'easin_id' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_easin_id(easin_id))
  }

  # Get species by `environment`
  if ("environment" %in% names(query_params)) {
    environment <- query_params$environment
    if (!purrr::is_character(environment)) {
      cli::cli_abort(
        "Argument 'environment' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_environments <- environments() %>% dplyr::pull("env_code")
    if (any(!environment %in% valid_environments)) {
      wrong_environments <- environment[!environment %in% valid_environments]
      cli::cli_abort(
        "Argument 'environment' must be one of: {valid_environments}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_environment(environment))
  }

  # Get species by `scientific_name`
  if ("scientific_name" %in% names(query_params)) {
    scientific_name <- query_params$scientific_name
    if (!purrr::is_character(scientific_name)) {
      cli::cli_abort(
        "Argument 'scientific_name' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    # Length of each scientific name must be at least 4 characters
    if (any(nchar(scientific_name) < 4)) {
      short_names <- scientific_name[nchar(scientific_name) < 4]
      cli::cli_abort(
        "Each scientific name must be at least 4 characters long. Short names: {short_names}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    # Replace spaces with `%20`
    scientific_name <- gsub(" ", "%20", scientific_name)
    return(get_species_by_scientific_name(scientific_name))
  }

  # Get species by `country_code`
  if ("country_code" %in% names(query_params)) {
    country_code <- query_params$country_code
    if (!purrr::is_character(country_code)) {
      cli::cli_abort(
        "Argument 'country_code' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_countries <- countries() %>% dplyr::pull("country_code")
    if (any(!country_code %in% valid_countries)) {
      wrong_countries <- country_code[!country_code %in% valid_countries]
      cli::cli_abort(
        "Countr{?y/ies} invalid: {wrong_coutries}. Use `countries()` to get all valid values.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_country_code(country_code))
  }

  # Get species by `region_code`
  if ("region_code" %in% names(query_params)) {
    region_code <- query_params$region_code
    if (!purrr::is_character(region_code)) {
      cli::cli_abort(
        "Argument 'region_code' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_regions <- regions() %>% dplyr::pull("region_code")
    if (any(!region_code %in% valid_regions)) {
      wrong_regions <- region_code[!region_code %in% valid_regions]
      cli::cli_abort(
        "Region{?s} code{?s} invalid: {wrong_regions}. Use `regions()` to get all valid values.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_region_code(region_code))
  }

  # Get species by `impact`
  if ("impact" %in% names(query_params)) {
    impact <- query_params$impact
    if (!purrr::is_character(impact, n = 1)) {
      cli::cli_abort(
        "Argument 'impact' must be character of length 1.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_impacts <- c("hi", "lo")
    if (!impact %in% valid_impacts) {
      cli::cli_abort(
        "Argument 'impact' must be one of: {valid_impacts}. Invalid value: {impact}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_impact(impact))
  }
}


#' Get all species
#'
#' This function retrieves all species from the EASIN's Catalogue Web Service.
#' It is used internally by `get_species()` if all args are `NULL`.
#' @return A tibble data frame containing all species.
#' @noRd
#' @examples
#' get_all_species()
get_all_species <- function() {
  url_all_species <- "https://easin.jrc.ec.europa.eu/apixg/catxg/getall/skip/0/take/15000"
  data <- get_species_static_url(url_all_species)
  data <- clean_up_names(data, cols = "Name")
  return(data)
}

#' Get all species of Union Concern
#'
#' Retrieves all species of Union Concern from the EASIN's Catalogue Web Service.
#' It is used internally by `get_species()` if `union_concern = TRUE`.
#'
#' @return A data frame containing all species of Union Concern.
#' @noRd
#' @examples
#' get_union_concern_species()
get_union_concern_species <- function() {
  union_concern_url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/euconcern"
  data <- get_species_static_url(union_concern_url)
  return(data)
}

#' Get species by environment(s)
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by one or
#' more environment types. It is used internally by `get_species()` if
#' `environment` argument is provided.
#' @param environments A character vector containing one or more environment
#' types.
#' @return A data frame containing species filtered by the specified environment
#' types.
#' @noRd
#' @examples
#' get_species_by_environment(c("MAR", "TER"))
get_species_by_environment <- function(environments) {
  data <- get_species_dynamic_url(
    arg = "env",
    values = environments,
    is_pagination = TRUE
  )
  data <- clean_up_names(data, cols = "Name")
  return(data)
}

#' Get species by EASIN ID(s)
#'
#' Retrieves species information from the EASIN's Catalogue Web Service for one
#' or more EASIN IDs. It is used internally by `get_species()` if `easin_id`
#' argument is provided.
#'
#' @param easin_ids A character vector containing one or more EASIN IDs.
#' @return A tibble data frame containing species information.
#' @noRd
#' @examples
#' get_species_by_easin_id(c("R00460", "R12250"))
get_species_by_easin_id <- function(easin_ids) {
  data <- get_species_dynamic_url(
    arg = "easinid",
    values = easin_ids,
    is_pagination = FALSE
  )
  return(data)
}

#' Get species by scientific name or part of it
#'
#' Retrieves species from the EASIN's Catalogue Web Service based on a
#' scientific name or part of it. It is used internally by `get_species()` if
#' `scientific_name` argument is provided.
#'
#' @param scientific_names A character vector containing one or more scientific
#' names or parts of it.
#' @return A tibble data frame containing species information.
get_species_by_scientific_name <- function(scientific_names) {
  data <- get_species_dynamic_url(
    arg = "term",
    values = scientific_names,
    is_pagination = FALSE
  )
  return(data)
}

#' Get species by country
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by one or
#' more environment types. It is used internally by `get_species()` if
#' `environment` argument is provided.
#' @param country_codes A character vector containing one or more ISO 3166-1 alpha-2 country codes.
#' @return A data frame containing species filtered by the specified countries.
#' @noRd
#' @examples
#' get_species_by_country_code(c("AT", "BG"))
get_species_by_country_code <- function(country_codes) {
  data <- get_species_dynamic_url(
    arg = "concernedms",
    values = country_codes,
    is_pagination = TRUE
  )
  return(data)
}

#' Get species by Outermost region code(s)
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by one or
#' more Outermost region codes. It is used internally by `get_species()` if
#' `region_code` argument is provided.
#' @param region_codes A character vector containing one or more Outermost region codes.
#' @return A tibble data frame containing species information for species present in the given Outermost regions.
get_species_by_region_code <- function(region_codes) {
  data <- get_species_dynamic_url(
    arg = "concernedregions",
    values = region_codes,
    is_pagination = TRUE
  )
  return(data)
}

get_species_by_impact <- function(impact) {
  data <- get_species_dynamic_url(
    arg = "impact",
    values = impact,
    is_pagination = TRUE
  )
  return(data)
}
