#' Get species information from the EASIN's Catalogue Web Service
#'
#' This function retrieves species information from the EASIN's
#' [Catalogue](https://easin.jrc.ec.europa.eu/easin/Catalogue). Users can
#' retrieve records by species’ scientific name, environment, impact, taxonomy,
#' Union concern status
#' ([LegalFramework](https://easin.jrc.ec.europa.eu/easin/LegalFramework/Index)). More on [EASIN Web Services](https://easin.jrc.ec.europa.eu/apixg).
#'
#' @param easin_id Integer. EASIN Species ID(s).
#' @param scientific_name Character. Scientific name(s) or part(s) of it. Case
#' insensitive.
#' @param environment Character. Environment type(s): one or more from `"MAR"`,
#'   `"FRW"`, `"TER"`, `"OLI"` to filter species by, marine, freshwater,
#'   terrestrial or oligohaline environments respectively.
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
get_species <- function(
    easin_id = NULL,
    scientific_name = NULL,
    environment = NULL,
    union_concern = NULL
) {
  # Build query parameters
  query_params <- list(
    easin_id = easin_id,
    scientific_name = scientific_name,
    environment = environment,
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

  # If `union_concern` is passed, check it's a boolean
  if ("union_concern" %in% names(query_params)) {
    union_concern <- query_params$union_concern
    if (!purrr::is_logical(union_concern, n = 1) | union_concern != TRUE) {
      cli::cli_abort("Argument 'union_concern' must be TRUE")
    }
    return(get_union_concern_species())
  }

  # If `easin_id` is passed, check it's a character value
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

  # If `environment` is passed, check it's a valid character value
  if ("environment" %in% names(query_params)) {
    environment <- query_params$environment
    if (!purrr::is_character(environment)) {
      cli::cli_abort(
        "Argument 'environment' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_environments <- c("MAR", "FRW", "TER", "OLI")
    if (any(!environment %in% valid_environments)) {
      wrong_environments <- environment[!environment %in% valid_environments]
      cli::cli_abort(
        "Argument 'environment' must be one of: {valid_environments}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_environment(environment))
  }

  # If `scientific_name` is passed, check it's a character value
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
