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
#' @param environment Character. Environment type(s): one or more of: `"MAR"`,
#'   `"FRW"`, `"TER"`, `"OLI"` to filter species by, marine, freshwater,
#'   terrestrial or oligohaline environments respectively. Use `environments()`
#'   to look up the list of environment codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param country_code Character. Countries' ISO 3166-1 alpha-2 code(s) to
#'   filter species of Member State concern. Use `countries()` to look up the
#'   list of country codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation. Only few
#'   states submitted their species of Member State concern to EASIN.
#' @param region_code Character. Species of Outermost regions concern codes as
#'   defined in NUTS (Nomenclature of territorial units for statistics). Use
#'   `regions()` to look up the list of region codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param impact Character. Species impact(s). One or more of: `"hi"` (high)
#'   and `"lo"` (low).  Use `impacts()` to look up the list of impact codes and
#'   their meaning. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param taxon Character named vector with the taxon name(s)
#'   named by their taxonomic rank(s). Use `ranks()` to look up the list of valid
#'   ranks. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param taxonomy Character named vector with the taxonomic names named
#'   by their taxonomic rank. Provide them in the right order from kingdom up to
#'   family. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param present_in_country Character. One or more countries' ISO 3166-1
#'   alpha-2 codes to filter species present in these countries. Use
#'   `countries()` to look up the list of country codes. Source: EASIN
#'   [Catalogue Web Service](https://easin.jrc.ec.europa.eu/apixg)
#'   documentation.
#' @param status Character. Species status code(s). One or more of:  `"A"`,
#'   `"C"` and `"Q"`. Use `statuses()` to look up the list of status codes and
#'   their meaning. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param horizon Logical. If `TRUE`, returns only species coming from Horizon
#' Scanning assessments. Only `TRUE` is allowed.
#' @param partly_native Logical. If `TRUE`, returns only specise which are
#'   native in one or more EU countries.
#' @param native_in_country Character. One or more countries' ISO 3166-1 alpha-2
#'   codes to filter species native in those countries. Use `countries()` to
#'   look up the list of country codes. Source: EASIN [Catalogue Web
#'   Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @param union_concern Logical. If `TRUE`, returns only species of Union
#' concern. Only `TRUE` is allowed.
#' @return A tibble data frame containing species information.
#' @family species functions
#' @export
#' @examples
#' # Get list of all species in the EASIN catalogue
#' get_species()
#'
#' # Get list of all species of Union concern
#' get_species(union_concern = TRUE)
#'
#' # Get Horizon scanning species
#' get_species(horizon = TRUE)
#'
#' # Get info about one or more species by EASIN Species IDs
#' get_species(easin_id = c("R00460", "R12250"))
#'
#' # Get info about one or more species by scientific names or parts of it
#' get_species(scientific_name = c("Aceria ambrosia", "Procambarus"))
#'
#' # Get species by `environment`
#' get_species(environment = c("MAR","OLI"))
#'
#' # Get species by `country_code`
#' get_species(country_code = c("IE", "LT"))
#'
#' # Get species by `region_code`
#' get_species(region_code = c("ES7", "PT3"))
#'
#' # Get species by `taxon`
#' get_species(taxon = c(family = "Vespidae"))
#'
#' # Get species by full `taxonomy` levels (up to family)
#' get_species(
#'   taxonomy = c(
#'     kingdom = "Animalia",
#'     phylum = "Arthropoda",
#'     class = "Insecta",
#'     order = "Hymenoptera",
#'     family = "Vespidae"
#'   )
#' )
#'
#' # Get species present in one or more countries
#' get_species(present_in_country = c("LU", "IE"))
#'
#' # Get species by `status`
#' get_species(status = c("Q", "A"))
#'
#' # Get species which are native in at least one country
#' get_species(partly_native = TRUE)
#'
#' # Get species which are native in one or more countries
#' get_species(native_in_country = c("EE","FI"))
get_species <- function(
    easin_id = NULL,
    scientific_name = NULL,
    environment = NULL,
    country_code = NULL,
    region_code = NULL,
    impact = NULL,
    taxon = NULL,
    taxonomy = NULL,
    present_in_country = NULL,
    status = NULL,
    horizon = NULL,
    partly_native = NULL,
    native_in_country = NULL,
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
    taxon = taxon,
    taxonomy = taxonomy,
    present_in_country = present_in_country,
    status = status,
    horizon = horizon,
    partly_native = partly_native,
    native_in_country = native_in_country,
    union_concern = union_concern
  )

  # Remove NULL parameters via purrr
  query_params <- purrr::compact(query_params)

  # Get all species if no parameters are provided
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
        "Argument 'environment' must be one or more of: {valid_environments}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_environment(environment))
  }

  # Get species of `union_concern`
  if ("union_concern" %in% names(query_params)) {
    union_concern <- query_params$union_concern
    if (!isTRUE(union_concern)) {
      cli::cli_abort("Argument 'union_concern' must be TRUE")
    }
    return(get_union_concern_species())
  }

  # Get species of Member State concern by `country_code`
  if ("country_code" %in% names(query_params)) {
    country_code <- toupper(query_params$country_code)
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
    region_code <- toupper(query_params$region_code)
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
    if (!purrr::is_character(impact)) {
      cli::cli_abort(
        "Argument 'impact' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_impacts <- c("hi", "lo")
    if (any(!impact %in% valid_impacts)) {
      wrong_impact <- impact[!impact %in% valid_impacts]
      cli::cli_abort(
        "Argument 'impact' must be one or more of: {valid_impacts}. Invalid value{?s}: {wrong_impact}.  Use `impacts()` to get all valid values and their meaning.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_impact(impact))
  }

  # Get species by taxon
  if ("taxon" %in% names(query_params)) {
    taxon <- query_params$taxon
    rank <- names(taxon)
    if (!purrr::is_character(taxon)) {
      cli::cli_abort(
        "Argument 'taxon' must be a named vector.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_ranks <- ranks() %>% dplyr::pull("rank")
    if (!rank %in% valid_ranks) {
      cli::cli_abort(
        "If you want to include a taxonomic level, it must be one of: {.val {valid_ranks}}",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_taxon(rank = rank, taxon = taxon))
  }

  # Get species by full taxonomy
  if ("taxonomy" %in% names(query_params)) {
    taxonomy <- query_params$taxonomy
    rank <- names(taxonomy)
    if (!purrr::is_character(taxonomy, n = 5)) {
      cli::cli_abort(
        "Argument 'taxonomy' must be a named vector of length 5.",
        class = "reasin_error_assignment_invalid"
      )
    }
    ranks <- ranks() %>% dplyr::pull("rank")
    if (!identical(rank, ranks)) {
      cli::cli_abort(
        "If you want to include taxonomic levels, you must include all levels up to family: {.val {ranks}}"
      )
    }
    return(get_species_by_taxonomy(rank = rank, taxonomy = taxonomy))
  }

  # Get species present in a country
  if ("present_in_country" %in% names(query_params)) {
    country <- query_params$present_in_country
    if (!purrr::is_character(country)) {
      cli::cli_abort(
        "Argument 'present_in_country' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_countries <- countries() %>% dplyr::pull(country_code)
    if (any(!country %in% valid_countries)) {
      wrong_countries <- country[!country %in% valid_countries]
      cli::cli_abort(
        "Argument 'present_in_country' must be one or more of: {valid_countries}. Invalid value: {country}.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_presence_in_country(country))
  }

  # Get species via `status`
  if ("status" %in% names(query_params)) {
    status <- toupper(query_params$status)
    if (!purrr::is_character(status)) {
      cli::cli_abort(
        "Argument 'status' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_status <- statuses() %>% dplyr::pull("status_code")
    if (any(!status %in% valid_status)) {
      wrong_status <- status[!status %in% valid_status]
      cli::cli_abort(
        "Argument 'status' must be one or more of: {valid_status}. Invalid value{?s}: {wrong_status}. Use `statuses()` to get all valid values and their meaning.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_species_by_status(status))
  }

  # Get species added to EASIN database based on Horizon Scanning assessments
  if ("horizon" %in% names(query_params)) {
    horizon <- query_params$horizon
    if (!isTRUE(horizon)) {
      cli::cli_abort("Argument 'horizon' must be TRUE.")
    }
    return(get_horizon_scanning_species())
  }

  # Get species native at least in one country
  if ("partly_native" %in% names(query_params)) {
    partly_native <- query_params$partly_native
    if (!isTRUE(partly_native)) {
      cli::cli_abort("Argument 'partly_native' must be TRUE.")
    }
    return(get_partly_native_species())
  }

  # Get species which are native in the given countries
  if ("native_in_country" %in% names(query_params)) {
    native_in_country <- query_params$native_in_country
    if (!purrr::is_character(native_in_country)) {
      cli::cli_abort(
        "Argument 'native_in_country' must be character.",
        class = "reasin_error_assignment_invalid"
      )
    }
    valid_countries <- countries() %>% dplyr::pull(country_code)
    if (any(!native_in_country %in% valid_countries)) {
      wrong_countries <- native_in_country[!native_in_country %in% valid_countries]
      cli::cli_abort(
        "Countr{?y/ies} invalid: {wrong_coutries}. Use `countries()` to get all valid values.",
        class = "reasin_error_assignment_invalid"
      )
    }
    return(get_native_species_in_country(native_in_country))
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
  url_all_species <- "https://easin.jrc.ec.europa.eu/apixg/catxg/getall/skip/0/take/20000"
  data <- get_species_static_url(url_all_species) %>%
    clean_up_names(cols = "Name")
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

#' Get all species based on Horizon Scanning assessments
#'
#' Retrieves all species from the EASIN's Catalogue Web Service that have been
#' assessed in the context of Horizon Scanning. It is used internally by
#' `get_species()` if `horion_scanning = TRUE`.
#' @return A data frame containing all species assessed in the context of
#'   Horizon Scanning.
#' @noRd
#' @examples
#' get_horizon_scanning_species()
get_horizon_scanning_species <- function() {
  horizon_url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/horizon/"
  data <- get_species_static_url(horizon_url, is_pagination = TRUE)
  return(data)
}

#' Get species native in at least one country
#'
#' Retrieves all species from the EASIN's Catalogue Web Service that are native
#' in at least one Member State.
#' @return A data frame containing all species native in at least one country.
#' @noRd
#' @examples
#' get_partly_native_species()
get_partly_native_species <- function() {
  partly_native_url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/partlynative/"
  data <- get_species_static_url(partly_native_url, is_pagination = TRUE)
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
#' @noRd
#' @examples
#' get_species_by_scientific_name("Vespa")
get_species_by_scientific_name <- function(scientific_names) {
  data <- get_species_dynamic_url(
    arg = "term",
    values = scientific_names,
    is_pagination = FALSE
  )
  return(data)
}

#' Get species of Member State concern
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by one or
#' more environment types. It is used internally by `get_species()` if
#' `country_code` argument is provided.
#' @param country_codes A character vector containing one or more ISO 3166-1 alpha-2 country codes.
#' @return A data frame containing species filtered by the specified countries.
#' @noRd
#' @examples
#' get_species_by_country_code(c("AT", "BG"))
get_species_by_country_code <- function(country_codes) {
  # This endpoint accepts multiple country codes comma separated. We save an API
  # call by passing them all at once.
  country_codes <- paste(country_codes, collapse = ",")
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
#' @noRd
#' @examples
#' get_speciesby_region_code("PT3")
get_species_by_region_code <- function(region_codes) {
  # This endpoint accepts multiple region codes comma separated. We save an API
  # call by passing them all at once.
  region_codes <- paste(region_codes, collapse = ",")
  data <- get_species_dynamic_url(
    arg = "concernedregions",
    values = region_codes,
    is_pagination = TRUE
  )
  return(data)
}

#' Get species with a specific impact
#'
#' Retrieves species from the EASIN's Catalogue Web Service It is used
#' internally by `get_species()` if `impact` argument is provided.
#' @param impact A character containing one or more of: `"hi"` (high) or `"lo"` (low).
#' @return A data frame containing species filtered by the specified countries.
#' @noRd
#' @examples
#' get_species_by_country(impact = "lo")
get_species_by_impact <- function(impact) {
  data <- get_species_dynamic_url(
    arg = "impact",
    values = impact,
    is_pagination = TRUE
  )
  return(data)
}

#' Get species by taxon
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by a given
#' `taxon`. It is used internally by `get_species()` if `taxon` argument is
#' provided.
#' @param rank A character representing the taxonomy level. From kingdom up to
#'   family.
#' @param taxon A character string representing the taxonomy name of given
#'   `rank`.
#' @return A tibble data frame containing species information for species
#'  present in the given taxon.
#' @noRd
#' @examples
#' get_species_by_taxon(rank = "family", taxon = "Vespidae")
get_species_by_taxon <- function(rank, taxon) {
  data <- get_species_dynamic_url(
    arg = rank,
    value = taxon,
    is_pagination = TRUE
  )
  return(data)
}

#' Get species by full taxonomy
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by full
#' `taxonomy`. It is used internally by `get_species()` if `taxonomy` argument
#' is provided.
#' @param rank A character representing the taxonomy level(s).
#' @param taxonomy A character string representing the taxonomy name of given `rank`.
#' @return A tibble data frame containing species information for species
#'   present in the given taxonomy.
#' @noRd
#' @examples
#' get_species_by_taxonomy(
#'   rank = c("kingdom", "phylum", "class", "order", "family"),
#'   taxonomy = c(
#'     "Animalia",
#'     "Arthropoda",
#'     "Insecta",
#'     "Hymenoptera",
#'     "Vespidae"
#'   )
#' )
get_species_by_taxonomy <- function(rank, taxonomy) {
  base_url <- "https://easin.jrc.ec.europa.eu/apixg/catxg/"
  data <- get_species_static_url(
    base_url = base_url,
    arg = rank,
    value = taxonomy
  )
  return(data)
}

#' Get species present in a country
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by presence
#' in a give country. It is used internally by `get_species()` if
#' `present_in_country` argument is provided.
#' @param country A character containing one ISO 3166-1 alpha-2 country codes.
#' @return A data frame containing species filtered by the specified countries.
#' @noRd
#' @examples
#' get_species_by_country(present_in_country = "LU")
get_species_by_presence_in_country <- function(countries) {
  # For this endpoint we can pass countrycodes comma separated. We save a API
  # call by passing them all at once.
  countries <- paste(countries, collapse = ",")
  data <- get_species_dynamic_url(
    arg = "incountries",
    values = countries,
    is_pagination = TRUE
  ) %>%
    clean_up_names(cols = "Name")
  return(data)
}

#' Get species via status
#'
#' Retrieves species from the EASIN's Catalogue Web Service filtered by status.
#' It is used internally by `get_species()` if `status` argument is provided.
#' @param status A character vector containing one or more status types.
#' @return A data frame containing species filtered by the specified status
#'   types.
#' @noRd
#' @examples
#' get_species_by_status(c("Q", "C"))
get_species_by_status <- function(status) {
  data <- get_species_dynamic_url(
    arg = "status",
    values = status,
    is_pagination = TRUE
  ) %>%
    clean_up_names(cols = "Name")
  return(data)
}


#' Get native species in one or more countries
#'
#' Retrieves from the EASIN's Catalogue Web Servic the species for countries in
#' which they are native.
#' @param country_codes A character vector containing one or more ISO 3166-1 alpha-2 country codes.
#' @return A data frame containing species filtered by the specified countries.
#' @noRd
#' @examples
#' get_native_species_in_country(c("AT", "BG"))
get_native_species_in_country <- function(countries) {
  # For this endpoint we can pass countrycodes comma separated. We save a API
  # call by passing them all at once.
  countries <- paste(countries, collapse = ",")
  data <- get_species_dynamic_url(
    arg = "nativeincountries",
    values = countries,
    is_pagination = TRUE
  )
  return(data)
}
