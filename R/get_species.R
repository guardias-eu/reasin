#' Get species information from the EASIN's Catalogue Web Service
#'
#' This function retrieves species information from the EASIN's
#' [Catalogue](https://easin.jrc.ec.europa.eu/easin/Catalogue). Users can
#' retrieve records by species’ scientific name, environment, impact, taxonomy,
#' Union concern status
#' ([LegalFramework](https://easin.jrc.ec.europa.eu/easin/LegalFramework/Index)). More on [EASIN Web Services](https://easin.jrc.ec.europa.eu/apixg).
#'
#' @param easin_id Integer. EASIN Species ID. If provided, returns info about a
#' single species.
#' @param scientific_name Character. Scientific name or part of it. Case
#' insensitive.
#' @param environment Character. Environment type. One of "Terrestrial",
#' "Aquatic", "Marine", "Freshwater". Case insensitive.
#' @param is_union_concern Logical. If TRUE, returns only species of Union
#'   concern. If FALSE, returns only species not of Union concern. If NULL
#'   (default), returns all species.
#' @export
#' @examples
#' # Get list of all species in the EASIN catalogue
#' get_species()
#'
#' # Get list of all species of Union concern
#' get_species(is_union_concern = TRUE)
#'
#' # Get info about a single species by EASIN Species ID
#' get_species(easin_id = 1234)
#'
#' # Get species by scientific name or part of it
#' get_species(scientific_name = "Ambrosia")
#'
#' # Get species by `environment`
#' get_species(environment = "terrestrial")
get_species <- function(
    easin_id = NULL,
    scientific_name = NULL,
    environment = NULL,
    is_union_concern = NULL
) {
  # Build query parameters
  query_params <- list(
    easin_id = easin_id,
    scientific_name = scientific_name,
    environment = environment,
    is_union_concern = is_union_concern
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

  # If `easin_id` is passed, check it's a character value
  if ("easin_id" %in% names(query_params)) {
    easin_id <- query_params$easin_id
    if (!purrr::is_character(easin_id)) {
      cli::cli_abort("Argument 'easin_id' it must be character.")
    }
  }

  # If `environment` is passed, check it's a boolean

}


#' Get all species
#'
#' This function retrieves all species from the EASIN's Catalogue Web Service.
#' It is used internally by `get_species()` if all args are `NULL`.
#' @return A data frame containing all species.
#' @keywords internal
#' @noRd
get_all_species <- function() {
  # Make the GET request to the EASIN Catalogue Web Service
  response <- httr::GET(
    url = "https://easin.jrc.ec.europa.eu/apixg/catxg/getall/skip/0/take/15000",
  )

  # Check for HTTP errors
  httr::stop_for_status(response)

  # Parse the JSON response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, flatten = TRUE) %>%
    dplyr::tibble()
  # Remobve ' from Name column at the begin and at the end
  data$Name <- gsub("^'|'$", "", data$Name)
  return(data)
}
