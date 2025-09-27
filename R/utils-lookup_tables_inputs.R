#' Environments and their abbreviations
#'
#' Shows the valid environments and their abbreviations. The abbreviations are
#' used in the `get_species()` function to filter species by environment. Based
#' on the [Catalogue Web Service](https://easin.jrc.ec.europa.eu/apixg)
#' documentation.
#' @return A tibble with 4 rows and 2 variables:
#' - `environment`: The full name of the environment.
#' - `env_code`: The abbreviation used in the `get_species()` function.
#' @family misc functions
#' @examples
#' environments()
environments <- function() {
  dplyr::tibble(
    environment = c("marine", "freshwater", "terrestrial", "oligohaline"),
    env_code = c("MAR", "FRW", "TER", "OLI")
  )
}

#' Countries and country codes
#'
#' Shows the valid countries and their codes. The codes are used in the
#' `get_species()` function to filter species by country. Based on the
#' [Catalogue Web Service](https://easin.jrc.ec.europa.eu/apixg) documentation.
#' @return A tibble with 2 columns:
#' - `country`: The full name of the country.
#' - `country_code`: The abbreviations to be used in the `get_species(country_code = )` function.
#' @family misc functions
#' @examples
#' countries()
countries <- function() {
  dplyr::tibble(
    country = c(
      "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
      "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
      "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
      "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
      "Slovenia", "Spain", "Sweden", "UK in respect of Northern Ireland"
    ),
    country_code = c(
      "AT", "BE", "BG", "HR", "CY", "CZ",
      "DK", "EE", "FI", "FR", "DE", "EL",
      "HU", "IE", "IT", "LV", "LT", "LU",
      "MT", "NL", "PL", "PT", "RO",
      "SK", "SI", "ES", "SE", "XI"
    )
  )
}

regions <- function() {
  dplyr::tibble(
    region = c(
      "Canary Islands",
      "Guadeloupe",
      "Martinique",
      "French Guiana",
      "Réunion",
      "Mayotte",
      "Saint Martin",
      "Azores",
      "Madeira"
    ),
    region_code = c(
      "ES7",
      "FRY1",
      "FRY2",
      "FRY3",
      "FRY4",
      "FRY5",
      "FRY6",
      "PT2",
      "PT3"
    )
  )
}
