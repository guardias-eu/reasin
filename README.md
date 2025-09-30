
# reasin

<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/reasin)](https://CRAN.R-project.org/package=reasin)
[![R-CMD-check](https://github.com/guardias-eu/reasin/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/guardias-eu/reasin/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/guardias-eu/reasin/graph/badge.svg)](https://app.codecov.io/gh/guardias-eu/reasin)
[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of reasin is to provide a R interface to the API of the European Alien Species Information Network (EASIN).

At the moment only interface to the Catalogue Web Service (checklist data) is implemented. Expect more features soon.

## Installation

You can install the development version of reasin from [GitHub](https://github.com/guardias-eu/reasin) with:

``` r
# install.packages("pak")
pak::pak("guardias-eu/reasin") # dev version
```

## Example

Use `get_species()` to retrieve information on species from the EASIN [Catalogue
Web Service](https://easin.jrc.ec.europa.eu/apixg):

``` r
library(reasin)

# Get overview with all species
get_species()

# Get details for few species via scientific name or part of it
get_species(scientific_name = "Procambarus")
```

More examples can be found in [`get_species()` documentation](https://guardias-eu.github.io/reasin/reference/get_species.html).

Check also the [Reference
section](https://guardias-eu.github.io/reasin/reference/index.html) for a list
with all available functions.
