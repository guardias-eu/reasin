# reasin

The goal of reasin is to provide a R interface to the API of the
European Alien Species Information Network (EASIN).

At the moment only interface to the EASIN [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg) (checklist data) is
implemented. Expect more features soon.

## Installation

You can install the development version of reasin from
[GitHub](https://github.com/guardias-eu/reasin) with:

``` r
# install.packages("pak")
pak::pak("guardias-eu/reasin") # dev version
```

## Example

Use
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
to retrieve information on species from the EASIN [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg):

``` r
library(reasin)

# Get overview with all species
get_species()

# Get details for few species via scientific name or part of it
get_species(scientific_name = "Procambarus")
```

More examples can be found in [`get_species()`
documentation](https://guardias-eu.github.io/reasin/reference/get_species.html).

Check also the [Reference
section](https://guardias-eu.github.io/reasin/reference/index.html) for
a list with all available functions.

## Funding

This package is being developed in the framework of the
[GuardIAS](https://guardias.eu/) prject. GuardIAS receives funding from
the European Union’s Horizon Europe Research and Innovation Programme
(ID No 101181413).
