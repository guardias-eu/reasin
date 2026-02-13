# Environments and their abbreviations

Shows the valid environments and their abbreviations. The abbreviations
are used in the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by environment. Based on the [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
environments()
```

## Value

A tibble with 4 rows and 2 variables:

- `environment`: The full name of the environment.

- `env_code`: The abbreviation used in the
  [`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
  function.

## See also

Other misc functions:
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md),
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md),
[`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md),
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md),
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)

## Examples

``` r
environments()
#> # A tibble: 4 × 2
#>   environment env_code
#>   <chr>       <chr>   
#> 1 marine      MAR     
#> 2 freshwater  FRW     
#> 3 terrestrial TER     
#> 4 oligohaline OLI     
```
