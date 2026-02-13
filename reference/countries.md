# Countries and country codes

Shows the valid countries and their codes. The codes are used in the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by country. Based on the [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
countries()
```

## Value

A tibble with 2 columns:

- `country`: The full name of the country.

- `country_code`: The abbreviations to be used in the
  `get_species(country_code = )` function.

## See also

Other misc functions:
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md),
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md),
[`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md),
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md),
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)

## Examples

``` r
countries()
#> # A tibble: 28 × 2
#>    country  country_code
#>    <chr>    <chr>       
#>  1 Austria  AT          
#>  2 Belgium  BE          
#>  3 Bulgaria BG          
#>  4 Croatia  HR          
#>  5 Cyprus   CY          
#>  6 Czechia  CZ          
#>  7 Denmark  DK          
#>  8 Estonia  EE          
#>  9 Finland  FI          
#> 10 France   FR          
#> # ℹ 18 more rows
```
