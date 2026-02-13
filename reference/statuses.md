# Species statuses

Shows the valid species statuses. The statuses are used in the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by status. Based on the [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
statuses()
```

## Value

A tibble with 2 columns:

- `status`: The full name of the status.

- `status_code`: The abbreviations to be used in the
  `get_species(status = )` function.

## See also

Other misc functions:
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md),
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md),
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md),
[`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md),
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md)

## Examples

``` r
statuses()
#> # A tibble: 3 × 2
#>   status       status_code
#>   <chr>        <chr>      
#> 1 Alien        A          
#> 2 Cryptogenic  C          
#> 3 Questionable Q          
```
