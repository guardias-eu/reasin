# Impact

Shows the valid impact levels. The impact levels are used in the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by impact. Based on the [Catalogue Web
Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
impacts()
```

## Value

A tibble with 2 columns:

- `impact`: The full name of the impact level.

- `impact_code`: The abbreviation to be used in the
  `get_species(impact = )` function.

## See also

Other misc functions:
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md),
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md),
[`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md),
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md),
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)

## Examples

``` r
impacts()
#> # A tibble: 2 × 2
#>   impact impact_code
#>   <chr>  <chr>      
#> 1 high   hi         
#> 2 low    lo         
```
