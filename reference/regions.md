# Outermost regions

Shows the valid outermost regions and their codes. The codes are used in
the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by outermost regions. Based on the [Catalogue
Web Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
regions()
```

## Value

A tibble with 2 columns:

- `region`: The full name of the outermost region.

- `region_code`: The abbreviations to be used in the
  `get_species(region_code = )` function.

## See also

Other misc functions:
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md),
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md),
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md),
[`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md),
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)

## Examples

``` r
regions()
#> # A tibble: 9 × 2
#>   region         region_code
#>   <chr>          <chr>      
#> 1 Canary Islands ES7        
#> 2 Guadeloupe     FRY1       
#> 3 Martinique     FRY2       
#> 4 French Guiana  FRY3       
#> 5 Reunion        FRY4       
#> 6 Mayotte        FRY5       
#> 7 Saint Martin   FRY6       
#> 8 Azores         PT2        
#> 9 Madeira        PT3        
```
