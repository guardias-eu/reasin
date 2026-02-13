# Taxonomic ranks

Shows the valid taxonomic ranks. The ranks are used in the
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
function to filter species by taxonomic rank. Based on the [Catalogue
Web Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

## Usage

``` r
ranks()
```

## Value

A tibble with 1 column:

- `rank`: The valid taxonomic ranks.

## See also

Other misc functions:
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md),
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md),
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md),
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md),
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)

## Examples

``` r
ranks()
#> # A tibble: 5 × 1
#>   rank   
#>   <chr>  
#> 1 kingdom
#> 2 phylum 
#> 3 class  
#> 4 order  
#> 5 family 
```
