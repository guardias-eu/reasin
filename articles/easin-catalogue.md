# The EASIN Catalogue

## Introduction

Load reasin with:

``` r

library(reasin)
```

To retrieve species data from the EASIN Catalogue Web Service you need
to use the function
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md).
Behind the screen several API endpoints will be called, depending on the
arguments used. In this article we show what you can do with
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md).

## Overview of all species

The first, basic way to get species data is to call
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
without any arguments. This will retrieve all species from the EASIN
Catalogue:

``` r

get_species()
```

You can limit the number of rows returned by using the `limit` argument.
For example, to get only the first 10 species:

``` r

get_species(limit = 10)
#> # A tibble: 10 × 12
#>    EasinID Name     Authorship LSID  Reference HasImpact IsEUConcern IsMSConcern
#>    <chr>   <chr>    <chr>      <chr> <chr>     <lgl>     <lgl>       <lgl>      
#>  1 R19422  Candida… "Fagen et… urn:… https://… FALSE     FALSE       FALSE      
#>  2 R11525  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  3 R19423  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  4 R19424  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  5 R19425  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  6 R19426  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  7 R19427  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  8 R19428  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#>  9 R11526  Candida… ""         urn:… https://… TRUE      FALSE       FALSE      
#> 10 R19429  Candida… ""         urn:… https://… FALSE     FALSE       FALSE      
#> # ℹ 4 more variables: IsOutermostConcern <lgl>, IsPartNative <lgl>,
#> #   IsHorizonScanning <lgl>, Status <chr>
```

Aside the `EASIN_ID` and columns containing scientific name (`Name` and
`Authorship`), the returned data frame contains columns important for
understanding the species distribution and status in Europe, such as
`HasImpact`, `IsEUConcern` and `IsMSConcern` (is a taxon of concern for
some Member States?), `IsPartNative` (is the taxon native for at least
one Member State?). However, if you want to know more about for example
its taxonomy, the native range, the pathways of introduction and the
environments it inhabits, you need to use
[`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
with specific arguments. So, read on!

## Detailed information for some species

### Information by `easin_id`

The most straightforward way to get species information is to provide
one or more EASIN IDs via the `easin_id` argument:

``` r

some_species <- get_species(easin_id = c("R00460", "R12250"))
some_species
#> # A tibble: 2 × 27
#>   EASINID Name       Authorship FirstIntroductionsInEU PresentInCountries Status
#>   <chr>   <chr>      <chr>      <list>                 <list>             <chr> 
#> 1 R00460  Ailanthus… (Mill.) S… <df [1 × 3]>           <df [54 × 1]>      A     
#> 2 R12250  Procambar… (Girard, … <df [1 × 3]>           <df [24 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <lgl>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <lgl>, IsPartNative <lgl>,
#> #   NativeRange <lgl>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>
```

### Information by scientific name or taxonomy

You can also get species by their scientific name (or part of it) using
the `scientific_name` argument. For example, to get all species whose
scientific name contains `"Aceria ambrosia"` or `"Procambarus"`:

``` r

get_species(scientific_name = c("Aceria ambrosia", "Procambarus"))
#> # A tibble: 4 × 27
#>   EASINID Name       Authorship FirstIntroductionsInEU PresentInCountries Status
#>   <chr>   <chr>      <chr>      <list>                 <list>             <chr> 
#> 1 R16888  Aceria am… Wilson, 1… <df [1 × 3]>           <df [1 × 1]>       A     
#> 2 R12248  Procambar… Girard, 1… <df [1 × 3]>           <df [1 × 1]>       A     
#> 3 R12250  Procambar… (Girard, … <df [1 × 3]>           <df [24 × 1]>      A     
#> 4 R17660  Procambar… Lyko, 2017 <df [1 × 3]>           <df [17 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <lgl>, IsPartNative <lgl>,
#> #   NativeRange <lgl>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>
```

Or you can use `taxon` and provide a character named vector with the
taxon name(s) named by their taxonomic rank(s):

``` r

get_species(taxon = c(order = "Cumacea", family = "Varunidae"))
#> # A tibble: 11 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R17273  Eocuma d… "Fage, 19… <df [1 × 3]>           <df [1 × 1]>       A     
#>  2 R16626  Eocuma r… "Corbera … <NULL>                 <df [1 × 1]>       A     
#>  3 R16627  Eocuma s… "(Kossman… <df [1 × 3]>           <df [5 × 1]>       A     
#>  4 R20899  Nippoleu… "(Gamo, 1… <NULL>                 <df [3 × 1]>       A     
#>  5 R12404  Pseudocu… "Sars, 18… <df [1 × 3]>           <df [2 × 1]>       A     
#>  6 R17543  Pseudocu… "Sars, 18… <df [1 × 3]>           <df [2 × 1]>       A     
#>  7 R16820  Scherocu… "(Calman,… <NULL>                 <df [1 × 1]>       A     
#>  8 R20828  Eriochei… "(De Haan… <NULL>                 <df [1 × 1]>       A     
#>  9 R05836  Eriochei… "H.Milne … <df [1 × 3]>           <df [29 × 1]>      A     
#> 10 R20058  Hemigrap… "(De Haan… <df [1 × 3]>           <df [10 × 1]>      A     
#> 11 R07229  Hemigrap… "Asakura … <df [1 × 3]>           <df [10 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>
```

Use [`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md)
to look up all the ranks you can use:

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

### Information by countries or regions

There are some arguments you can use to get detailed information at
country/regional level:

- `present_in_country`: to get species present in one or more countries,
  probably one of the most useful arguments.
- `native_in_country`: to get species native in one or more countries.
- `country_code`: to get species of **Member State concern** for one or
  more countries. Very few Member States have communicated this
  information to the European Commission, so it could be that you get an
  empty data frame back.
- `region_code`: to get species of **outermost regions concern** for one
  or more regions.

You must use [ISO 3166-1
alpha-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2) country
codes. Still, you don’t have to remember all the country codes: you can
use the
[`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md)
function to get a data frame with all country codes and their
description:

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

Similar for outermost regions, you can use the
[`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md)
function to get the codes and their description:

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

Some examples:

- species present in Luxembourg and Ireland:

``` r

get_species(present_in_country = c("LU", "IE"))
#> # A tibble: 3,603 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00001  Abax par… (Duftschm… <df [1 × 3]>           <df [12 × 1]>      A     
#>  2 R00004  Abgralla… (Signoret… <df [1 × 3]>           <df [5 × 1]>       C     
#>  3 R00005  Abies al… Mill.      <df [1 × 3]>           <df [39 × 1]>      A     
#>  4 R00006  Abies ba… (L.) Mill. <df [1 × 3]>           <df [11 × 1]>      A     
#>  5 R00007  Abies ce… Loudon     <df [1 × 3]>           <df [13 × 1]>      A     
#>  6 R00008  Abies co… (Gordon) … <df [1 × 3]>           <df [16 × 1]>      A     
#>  7 R00011  Abies gr… (Douglas … <df [1 × 3]>           <df [15 × 1]>      A     
#>  8 R00013  Abies no… (Steven) … <df [1 × 3]>           <df [25 × 1]>      A     
#>  9 R00014  Abies pi… Boiss.     <df [1 × 3]>           <df [15 × 1]>      A     
#> 10 R00015  Abies pr… Rehder     <df [1 × 3]>           <df [9 × 1]>       A     
#> # ℹ 3,593 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …
```

- species native in Estonia and Finland:

``` r

get_species(native_in_country = c("EE", "FI"))
#> # A tibble: 1,252 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R19422  Candidat… "Fagen et… <df [1 × 3]>           <df [14 × 1]>      Q     
#>  2 R11526  Candidat… ""         <df [1 × 3]>           <df [29 × 1]>      Q     
#>  3 R00024  Abramis … "(Linnaeu… <df [1 × 3]>           <df [33 × 1]>      A     
#>  4 R00070  Acanthoc… "(Müller,… <df [1 × 3]>           <df [1 × 1]>       C     
#>  5 R00072  Acanthoc… "(Linnaeu… <df [1 × 3]>           <df [28 × 1]>      A     
#>  6 R00074  Acanthoc… "(Fabrici… <df [1 × 3]>           <df [22 × 1]>      A     
#>  7 R00079  Acanthol… "(Linnaeu… <df [1 × 3]>           <df [14 × 1]>      A     
#>  8 R19728  Accipite… "(Linnaeu… <df [1 × 3]>           <df [51 × 1]>      C     
#>  9 R00107  Accipite… "(Linnaeu… <df [1 × 3]>           <df [63 × 1]>      A     
#> 10 R00119  Acer pla… "L."       <df [1 × 3]>           <df [42 × 1]>      A     
#> # ℹ 1,242 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …
```

- species of Member State concern for Ireland and Lithuania:

``` r

get_species(country_code = c("IE", "LT"))
#> # A tibble: 101 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00116  Acer neg… L.         <df [1 × 3]>           <df [44 × 1]>      A     
#>  2 R17242  Alexandr… (Howell) … <df [1 × 3]>           <df [6 × 1]>       A     
#>  3 R00595  Allium t… L.         <df [1 × 3]>           <df [22 × 1]>      A     
#>  4 R17488  Amphibal… Darwin, 1… <df [1 × 3]>           <df [26 × 1]>      C     
#>  5 R01070  Anser an… (Linnaeus… <df [1 × 3]>           <df [56 × 1]>      C     
#>  6 R01255  Aponoget… L.f.       <df [1 × 3]>           <df [9 × 1]>       A     
#>  7 R01414  Arion vu… Moquin-Ta… <df [1 × 3]>           <df [38 × 1]>      A     
#>  8 R01826  Azolla f… Lam.       <df [1 × 3]>           <df [35 × 1]>      A     
#>  9 R18515  Bidens f… Linnaeus,… <df [1 × 3]>           <df [37 × 1]>      A     
#> 10 R02253  Branta c… (Linnaeus… <df [1 × 3]>           <df [40 × 1]>      A     
#> # ℹ 91 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …
```

- species of outermost regions concern for Canary Islands:

``` r

get_species(region_code = c("ES7"))
#> # A tibble: 27 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R01047  Anolis a… Barbour &… <df [1 × 3]>           <df [1 × 1]>       Q     
#>  2 R01048  Anolis c… Voigt, 18… <df [1 × 3]>           <df [2 × 1]>       Q     
#>  3 R01049  Anolis e… Merrem, 1… <df [1 × 3]>           <df [1 × 1]>       Q     
#>  4 R01050  Anolis p… Gray, 1840 <df [1 × 3]>           <df [2 × 1]>       Q     
#>  5 R18148  Anolis s… Duméril &… <df [1 × 3]>           <df [3 × 1]>       Q     
#>  6 R01775  Austrocy… (Lam.) Ba… <df [1 × 3]>           <df [3 × 1]>       A     
#>  7 R03305  Chamaele… Duméril &… <df [1 × 3]>           <df [3 × 1]>       Q     
#>  8 R03485  Chlamydo… Gray, 1825 <df [1 × 3]>           <df [1 × 1]>       Q     
#>  9 R04627  Cyclura … (Bonnater… <df [1 × 3]>           <df [1 × 1]>       Q     
#> 10 R04773  Cytisus … (Hill) Ro… <df [1 × 3]>           <df [11 × 1]>      A     
#> # ℹ 17 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <lgl>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …
```

### Information by environment, impact or status

You can also get species by the environments they inhabit. For example,
to get all species inhabiting freshwater environments:

``` r

freshwater_species <- get_species(environment = "FRW")
head(freshwater_species)
#> # A tibble: 6 × 27
#>   EASINID Name       Authorship FirstIntroductionsInEU PresentInCountries Status
#>   <chr>   <chr>      <chr>      <list>                 <list>             <chr> 
#> 1 R00024  Abramis b… (Linnaeus… <df [1 × 3]>           <df [33 × 1]>      A     
#> 2 R00070  Acanthoce… (Müller, … <df [1 × 3]>           <df [1 × 1]>       C     
#> 3 R00094  Acanthuru… Steindach… <NULL>                 <df [10 × 1]>      A     
#> 4 R00174  Acipenser… Brandt, 1… <df [1 × 3]>           <df [21 × 1]>      A     
#> 5 R00176  Acipenser… Brandt & … <df [1 × 3]>           <df [14 × 1]>      A     
#> 6 R00179  Acipenser… Linnaeus,… <df [1 × 3]>           <df [20 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>
```

How to remember all the environment codes? Not easy, right? Don’t worry:
you can use the
[`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md)
function to get a data frame with all environment codes and their
description:

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

Similar to environments, you can use the
[`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md)
and
[`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)
functions to get the codes and their description related to impact and
status:

``` r

statuses()
#> # A tibble: 3 × 2
#>   status       status_code
#>   <chr>        <chr>      
#> 1 Alien        A          
#> 2 Cryptogenic  C          
#> 3 Questionable Q
```

``` r

impacts()
#> # A tibble: 2 × 2
#>   impact impact_code
#>   <chr>  <chr>      
#> 1 high   hi         
#> 2 low    lo
```

### Information by general criteria

There also are other arguments you can use to get species by more
general criteria:

- `union_concern`: to get species of Union concern. Only `TRUE` is
  allowed.
- `horizon`: to get species coming from Horizon Scanning assessments.
  Only `TRUE` is allowed.
- `partly_native`: to get species which are native in one or more EU
  countries. Only `TRUE` is allowed.

For example, to get all species of Union concern:

``` r

get_species(union_concern = TRUE)
#> # A tibble: 114 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00046  Acacia m… De Wild.   <df [1 × 3]>           <df [8 × 1]>       A     
#>  2 R00053  Acacia s… (Labill.)… <df [1 × 3]>           <df [22 × 1]>      A     
#>  3 R00210  Acridoth… (Linnaeus… <df [1 × 3]>           <df [9 × 1]>       A     
#>  4 R00212  Acridoth… (Linnaeus… <df [1 × 3]>           <df [25 × 1]>      A     
#>  5 R00460  Ailanthu… (Mill.) S… <df [1 × 3]>           <df [54 × 1]>      A     
#>  6 R00644  Alopoche… (Linnaeus… <df [1 × 3]>           <df [39 × 1]>      A     
#>  7 R00669  Alternan… (Mart.) G… <df [1 × 3]>           <df [5 × 1]>       A     
#>  8 R00826  Ameiurus… (Rafinesq… <df [1 × 3]>           <df [30 × 1]>      A     
#>  9 R00994  Andropog… L.         <df [1 × 3]>           <df [3 × 1]>       A     
#> 10 R01506  Arthurde… (Dendy, 1… <df [1 × 3]>           <df [8 × 1]>       A     
#> # ℹ 104 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …
```
