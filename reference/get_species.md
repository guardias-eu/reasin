# Get species information from the EASIN's Catalogue Web Service

This function retrieves species information from the EASIN's
[Catalogue](https://easin.jrc.ec.europa.eu/easin/Catalogue). Users can
retrieve records by species’ scientific name, environment, impact,
taxonomy, Union concern status
([LegalFramework](https://easin.jrc.ec.europa.eu/easin/LegalFramework/Index)).
More on [EASIN Web Services](https://easin.jrc.ec.europa.eu/apixg).

## Usage

``` r
get_species(
  easin_id = NULL,
  scientific_name = NULL,
  environment = NULL,
  country_code = NULL,
  region_code = NULL,
  impact = NULL,
  taxon = NULL,
  taxonomy = NULL,
  present_in_country = NULL,
  status = NULL,
  horizon = NULL,
  partly_native = NULL,
  native_in_country = NULL,
  union_concern = NULL
)
```

## Arguments

- easin_id:

  Integer. EASIN Species ID(s).

- scientific_name:

  Character. Scientific name(s) or part(s) of it. Case insensitive.

- environment:

  Character. Environment type(s): one or more of: `"MAR"`, `"FRW"`,
  `"TER"`, `"OLI"` to filter species by, marine, freshwater, terrestrial
  or oligohaline environments respectively. Use
  [`environments()`](https://guardias-eu.github.io/reasin/reference/environments.md)
  to look up the list of environment codes. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- country_code:

  Character. Countries' ISO 3166-1 alpha-2 code(s) to filter species of
  Member State concern. Use
  [`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md)
  to look up the list of country codes. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation. Only few
  states submitted their species of Member State concern to EASIN.

- region_code:

  Character. Species of Outermost regions concern codes as defined in
  NUTS (Nomenclature of territorial units for statistics). Use
  [`regions()`](https://guardias-eu.github.io/reasin/reference/regions.md)
  to look up the list of region codes. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- impact:

  Character. Species impact(s). One or more of: `"hi"` (high) and `"lo"`
  (low). Use
  [`impacts()`](https://guardias-eu.github.io/reasin/reference/impacts.md)
  to look up the list of impact codes and their meaning. Source: EASIN
  [Catalogue Web Service](https://easin.jrc.ec.europa.eu/apixg)
  documentation.

- taxon:

  Character named vector with the taxon name(s) named by their taxonomic
  rank(s). Use
  [`ranks()`](https://guardias-eu.github.io/reasin/reference/ranks.md)
  to look up the list of valid ranks. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- taxonomy:

  Character named vector with the taxonomic names named by their
  taxonomic rank. Provide them in the right order from kingdom up to
  family. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- present_in_country:

  Character. One or more countries' ISO 3166-1 alpha-2 codes to filter
  species present in these countries. Use
  [`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md)
  to look up the list of country codes. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- status:

  Character. Species status code(s). One or more of: `"A"`, `"C"` and
  `"Q"`. Use
  [`statuses()`](https://guardias-eu.github.io/reasin/reference/statuses.md)
  to look up the list of status codes and their meaning. Source: EASIN
  [Catalogue Web Service](https://easin.jrc.ec.europa.eu/apixg)
  documentation.

- horizon:

  Logical. If `TRUE`, returns only species coming from Horizon Scanning
  assessments. Only `TRUE` is allowed.

- partly_native:

  Logical. If `TRUE`, returns only specise which are native in one or
  more EU countries.

- native_in_country:

  Character. One or more countries' ISO 3166-1 alpha-2 codes to filter
  species native in those countries. Use
  [`countries()`](https://guardias-eu.github.io/reasin/reference/countries.md)
  to look up the list of country codes. Source: EASIN [Catalogue Web
  Service](https://easin.jrc.ec.europa.eu/apixg) documentation.

- union_concern:

  Logical. If `TRUE`, returns only species of Union concern. Only `TRUE`
  is allowed.

## Value

A tibble data frame containing species information.

## Examples

``` r
# Get list of all species in the EASIN catalogue
get_species()
#> # A tibble: 15,686 × 12
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
#> # ℹ 15,676 more rows
#> # ℹ 4 more variables: IsOutermostConcern <lgl>, IsPartNative <lgl>,
#> #   IsHorizonScanning <lgl>, Status <chr>

# Get list of all species of Union concern
get_species(union_concern = TRUE)
#> # A tibble: 114 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00046  Acacia m… De Wild.   <df [1 × 3]>           <df [8 × 1]>       A     
#>  2 R00053  Acacia s… (Labill.)… <df [1 × 3]>           <df [22 × 1]>      A     
#>  3 R00210  Acridoth… (Linnaeus… <df [1 × 3]>           <df [9 × 1]>       A     
#>  4 R00212  Acridoth… (Linnaeus… <df [1 × 3]>           <df [24 × 1]>      A     
#>  5 R00460  Ailanthu… (Mill.) S… <df [1 × 3]>           <df [52 × 1]>      A     
#>  6 R00644  Alopoche… (Linnaeus… <df [1 × 3]>           <df [38 × 1]>      A     
#>  7 R00669  Alternan… (Mart.) G… <df [1 × 3]>           <df [4 × 1]>       A     
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

# Get Horizon scanning species
get_species(horizon = TRUE)
#> # A tibble: 79 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00083  Acanthop… (M.Vahl) … <NULL>                 <df [1 × 1]>       A     
#>  2 R00174  Acipense… Brandt, 1… <df [1 × 3]>           <df [21 × 1]>      A     
#>  3 R20137  Aeolesth… (Solsky, … <NULL>                 <NULL>             A     
#>  4 R20138  Agrilus … (Gory, 18… <NULL>                 <df [2 × 1]>       A     
#>  5 R20139  Agrilus … (Schaeffe… <NULL>                 <NULL>             A     
#>  6 R00478  Albizia … (L.) Bent… <NULL>                 <df [4 × 1]>       A     
#>  7 R20140  Alocasia… (L.) G.Don <df [1 × 3]>           <df [4 × 1]>       A     
#>  8 R20141  Amynthas… (Goto & H… <df [1 × 3]>           <df [2 × 1]>       A     
#>  9 R20142  Annona c… (Mill.)    <df [1 × 3]>           <df [3 × 1]>       A     
#> 10 R20143  Apalone … (Le Sueur… <df [1 × 3]>           <df [1 × 1]>       A     
#> # ℹ 69 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <lgl>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get info about one or more species by EASIN Species IDs
get_species(easin_id = c("R00460", "R12250"))
#> # A tibble: 2 × 27
#>   EASINID Name       Authorship FirstIntroductionsInEU PresentInCountries Status
#>   <chr>   <chr>      <chr>      <list>                 <list>             <chr> 
#> 1 R00460  Ailanthus… (Mill.) S… <df [1 × 3]>           <df [52 × 1]>      A     
#> 2 R12250  Procambar… (Girard, … <df [1 × 3]>           <df [24 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <lgl>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <lgl>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>

# Get info about one or more species by scientific names or parts of it
get_species(scientific_name = c("Aceria ambrosia", "Procambarus"))
#> # A tibble: 4 × 27
#>   EASINID Name       Authorship FirstIntroductionsInEU PresentInCountries Status
#>   <chr>   <chr>      <chr>      <list>                 <list>             <chr> 
#> 1 R16888  Aceria am… Wilson, 1… <df [1 × 3]>           <df [1 × 1]>       A     
#> 2 R12248  Procambar… Girard, 1… <df [1 × 3]>           <df [1 × 1]>       A     
#> 3 R12250  Procambar… (Girard, … <df [1 × 3]>           <df [24 × 1]>      A     
#> 4 R17660  Procambar… Lyko, 2017 <df [1 × 3]>           <df [16 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <lgl>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>

# Get species by `environment`
get_species(environment = c("MAR","OLI"))
#> # A tibble: 2,351 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R20136  Ablennes… "(Valenci… <df [1 × 3]>           <df [2 × 1]>       A     
#>  2 R20210  Abudefdu… "(Linnaeu… <df [1 × 3]>           <NULL>             A     
#>  3 R19331  Abudefdu… "(Steinda… <df [1 × 3]>           <df [2 × 1]>       A     
#>  4 R00027  Abudefdu… "(Linnaeu… <df [1 × 3]>           <df [4 × 1]>       A     
#>  5 R19652  Abudefdu… "(Lacepèd… <df [1 × 3]>           <df [4 × 1]>       A     
#>  6 R20399  Abudefdu… "(Forsskå… <NULL>                 <df [1 × 1]>       A     
#>  7 R20754  Abudefdu… "(Müller … <NULL>                 <df [1 × 1]>       A     
#>  8 R19332  Abudefdu… "(Quoy & … <df [1 × 3]>           <df [7 × 1]>       A     
#>  9 R16518  Abyla tr… "Quoy & G… <df [1 × 3]>           <df [2 × 1]>       Q     
#> 10 R17484  Acanthar… "(Forest,… <df [1 × 3]>           <df [1 × 1]>       A     
#> # ℹ 2,341 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species by `country_code`
get_species(country_code = c("IE", "LT"))
#> # A tibble: 104 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00116  Acer neg… L.         <df [1 × 3]>           <df [43 × 1]>      A     
#>  2 R17242  Alexandr… (Howell) … <df [1 × 3]>           <df [6 × 1]>       A     
#>  3 R00595  Allium t… L.         <df [1 × 3]>           <df [22 × 1]>      A     
#>  4 R00835  Amelanch… Wiegand    <df [1 × 3]>           <df [19 × 1]>      A     
#>  5 R17488  Amphibal… Darwin, 1… <df [1 × 3]>           <df [26 × 1]>      C     
#>  6 R01070  Anser an… (Linnaeus… <df [1 × 3]>           <df [55 × 1]>      C     
#>  7 R01255  Aponoget… L.f.       <df [1 × 3]>           <df [9 × 1]>       A     
#>  8 R01414  Arion vu… Moquin-Ta… <df [1 × 3]>           <df [37 × 1]>      A     
#>  9 R01826  Azolla f… Lam.       <df [1 × 3]>           <df [35 × 1]>      A     
#> 10 R18515  Bidens f… Linnaeus,… <df [1 × 3]>           <df [37 × 1]>      A     
#> # ℹ 94 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species by `region_code`
get_species(region_code = c("ES7", "PT3"))
#> # A tibble: 357 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00031  Abutilon… (Cav.) Sw… <df [1 × 3]>           <df [2 × 1]>       A     
#>  2 R00033  Abutilon… Medik.     <df [1 × 3]>           <df [45 × 1]>      A     
#>  3 R00035  Acacia b… F.Muell.   <df [1 × 3]>           <df [7 × 1]>       A     
#>  4 R00037  Acacia c… G.Don      <df [1 × 3]>           <df [3 × 1]>       A     
#>  5 R00038  Acacia c… G.Don      <df [1 × 3]>           <df [10 × 1]>      A     
#>  6 R00039  Acacia d… Link s.l.  <df [1 × 3]>           <df [23 × 1]>      A     
#>  7 R00042  Acacia k… Hayne      <df [1 × 3]>           <df [12 × 1]>      A     
#>  8 R00043  Acacia l… Benth.     <df [1 × 3]>           <df [2 × 1]>       A     
#>  9 R00044  Acacia l… (Andrews)… <df [1 × 3]>           <df [8 × 1]>       A     
#> 10 R00045  Acacia l… (Labill.)… <df [1 × 3]>           <df [1 × 1]>       A     
#> # ℹ 347 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species by `taxon`
get_species(taxon = c(family = "Vespidae"))
#> # A tibble: 10 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R05238  "Dolicho… "(Fabrici… <df [1 × 3]>           <df [17 × 1]>      A     
#>  2 R20108  "Laticor… "(Shoemak… <df [1 × 3]>           <NULL>             A     
#>  3 R11911  "Poliste… "(Christ,… <df [1 × 3]>           <df [34 × 1]>      A     
#>  4 R19647  "Vespa b… "Fabriciu… <df [1 × 3]>           <df [1 × 1]>       A     
#>  5 R20364  "Vespa o… "Linnaeus… <df [1 × 3]>           <df [1 × 1]>       A     
#>  6 R15970  "Vespa v… "Buysson,… <df [1 × 3]>           <df [15 × 1]>      A     
#>  7 R15972  "Vespula… "(Fabrici… <df [1 × 3]>           <df [51 × 1]>      A     
#>  8 R20193  "Vespula… "(de Saus… <NULL>                 <NULL>             A     
#>  9 R15973  "Vespula… "(Linnaeu… <df [1 × 3]>           <df [28 × 1]>      A     
#> 10 R15974  "Vespula… "(Linnaeu… <df [1 × 3]>           <df [42 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>

# Get species by full `taxonomy` levels (up to family)
get_species(
  taxonomy = c(
    kingdom = "Animalia",
    phylum = "Arthropoda",
    class = "Insecta",
    order = "Hymenoptera",
    family = "Vespidae"
  )
)
#> # A tibble: 10 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R05238  "Dolicho… "(Fabrici… <df [1 × 3]>           <df [17 × 1]>      A     
#>  2 R20108  "Laticor… "(Shoemak… <df [1 × 3]>           <NULL>             A     
#>  3 R11911  "Poliste… "(Christ,… <df [1 × 3]>           <df [34 × 1]>      A     
#>  4 R19647  "Vespa b… "Fabriciu… <df [1 × 3]>           <df [1 × 1]>       A     
#>  5 R20364  "Vespa o… "Linnaeus… <df [1 × 3]>           <df [1 × 1]>       A     
#>  6 R15970  "Vespa v… "Buysson,… <df [1 × 3]>           <df [15 × 1]>      A     
#>  7 R15972  "Vespula… "(Fabrici… <df [1 × 3]>           <df [51 × 1]>      A     
#>  8 R20193  "Vespula… "(de Saus… <NULL>                 <NULL>             A     
#>  9 R15973  "Vespula… "(Linnaeu… <df [1 × 3]>           <df [28 × 1]>      A     
#> 10 R15974  "Vespula… "(Linnaeu… <df [1 × 3]>           <df [42 × 1]>      A     
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>,
#> #   Synonyms <list>, CBD_Pathways <list>

# Get species present in one or more countries
get_species(present_in_country = c("LU", "IE"))
#> # A tibble: 3,496 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R00001  Abax par… (Duftschm… <df [1 × 3]>           <df [11 × 1]>      A     
#>  2 R00004  Abgralla… (Signoret… <df [1 × 3]>           <df [5 × 1]>       C     
#>  3 R00005  Abies al… Mill.      <df [1 × 3]>           <df [38 × 1]>      A     
#>  4 R00006  Abies ba… (L.) Mill. <df [1 × 3]>           <df [11 × 1]>      A     
#>  5 R00007  Abies ce… Loudon     <df [1 × 3]>           <df [13 × 1]>      A     
#>  6 R00008  Abies co… (Gordon) … <df [1 × 3]>           <df [16 × 1]>      A     
#>  7 R00011  Abies gr… (Douglas … <df [1 × 3]>           <df [15 × 1]>      A     
#>  8 R00013  Abies no… (Steven) … <df [1 × 3]>           <df [23 × 1]>      A     
#>  9 R00014  Abies pi… Boiss.     <df [1 × 3]>           <df [15 × 1]>      A     
#> 10 R00015  Abies pr… Rehder     <df [1 × 3]>           <df [9 × 1]>       A     
#> # ℹ 3,486 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species by `status`
get_species(status = c("Q", "A"))
#> # A tibble: 14,362 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R19422  Candidat… "Fagen et… <df [1 × 3]>           <df [14 × 1]>      Q     
#>  2 R11525  Candidat… ""         <df [1 × 3]>           <df [9 × 1]>       Q     
#>  3 R19423  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  4 R19424  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  5 R19425  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  6 R19426  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  7 R19427  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  8 R19428  Candidat… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  9 R11526  Candidat… ""         <df [1 × 3]>           <df [29 × 1]>      Q     
#> 10 R19429  Candidat… ""         <df [1 × 3]>           <df [2 × 1]>       Q     
#> # ℹ 14,352 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species which are native in at least one country
get_species(partly_native = TRUE)
#> # A tibble: 5,214 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R19422  'Candida… "Fagen et… <df [1 × 3]>           <df [14 × 1]>      Q     
#>  2 R11525  'Candida… ""         <df [1 × 3]>           <df [9 × 1]>       Q     
#>  3 R19424  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  4 R19425  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  5 R19426  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  6 R19427  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  7 R19428  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#>  8 R11526  'Candida… ""         <df [1 × 3]>           <df [29 × 1]>      Q     
#>  9 R19429  'Candida… ""         <df [1 × 3]>           <df [2 × 1]>       Q     
#> 10 R19430  'Candida… ""         <df [1 × 3]>           <df [1 × 1]>       Q     
#> # ℹ 5,204 more rows
#> # ℹ 21 more variables: HasImpact <lgl>, IsEUConcern <lgl>, EUConcernName <chr>,
#> #   IsOutermostConcern <lgl>, ConcernedOutermostRegions <list>,
#> #   IsMSConcern <lgl>, ConcernedMS <list>, IsPartNative <lgl>,
#> #   NativeRange <list>, IsHorizonScanning <lgl>, LastRevisionDate <lgl>,
#> #   Kingdom <chr>, Phylum <chr>, Class <chr>, Order <chr>, Family <chr>,
#> #   ImpactSources <list>, ImpactOnSectors <list>, CommonNames <list>, …

# Get species which are native in one or more countries
get_species(native_in_country = c("EE","FI"))
#> # A tibble: 1,252 × 27
#>    EASINID Name      Authorship FirstIntroductionsInEU PresentInCountries Status
#>    <chr>   <chr>     <chr>      <list>                 <list>             <chr> 
#>  1 R19422  'Candida… "Fagen et… <df [1 × 3]>           <df [14 × 1]>      Q     
#>  2 R11526  'Candida… ""         <df [1 × 3]>           <df [29 × 1]>      Q     
#>  3 R00024  Abramis … "(Linnaeu… <df [1 × 3]>           <df [32 × 1]>      A     
#>  4 R00070  Acanthoc… "(Müller,… <df [1 × 3]>           <df [1 × 1]>       C     
#>  5 R00072  Acanthoc… "(Linnaeu… <df [1 × 3]>           <df [28 × 1]>      A     
#>  6 R00074  Acanthoc… "(Fabrici… <df [1 × 3]>           <df [21 × 1]>      A     
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
