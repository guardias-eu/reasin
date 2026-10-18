# Changelog

## reasin 0.1.0

- Added a `NEWS.md` file to track changes to the package.
- Add `limit` argument to
  [`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
  to limit the number of species returned from the API
  ([\#10](https://github.com/guardias-eu/reasin/issues/10)).
- reasin now relies on R \>= 4.1.0 to use base pipes (\|\> rather than
  %\>%) ([\#9](https://github.com/guardias-eu/reasin/issues/9)).
- Scientific names returned in column `Name` are now always consistently
  cleaned up from quotes
  ([\#11](https://github.com/guardias-eu/reasin/issues/11)).
- Vignette added to explain the use of
  [`get_species()`](https://guardias-eu.github.io/reasin/reference/get_species.md)
  and related help functions
  ([\#12](https://github.com/guardias-eu/reasin/issues/12)).
- Add [`checklist`](https://inbo.github.io/checklist/) infrastructure.
