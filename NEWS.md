# reasin 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Add `limit` argument to `get_species()` to limit the number of species returned from the API (#10).
* reasin now relies on R >= 4.1.0 to use base pipes (|> rather than %>%) (#9).
* Scientific names returned in column `Name` are now always consistently cleaned up from quotes (#11).
* Add [`checklist`](https://inbo.github.io/checklist/) infrastructure.
