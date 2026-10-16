test_that("`get_species()` returns all data correctly", {
  skip_if_offline()
  vcr::use_cassette("all_data", {
    all_data_df <- get_species()
  })
  expect_s3_class(all_data_df, "tbl_df")
  expect_s3_class(all_data_df, "data.frame")
  expect_identical(nrow(all_data_df), 15751L)
  expect_identical(
    names(all_data_df),
    c("EasinID",
      "Name",
      "Authorship",
      "LSID",
      "Reference",
      "HasImpact",
      "IsEUConcern",
      "IsMSConcern",
      "IsOutermostConcern",
      "IsPartNative",
      "IsHorizonScanning",
      "Status"
    )
  )
  # Columns `EasinID`, `Name`, `Authorship`, `LSID`, `Reference` and `Status`
  # are character
  expect_type(all_data_df$EasinID, "character")
  expect_type(all_data_df$Name, "character")
  expect_type(all_data_df$Authorship, "character")
  expect_type(all_data_df$LSID, "character")
  expect_type(all_data_df$Reference, "character")
  expect_type(all_data_df$Status, "character")
  # Columns `HasImpact`, `IsEUConcern`, `IsMSConcern`, `IsOutermostConcern`,
  # `IsPartNative` and `IsHorizonScanning` are logical
  expect_type(all_data_df$HasImpact, "logical")
  expect_type(all_data_df$IsEUConcern, "logical")
  expect_type(all_data_df$IsMSConcern, "logical")
  expect_type(all_data_df$IsOutermostConcern, "logical")
  expect_type(all_data_df$IsPartNative, "logical")
  expect_type(all_data_df$IsHorizonScanning, "logical")
})

test_that("`easin_id` accepts valid character input", {
  skip_if_offline()
  vcr::use_cassette("easin_id_valid", {
    result <- get_species(easin_id = c("R02554", "R00024", "R00481"))
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
    "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
    "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
    "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
    "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
    "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
    "Synonyms", "CBD_Pathways"
    )
  )
  expect_type(result$EASINID, "character")
  expect_type(result$Name, "character")
  expect_type(result$Authorship, "character")
  expect_type(result$FirstIntroductionsInEU, "list")
  expect_type(result$PresentInCountries, "list")
  expect_type(result$Status, "character")
  expect_type(result$HasImpact, "logical")
  expect_type(result$IsEUConcern, "logical")
  expect_type(result$EUConcernName, "character")
  expect_type(result$IsOutermostConcern, "logical")
  expect_type(result$ConcernedOutermostRegions, "list")
  expect_type(result$IsMSConcern, "logical")
  expect_type(result$ConcernedMS, "list")
  expect_type(result$IsPartNative, "logical")
  expect_type(result$NativeRange, "list")
  expect_type(result$IsHorizonScanning, "logical")
  # Column `LastRevisionDate` is NA because I cannot find taxa with this info at
  # this moment
  expect_type(result$LastRevisionDate, "logical")
  expect_type(result$Kingdom, "character")
  expect_type(result$Phylum, "character")
  expect_type(result$Class, "character")
  expect_type(result$Order, "character")
  expect_type(result$Family, "character")
  expect_type(result$ImpactSources, "list")
  expect_type(result$ImpactOnSectors, "list")
  expect_type(result$CommonNames, "list")
  expect_type(result$Synonyms, "list")
  expect_type(result$CBD_Pathways, "list")
})

test_that("`scientific_name` accepts valid character input", {
  skip_if_offline()
  vcr::use_cassette("scientific_name_valid", {
    result <- get_species(scientific_name = "Aceria")
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
  # We don't check the type of each column anymore. See previous tests.
})

test_that("`environment` accepts valid character input", {
  skip_if_offline()
  vcr::use_cassette("environment_valid", {
    result <- get_species(environment = "OLI", limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`environment` accepts multiple valid values", {
  skip_if_offline()
  vcr::use_cassette("environment_multiple", {
    result <- get_species(environment = c("OLI", "FRW"), limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`country_code` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("country_code_valid", {
    result <- get_species(country_code = "LT", limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
  # If there are no species for the specific country, an empty tibble is returned
  result <- get_species(country_code = "AT")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 0)
  expect_true(ncol(result) == 0)
})

test_that("`region_code` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("region_code_valid", {
    result <- get_species(region_code = "ES7", limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`impact` accepts 'hi' value", {
  skip_if_offline()
  vcr::use_cassette("impact_hi", {
    result <- get_species(impact = "hi", limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`impact` accepts multiple valid values", {
  skip_if_offline()
  vcr::use_cassette("impact_multiple", {
    result <- get_species(impact = c("hi", "lo"), limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`taxon` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("taxon_valid", {
    result <- get_species(taxon = c(family = "Vespidae"))
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
  # If there are no species for the specific taxon, an empty tibble is returned
  result <- get_species(taxon = c(family = "Lillillidae"))
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 0)
  expect_true(ncol(result) == 0)
})


test_that("`taxonomy` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("taxonomy_valid", {
    result <- get_species(taxonomy = c(
      kingdom = "Animalia",
      phylum = "Arthropoda",
      class = "Insecta",
      order = "Hymenoptera",
      family = "Vespidae"
    ))
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`present_in_country` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("present_in_country_valid", {
    result <- get_species(present_in_country = "MT" , limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`status` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("status_valid", {
    result <- get_species(status = "Q", limit = 5)
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_identical(
    names(result),
    c("EASINID", "Name", "Authorship", "FirstIntroductionsInEU",
      "PresentInCountries", "Status", "HasImpact", "IsEUConcern",
      "EUConcernName", "IsOutermostConcern", "ConcernedOutermostRegions",
      "IsMSConcern", "ConcernedMS", "IsPartNative", "NativeRange",
      "IsHorizonScanning", "LastRevisionDate", "Kingdom", "Phylum", "Class",
      "Order", "Family", "ImpactSources", "ImpactOnSectors", "CommonNames",
      "Synonyms", "CBD_Pathways"
    )
  )
})

test_that("`status` accepts multiple valid values", {
  skip_if_offline()
  vcr::use_cassette("status_multiple", {
    result <- get_species(status = c("Q", "C"), limit = 5)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`union_concern` accepts TRUE value", {
  skip_if_offline()
  vcr::use_cassette("union_concern_true", {
    result <- get_species(union_concern = TRUE)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`horizon` accepts TRUE value", {
  skip_if_offline()
  vcr::use_cassette("horizon_true", {
    result <- get_species(horizon = TRUE, limit = 5)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`partly_native` accepts TRUE value", {
  skip_if_offline()
  vcr::use_cassette("partly_native_true", {
    result <- get_species(partly_native = TRUE, limit = 5)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`native_in_country` accepts valid input", {
  skip_if_offline()
  vcr::use_cassette("native_in_country_valid", {
    result <- get_species(native_in_country = "LU", limit = 5)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`native_in_country` accepts multiple valid values", {
  skip_if_offline()
  vcr::use_cassette("native_in_country_multiple", {
    result <- get_species(native_in_country = c("LU", "IE"), limit = 5)
  })
  expect_s3_class(result, "data.frame")
})

test_that("`limit` reduces the number of rows returned", {
  skip_if_offline()
  vcr::use_cassette("limit_reduces_rows", {
    result <- get_species(limit = 10)
  })
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10)
})

test_that("`limit` works with paginated results", {
  skip_if_offline()
  vcr::use_cassette("limit_paginated", {
    result <- get_species(environment = "MAR", limit = 25)
  })
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 25)
})

test_that("`limit` works with non-paginated results", {
  skip_if_offline()
  vcr::use_cassette("limit_non_paginated", {
    result <- get_species(scientific_name = "Aceria", limit = 2)
  })
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), 2)
})

test_that("`limit` returns fewer rows than available when limit is small", {
  skip_if_offline()
  vcr::use_cassette("limit_small", {
    result_limited <- get_species(present_in_country = "MT", limit = 3)
    result_unlimited <- get_species(present_in_country = "MT")
  })
  expect_s3_class(result_limited, "tbl_df")
  expect_s3_class(result_unlimited, "tbl_df")
  expect_equal(nrow(result_limited), 3)
  expect_gt(nrow(result_unlimited), nrow(result_limited))
})

test_that("`limit` returns all rows when limit exceeds available data", {
  skip_if_offline()
  vcr::use_cassette("limit_exceeds_data", {
    result <- get_species(easin_id = c("R02554", "R00024"), limit = 100)
  })
  expect_s3_class(result, "tbl_df")
  expect_lte(nrow(result), 2)
})
