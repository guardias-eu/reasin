test_that("`easin_id` must be character", {
  expect_error(
    get_species(easin_id = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`scientific_name` must be character", {
  expect_error(
    get_species(scientific_name = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`scientific_name` must be at least 4 characters", {
  expect_error(
    get_species(scientific_name = "Ace"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`environment` must be character", {
  expect_error(
    get_species(environment = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`environment` must be one of valid values", {
  expect_error(
    get_species(environment = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})


test_that("`country_code` must be character", {
  expect_error(
    get_species(country_code = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`country_code` must be valid ISO 3166-1 alpha-2 codes", {
  expect_error(
    get_species(country_code = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`country_code` is case-insensitive", {
  skip_if_offline()
  vcr::use_cassette("country_code_lowercase", {
    result <- get_species(country_code = "at")
  })
  expect_s3_class(result, "data.frame")
})

test_that("`region_code` must be character", {
  expect_error(
    get_species(region_code = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`region_code` must be valid NUTS codes", {
  expect_error(
    get_species(region_code = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`region_code` is case-insensitive", {
  skip_if_offline()
  vcr::use_cassette("region_code_lowercase", {
    result <- get_species(region_code = "es7")
  })
  expect_s3_class(result, "data.frame")
})

test_that("`impact` must be character", {
  expect_error(
    get_species(impact = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`impact` must be 'hi' or 'lo'", {
  expect_error(
    get_species(impact = "invalid"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`taxon` must be a named vector", {
  expect_error(
    get_species(taxon = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`taxon` must have valid ranks", {
  expect_error(
    get_species(taxon = c(invalid_rank = "value")),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`taxonomy` must be a named vector of length 5", {
  expect_error(
    get_species(taxonomy = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`taxonomy` must have exactly 5 elements", {
  expect_error(
    get_species(taxonomy = c(kingdom = "Animalia", phylum = "Arthropoda")),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`present_in_country` must be character", {
  expect_error(
    get_species(present_in_country = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`present_in_country` must be valid country codes", {
  expect_error(
    get_species(present_in_country = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`status` must be character", {
  expect_error(
    get_species(status = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`status` must be valid status codes", {
  expect_error(
    get_species(status = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})


test_that("`status` is case-insensitive", {
  skip_if_offline()
  vcr::use_cassette("status_lowercase", {
    result <- get_species(status = "q")
  })
  expect_s3_class(result, "data.frame")
})

test_that("`union_concern` must be TRUE", {
  expect_error(
    get_species(union_concern = FALSE),
    "must be TRUE"
  )
})

test_that("`union_concern` must be logical TRUE", {
  expect_error(
    get_species(union_concern = "TRUE"),
    "must be TRUE"
  )
})

test_that("`horizon` must be TRUE", {
  expect_error(
    get_species(horizon = FALSE),
    "must be TRUE"
  )
})

test_that("`horizon` must be logical TRUE", {
  expect_error(
    get_species(horizon = "TRUE"),
    "must be TRUE"
  )
})

test_that("`partly_native` must be TRUE", {
  expect_error(
    get_species(partly_native = FALSE),
    "must be TRUE"
  )
})

test_that("`partly_native` must be logical TRUE", {
  expect_error(
    get_species(partly_native = "TRUE"),
    "must be TRUE"
  )
})

test_that("`native_in_country` must be character", {
  expect_error(
    get_species(native_in_country = 123),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`native_in_country` must be valid country codes", {
  expect_error(
    get_species(native_in_country = "INVALID"),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("Multiple non-NULL arguments raise an error", {
  expect_error(
    get_species(
      easin_id = "R00460",
      environment = "MAR",
      status = "A"
    ),
    class = "reasin_error_assignment_invalid"
  )
})

test_that("`easin_id` returns empty tibble when no species found", {
  skip_if_offline()
  vcr::use_cassette("easin_id_not_found", {
    result <- get_species(easin_id = "INVALID999")
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("`scientific_name` returns empty tibble when no species found", {
  skip_if_offline()
  vcr::use_cassette("scientific_name_not_found", {
    result <- get_species(scientific_name = "abcdefgxyz")
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("`taxon` returns empty tibble when no species found", {
  skip_if_offline()
  vcr::use_cassette("taxon_not_found", {
    result <- get_species(taxon = c(family = "Oldonidae"))
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("`taxonomy` returns empty tibble when no species found", {
  skip_if_offline()
  vcr::use_cassette("taxonomy_not_found", {
    result <- get_species(taxonomy = c(
      kingdom = "a",
      phylum = "b",
      class = "c",
      order = "d",
      family = "e"
    ))
  })
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
