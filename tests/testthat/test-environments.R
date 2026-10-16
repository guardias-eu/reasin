test_that("`environments()` returns a tibble", {
  result <- environments()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`environments()` returns correct structure", {
  result <- environments()
  expect_equal(nrow(result), 4)
  expect_equal(names(result), c("environment", "env_code"))
})

test_that("`environments()` returns correct values", {
  result <- environments()
  expect_equal(
    result$environment,
    c("marine", "freshwater", "terrestrial", "oligohaline")
  )
  expect_equal(result$env_code, c("MAR", "FRW", "TER", "OLI"))
})

test_that("`environments()` columns have correct types", {
  result <- environments()
  expect_type(result$environment, "character")
  expect_type(result$env_code, "character")
})
