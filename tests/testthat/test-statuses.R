test_that("`statuses()` returns a tibble", {
  result <- statuses()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`statuses()` returns correct structure", {
  result <- statuses()
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("status", "status_code"))
})

test_that("`statuses()` returns correct status names", {
  result <- statuses()
  expected_statuses <- c("Alien", "Cryptogenic", "Questionable")
  expect_equal(result$status, expected_statuses)
})

test_that("`statuses()` returns correct status codes", {
  result <- statuses()
  expected_codes <- c("A", "C", "Q")
  expect_equal(result$status_code, expected_codes)
})

test_that("`statuses()` columns have correct types", {
  result <- statuses()
  expect_type(result$status, "character")
  expect_type(result$status_code, "character")
})
