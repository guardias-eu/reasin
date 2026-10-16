test_that("`ranks()` returns a tibble", {
  result <- ranks()
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("`ranks()` returns correct structure", {
  result <- ranks()
  expect_equal(nrow(result), 5)
  expect_equal(names(result), c("rank"))
})

test_that("`ranks()` returns correct rank names", {
  result <- ranks()
  expected_ranks <- c("kingdom", "phylum", "class", "order", "family")
  expect_equal(result$rank, expected_ranks)
})

test_that("`ranks()` column has correct type", {
  result <- ranks()
  expect_type(result$rank, "character")
})
