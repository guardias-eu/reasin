test_that("`get_species()` returns all data correctly", {
  skip_if_offline()
  vcr::use_cassette("all_data", {
    all_data_df <- get_species()
  })
  # all_data_df is a tibble
  expect_s3_class(all_data_df, "tbl_df")
  expect_s3_class(all_data_df, "data.frame")
  expect_identical(nrow(all_data_df), 14353L)
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

