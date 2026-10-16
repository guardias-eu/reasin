library(vcr) # *Required* as vcr is set up on loading
library(qs2)
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  serialize_with = "qs2"
))
