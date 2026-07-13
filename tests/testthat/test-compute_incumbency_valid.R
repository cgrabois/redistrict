test_that("compute_incumbency_valid requires a match_level panel", {
  bad_panel <- data.frame(a = 1)
  expect_error(compute_incumbency_valid(bad_panel, "i2i"), "match_level")
})

test_that("compute_incumbency_valid validates incumbent_match_type", {
  panel <- build_district_panel(start_congress = 111, end_congress = 112, shape = "match_level")
  expect_error(compute_incumbency_valid(panel, "nonsense"), "i2i.*i2c")
  expect_error(compute_incumbency_valid(panel, NULL), "i2i.*i2c")
})

test_that("compute_incumbency_valid adds the documented columns", {
  panel <- build_district_panel(start_congress = 111, end_congress = 112, shape = "match_level")
  result <- compute_incumbency_valid(panel, "i2i")

  expect_true(all(c("source_incumbent_valid", "target_incumbent_valid") %in% names(result)))

  valid_values <- c("agree", "disagree", "no incumbent")
  expect_true(all(result$source_incumbent_valid %in% valid_values | is.na(result$source_incumbent_valid)))
  expect_true(all(result$target_incumbent_valid %in% valid_values | is.na(result$target_incumbent_valid)))
})
