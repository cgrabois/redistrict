test_that("compute_match_factor requires a match_level panel", {
  bad_panel <- data.frame(a = 1)
  expect_error(compute_match_factor(bad_panel, vars = "pop"), "match_level")
})

test_that("compute_match_factor validates vars against data", {
  panel <- build_district_panel(start_congress = 111, end_congress = 112, shape = "match_level")
  expect_error(compute_match_factor(panel, vars = "nonsense"), "nonsense")
})

test_that("compute_match_factor adds one numeric column per variable", {
  panel <- build_district_panel(start_congress = 111, end_congress = 112, shape = "match_level")
  result <- compute_match_factor(panel, vars = c("pop", "area"))

  expect_true(all(c("pop", "area") %in% names(result)))
  expect_true(is.numeric(result$pop))
  expect_true(is.numeric(result$area))
})
