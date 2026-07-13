test_that("build_district_panel validates its inputs", {
  expect_error(build_district_panel(start_congress = "abc"), "numeric")
  expect_error(build_district_panel(start_congress = 111, end_congress = 111))
  expect_error(build_district_panel(start_congress = 50, end_congress = 60), "92-119")
  expect_error(build_district_panel(variable = "nonsense"))
  expect_error(build_district_panel(method = "nonsense"))
  expect_error(build_district_panel(shape = "nonsense"))
  expect_error(build_district_panel(incumbent_lock = "nonsense"))
  expect_error(build_district_panel(threshold = -1), "non-negative|>= 0")
})

test_that("build_district_panel produces the documented columns per shape", {
  wide <- build_district_panel(start_congress = 111, end_congress = 113, shape = "wide")
  expect_true(all(c("lineage_id", "state_abb", "cd111", "cd112", "cd113") %in% names(wide)))

  long <- build_district_panel(start_congress = 111, end_congress = 113, shape = "long")
  expect_true(all(c("lineage_id", "state_abb", "congress", "district") %in% names(long)))

  match_level <- build_district_panel(start_congress = 111, end_congress = 113, shape = "match_level")
  expect_true(all(c("source", "target", "cycle", "state_abb") %in% names(match_level)))
})

test_that("build_district_panel allows a single congress pair", {
  match_level <- build_district_panel(start_congress = 111, end_congress = 112, shape = "match_level")
  expect_true(all(match_level$cycle == "cd111_cd112"))
})
