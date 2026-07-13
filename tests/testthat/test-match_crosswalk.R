test_that("match_crosswalk validates its inputs", {
  expect_error(match_crosswalk("abc"), "numeric")
  expect_error(match_crosswalk(111, variable = "nonsense"))
  expect_error(match_crosswalk(111, method = "nonsense"))
  expect_error(match_crosswalk(111, incumbent_lock = "nonsense"))
  expect_error(match_crosswalk(111, threshold = -1), "non-negative|>= 0")
})

test_that("match_crosswalk returns one row per matched district across all states", {
  result <- match_crosswalk(111)
  expect_true(all(c("source", "target", "state_abb") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("match_crosswalk with incumbent_lock still returns a valid data.frame", {
  result <- match_crosswalk(111, incumbent_lock = "i2i")
  expect_true(all(c("source", "target", "state_abb") %in% names(result)))
})
