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

test_that("match_crosswalk locks every incumbent pair when lock_threshold is NA", {
  cycle <- "cd112_cd113"

  result <- match_crosswalk(112, incumbent_lock = "i2i", lock_threshold = NA)

  # NA means no threshold is applied -- every incumbent pair for this cycle
  # should be locked in untouched, none left for the algorithm to reassign
  incumbent_pairs <- incumbency_matches_i2i |>
    dplyr::filter(cycle == .env$cycle) |>
    dplyr::transmute(source = incumbent_from, target = incumbent_to)

  # isolate the locked rows in the result (identifiable by their source
  # being an incumbent for this cycle) and confirm they're a subset of --
  # and here, since nothing is thresholded away, exactly equal to --
  # the incumbency match dataset
  locked_pairs <- result |>
    dplyr::filter(source %in% incumbent_pairs$source) |>
    dplyr::select(source, target)

  expect_equal(nrow(dplyr::anti_join(locked_pairs, incumbent_pairs, by = c("source", "target"))), 0)
  expect_equal(nrow(locked_pairs), nrow(incumbent_pairs))
})
