# Hand-built panels describing the same two lineages in all three shapes:
#   A: AA-01 (cd92) -> AA-01 (cd93) -> AA-02 (cd94) -> AA-02 (cd95)  4 observations -> 6 pairs
#   B: BB-01 (cd92) -> BB-01 (cd93), then dropped in cd94              2 observations -> 1 pair
# so every shape should give 7 comparisons.

mock_wide <- data.frame(
  lineage_id = 1:2,
  state_abb = c("AA", "BB"),
  cd92 = c("AA-01", "BB-01"),
  cd93 = c("AA-01", "BB-01"),
  cd94 = c("AA-02", NA),
  cd95 = c("AA-02", NA)
)

mock_long <- data.frame(
  lineage_id = c(1, 1, 1, 1, 2, 2),
  state_abb = c("AA", "AA", "AA", "AA", "BB", "BB"),
  congress = c(92, 93, 94, 95, 92, 93),
  district = c("AA-01", "AA-01", "AA-02", "AA-02", "BB-01", "BB-01")
)

mock_match_level <- data.frame(
  source = c("AA-01", "BB-01", "AA-01", "BB-01", "AA-02"),
  target = c("AA-01", "BB-01", "AA-02", NA, "AA-02"),
  cycle = c("cd92_cd93", "cd92_cd93", "cd93_cd94", "cd93_cd94", "cd94_cd95"),
  state_abb = c("AA", "BB", "AA", "BB", "AA")
)

test_that("compute_comparison_count counts all pairs within a lineage, wide", {
  expect_equal(compute_comparison_count(mock_wide, "wide"), 7)
})

test_that("compute_comparison_count counts all pairs within a lineage, long", {
  expect_equal(compute_comparison_count(mock_long, "long"), 7)
})

test_that("compute_comparison_count counts non-adjacent pairs, match_level", {
  expect_equal(compute_comparison_count(mock_match_level, "match_level"), 7)
})

test_that("compute_comparison_count ignores unmatched districts, match_level", {
  # an NA target (district ends) and an NA source (district appears) link nothing
  panel <- data.frame(
    source = c("AA-01", "AA-02", NA),
    target = c("AA-01", NA, "AA-03"),
    cycle = "cd92_cd93",
    state_abb = "AA"
  )
  expect_equal(compute_comparison_count(panel, "match_level"), 1)
})

test_that("compute_comparison_count treats the same name in different congresses as different districts", {
  # AA-01 reappears in cd94 but is not matched from cd93, so it is a new lineage
  panel <- data.frame(
    source = c("AA-01", "AA-05"),
    target = c("AA-02", "AA-01"),
    cycle = c("cd92_cd93", "cd93_cd94"),
    state_abb = "AA"
  )
  expect_equal(compute_comparison_count(panel, "match_level"), 2)
})

test_that("compute_comparison_count is 0 when nothing is observed twice", {
  expect_equal(compute_comparison_count(mock_long[c(1, 5), ], "long"), 0)
  expect_equal(
    compute_comparison_count(mock_match_level[0, ], "match_level"), 0
  )
})

test_that("compute_comparison_count agrees across shapes on real panels", {
  args <- list(start_congress = 111, end_congress = 114)
  counts <- vapply(
    c("wide", "long", "match_level"),
    function(shape) {
      panel <- do.call(build_district_panel, c(args, shape = shape))
      compute_comparison_count(panel, shape)
    },
    numeric(1)
  )
  expect_equal(unname(counts[["long"]]), unname(counts[["wide"]]))
  expect_equal(unname(counts[["long"]]), unname(counts[["match_level"]]))
})

test_that("compute_comparison_count validates shape", {
  expect_error(compute_comparison_count(mock_long, "nonsense"))
})

test_that("compute_comparison_count infers shape from column names when NA", {
  expect_equal(compute_comparison_count(mock_wide), 7)
  expect_equal(compute_comparison_count(mock_long), 7)
  expect_equal(compute_comparison_count(mock_match_level), 7)
})

test_that("compute_comparison_count errors informatively when shape can't be inferred", {
  expect_error(compute_comparison_count(data.frame(a = 1)), "Could not infer")
  expect_error(compute_comparison_count(data.frame(a = 1)), "set `shape` explicitly")
})
