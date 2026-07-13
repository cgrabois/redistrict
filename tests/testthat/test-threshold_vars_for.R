test_that("threshold_vars_for maps pop/area to both directional afacts", {
  expect_equal(threshold_vars_for("pop"), c("afact_s2t", "afact_t2s"))
  expect_equal(threshold_vars_for("area"), c("afact_s2t_area", "afact_t2s_area"))
})

test_that("threshold_vars_for maps afact_* variables to themselves", {
  expect_equal(threshold_vars_for("afact_s2t"), "afact_s2t")
  expect_equal(threshold_vars_for("afact_t2s"), "afact_t2s")
  expect_equal(threshold_vars_for("afact_s2t_area"), "afact_s2t_area")
  expect_equal(threshold_vars_for("afact_t2s_area"), "afact_t2s_area")
})

test_that("threshold_vars_for errors on an unknown variable", {
  expect_error(threshold_vars_for("nonsense"), "Unknown variable")
})
