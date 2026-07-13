test_that("hungarian_match finds the optimal assignment (square matrix)", {
  m <- matrix(c(1,5,2, 9,2,3, 3,4,8), nrow = 3, byrow = TRUE,
              dimnames = list(c("A","B","C"), c("X","Y","Z")))
  data <- make_mock_data(m)

  result <- hungarian_match(101, "pop", "XX", data = data, threshold = NA)

  expect_equal(result$target[result$source %in% "A"], "Y")
  expect_equal(result$target[result$source %in% "B"], "X")
  expect_equal(result$target[result$source %in% "C"], "Z")
})

test_that("hungarian_match handles more targets than sources", {
  m <- matrix(c(1,5,2, 9,2,3), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("X","Y","Z")))
  data <- make_mock_data(m)

  result <- hungarian_match(101, "pop", "XX", data = data, threshold = NA)

  expect_equal(result$target[result$source %in% "A"], "Y")
  expect_equal(result$target[result$source %in% "B"], "X")
  expect_true(is.na(result$source[result$target %in% "Z"]))
})

test_that("hungarian_match handles more sources than targets (transposed branch)", {
  m <- matrix(c(1,5, 9,2, 3,4), nrow = 3, byrow = TRUE,
              dimnames = list(c("A","B","C"), c("X","Y")))
  data <- make_mock_data(m)

  result <- hungarian_match(101, "pop", "XX", data = data, threshold = NA)

  expect_equal(result$target[result$source %in% "A"], "Y")
  expect_equal(result$target[result$source %in% "B"], "X")
  expect_true(is.na(result$target[result$source %in% "C"]))
})

test_that("hungarian_match un-matches a forced zero-value assignment when threshold = 0", {
  m <- matrix(c(5,0, 0,0), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)

  result <- hungarian_match(101, "afact_s2t", "XX", data = data, threshold = 0)

  expect_equal(result$target[result$source %in% "A"], "X")
  expect_true(is.na(result$target[result$source %in% "B"]))
  expect_true(is.na(result$source[result$target %in% "Y"]))
})

test_that("hungarian_match validates its inputs", {
  expect_error(hungarian_match("abc", "pop", "CA"), "numeric")
  expect_error(hungarian_match(111, "nonsense", "CA"))
  expect_error(hungarian_match(111, "pop", "CA", threshold = -1), "non-negative|>= 0")
})

test_that("hungarian_match errors clearly on NA in the matching matrix", {
  m <- matrix(c(NA, 1, 2, 3), 2, 2, dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)
  expect_error(hungarian_match(101, "pop", "XX", data = data), "NA values")
})
