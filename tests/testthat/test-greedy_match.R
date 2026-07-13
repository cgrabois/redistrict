test_that("greedy_match claims the highest available value first", {
  m <- matrix(c(1,5, 9,2), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)

  result <- greedy_match(101, "pop", "XX", data = data, threshold = NA)

  # 9 (B-X) is the single highest value and gets claimed first, leaving A-Y
  expect_equal(result$target[result$source %in% "B"], "X")
  expect_equal(result$target[result$source %in% "A"], "Y")
})

test_that("greedy_match breaks ties in column-major order", {
  m <- matrix(c(5,5, 1,1), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)

  result <- greedy_match(101, "pop", "XX", data = data, threshold = NA)

  # A-X and A-Y tie at 5; which(arr.ind = TRUE) returns A-X first in
  # column-major order
  expect_equal(result$target[result$source %in% "A"], "X")
  expect_equal(result$target[result$source %in% "B"], "Y")
})

test_that("greedy_match excludes zero-allocation pairs by default", {
  m <- matrix(c(5,0, 0,3), nrow = 2, byrow = TRUE,
              dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)

  result <- greedy_match(101, "afact_s2t", "XX", data = data, threshold = 0)

  expect_equal(result$target[result$source %in% "A"], "X")
  expect_equal(result$target[result$source %in% "B"], "Y")
})

test_that("greedy_match validates its inputs", {
  expect_error(greedy_match("abc", "pop", "CA"), "numeric")
  expect_error(greedy_match(111, "nonsense", "CA"))
  expect_error(greedy_match(111, "pop", "CA", threshold = -1), "non-negative|>= 0")
})

test_that("greedy_match errors clearly on NA in the matching matrix", {
  m <- matrix(c(NA, 1, 2, 3), 2, 2, dimnames = list(c("A","B"), c("X","Y")))
  data <- make_mock_data(m)
  expect_error(greedy_match(101, "pop", "XX", data = data), "NA values")
})
