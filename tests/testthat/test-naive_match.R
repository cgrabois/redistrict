test_that("naive_match pairs districts by number alone", {
  m <- matrix(1:6, nrow = 2, dimnames = list(c("XX-1","XX-2"), c("XX-1","XX-2","XX-3")))
  data <- make_mock_data(m)

  result <- naive_match(101, "pop", "XX", data = data)

  expect_equal(result$target[result$source %in% "XX-1"], "XX-1")
  expect_equal(result$target[result$source %in% "XX-2"], "XX-2")
  expect_true(is.na(result$source[result$target %in% "XX-3"]))
})

test_that("naive_match ignores overlap values entirely", {
  m1 <- matrix(c(100, 0, 0, 100), 2, 2, dimnames = list(c("XX-1","XX-2"), c("XX-1","XX-2")))
  m2 <- matrix(c(0, 100, 100, 0), 2, 2, dimnames = list(c("XX-1","XX-2"), c("XX-1","XX-2")))

  result1 <- naive_match(101, "pop", "XX", data = make_mock_data(m1))
  result2 <- naive_match(101, "pop", "XX", data = make_mock_data(m2))

  expect_equal(result1, result2)
})

test_that("naive_match validates its inputs", {
  expect_error(naive_match("abc", "pop", "CA"), "numeric")
  expect_error(naive_match(111, "nonsense", "CA"))
})
