test_that("simple", {
  time <- 1:3
  query <- matrix(rep(0,3), ncol=1)
  target <- matrix(0:2, ncol=1)
  scale <- 1
  result <- minDistTimeState(query, target, time, scale)
  expect_equal(result, c(0, 1, sqrt(2)))

  time <- 1:3
  query <- matrix(rep(0,6), ncol=2)
  target <- matrix(0:5, ncol=2)
  scale <- 0.5
  result <- minDistTimeState(query, target, time, scale)
  expect_equal(result, sqrt(c(9, 9.25, 10)))

  scale <- 1e3
  result <- minDistTimeState(query, target, time, scale)
  expect_equal(result, sqrt(rowSums((query-target)^2)))
})
