test_that("Args are checked properly :", {
  expect_error(mean_trunc())
  expect_error(nb.decimal())
  expect_error(arrondi())
  expect_error(rescale_norm())
  expect_error(mean_trunc(rnorm(100)))
})

test_that("Arg are converted when possible :", {
  expect_no_error(nb.decimal("1.1"))
})

# test_that("Runs properly :", {
#
# })

test_that("Output type are right :", {
  expect_type(nb.decimal(1.23), "integer")
  expect_type(arrondi(1.23, 1), "double")
  expect_type(mean_trunc(rnorm(100), sides = "right"), 'double')
  expect_type(rescale_norm(rnorm(100, 1, 2)), 'double')
})

test_that("Outputs are coherent :", {
  expect_equal(min(rescale_norm(rnorm(100, 1, 2))), 0)
  expect_equal(max(rescale_norm(rnorm(100, 1, 2))), 1)
  expect_equal(arrondi(1.2, 0), 1)
  expect_equal(arrondi(1.7, 0), 2)
  expect_equal(arrondi(1.5, 0), 2)
  expect_equal(nb.decimal(1.5), 1)
  expect_equal(nb.decimal(-1.256), 3)
  x <- rnorm(95)
  expect_equal(mean_trunc(c(x, rep (1000, 5)), sides = "right"), mean(x))
})
