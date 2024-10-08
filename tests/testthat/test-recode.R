test_that("Args are checked properly :", {
  expect_error(spread_mult(key = "id", value = c("x1", "x2")))
  expect_error(spread_mult(df = data_spread, value = c("x1", "x2")))
  expect_error(spread_mult(df = data_spread, key = "id"))
})

test_that("Arg are converted when possible :", {
  expect_no_error(spread_mult(df = as.matrix(data_spread), key = "id", value = c("x1", "x2")))
})

test_that("Runs properly :", {
  expect_no_error(unaggregate(data_agg, "N"))
  expect_no_error(spread_mult(df = data_spread, key = "id", value = c("x1", "x2")))
})

test_that("Output type are right :", {
  expect_s3_class(unaggregate(data_agg, "N"), "data.frame")
  expect_s3_class(spread_mult(df = data_spread, key = "id", value = c("x1", "x2")), "data.frame")
})

test_that("Outputs are coherent :", {
  expect_equal(ncol(unaggregate(data_agg, "N")), ncol(data_agg)-1)
  expect_equal(nrow(unaggregate(data_agg, "N")), sum(as.numeric(data_agg$N)))


  expect_equal(ncol(spread_mult(df = data_spread, key = "id", value = c("x1", "x2"))), ncol(data_agg)+1)
  expect_equal(nrow(spread_mult(df = data_spread, key = "id", value = c("x1", "x2"))), nrow(data_spread)/2)
})
