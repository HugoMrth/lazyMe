test_that("Args are checked properly :", {
  expect_error(convertAll(iris, "integer", "error"))
  expect_error(convertAll(iris, "error", "integer"))
  expect_error(convertAll(iris, "factor", "factor"))

  expect_error(relevel_factor())

  expect_error(as.binary(x1, true.val = "Maybe"))
  expect_error(as.binary(x1, true.val = "Yes", lang = "por"))
  expect_error(as.binary(x1, true.val = "Yes", values = "int"))
  expect_error(as.binary(x2, true.val = "Yes"))
  })


test_that("Runs properly :", {
  expect_no_error(convertAll(iris, "factor", "numeric"))
  expect_no_error(convertAll(iris, "numeric", "character"))

  expect_no_error(relevel_factor(iris$Species,
                                 new.levels = list(
                                   "setosa" = "Setosa",
                                   "versicolor" = "Versicolor",
                                   "virginica" = "Virginica"
                                 )))
  expect_no_error(relevel_factor(iris$Species,
                                 new.levels = cbind(
                                   c("setosa", "versicolor", "virginica"),
                                   c("Setosa", "Versicolor", "Virginica")
                                 )))
  expect_no_error(relevel_factor(iris$Species, ref = "virginica"))

  expect_no_error(as.binary(x1, true.val = "Yes"))
  expect_no_error(as.binary(x1, true.val = "No"))
  expect_no_error(as.binary(x1, true.val = "Yes", values = "numeric"))
  expect_no_error(as.binary(x1, true.val = "Yes", values = "factor"))
  expect_no_error(as.binary(x1, true.val = "Yes", values = "factor", ref = "pos"))
  expect_no_error(as.binary(x1, true.val = "Yes", values = "factor", lang = "en"))
  expect_no_error(as.binary(x3, true.val = "Yes"))
  expect_no_error(as.binary(x3, true.val = "Yes", na.ignore = FALSE))
})

test_that("Output type are right :", {
  expect_s3_class(convertAll(iris, "factor", "numeric"), "data.frame")
  expect_s3_class(convertAll(iris, "numeric", "character"), "data.frame")

  expect_s3_class(relevel_factor(iris$Species), "factor")

  expect_true(is.logical(as.binary(x1, true.val = "Yes")))
  expect_true(is.logical(as.binary(x1, true.val = "Yes", values = "factor", lang = "en")))
})

test_that("Outputs are coherent :", {
  expect_true(all(apply(convertAll(iris, "numeric", "character"), 2, class) == "character"))
  expect_true(all(apply(convertAll(iris, "factor", "numeric"), 2, class) == "numeric"))

  expect_equal(iris$Species, relevel_factor(iris$Species))
  expect_true(all(iris$Species == relevel_factor(iris$Species, ref = "virginica")))
})
