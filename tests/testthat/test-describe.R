test_that("Arg are check properly :", {
  expect_error(dataDesc::describe(iris, num.type = "error"))
  expect_error(dataDesc::describe(iris, prop.type = "error"))
  expect_error(dataDesc::describe(iris, prop.test = "error"))
  expect_error(dataDesc::describe(iris, mean.test = "error"))
  expect_error(dataDesc::describe(iris, conf.method.cat = "error"))
  expect_error(dataDesc::describe(iris, conf.method.num = "error"))
  expect_error(dataDesc::describe(iris, na.num.default = "error"))
  expect_error(dataDesc::describe(iris, na.str.default = "error"))
  expect_error(dataDesc::describe(iris, p.adjust.method = "error"))
  expect_error(dataDesc::describe(iris, lang = "error"))


  expect_error(dataDesc::describe())
  expect_error(dataDesc::describe(as.matrix(data)))

  expect_warning(dataDesc::describe(iris))
  expect_error(dataDesc::describe(iris, vars = "error"))
  expect_error(dataDesc::describe(iris, vars = 1:4, factor = c("Species", "Species")))
  expect_message(dataDesc::describe(iris, vars= 1:4))
  expect_message(dataDesc::describe(iris, vars= 1:4, include.p = TRUE))
  expect_message(dataDesc::describe(iris, vars= 1:4, include.test.name = TRUE))

  expect_error(dataDesc::describe(iris2, vars = 1:4, factor = 5,  weights = "Weights", num.type = "both"))

  expect_warning(dataDesc::describe(iris, vars = c("Sepal.Length", "Sepal.Width", "Sepal.Color"), factor = 'Species'))
  expect_warning(dataDesc::describe(iris, vars = 1:4, factor = 5, label = c("Lab1", "Lab2")))
  expect_message(dataDesc::describe(iris, vars = 1:2, factor = 5, label = c("Lab", "Lab")))

  expect_error(dataDesc::describe(iris, vars = 1:2, factor = 5, na.str.default = "value"))
  expect_error(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.default = "value"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.str.value = "missing"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.value = "missing"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.value = "mode"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.value = "value"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.default = "mean"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.default = "med"))
  expect_error(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.default = "value"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, na.num.default = "value", na.num.value = "mis"))

  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.level = 1.5))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.level = -0.5))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.level = "0.95"))

  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, chi.correct = "Yes"))

  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, decimal = 1.5))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, decimal = -1))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, decimal = "1"))


  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, cut.pvalue = 1.5))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, cut.pvalue = -1))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, cut.pvalue = "1"))

  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, include.n = "Yes"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, include.tot = "Yes"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, include.conf = "Yes"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, include.minmax = "Yes"))
  expect_warning(dataDesc::describe(iris, vars = 1:2, factor = 5, include.test.name = "Yes"))
  expect_error(dataDesc::describe(iris, vars = 1:2, factor = 5, p.adjust = "Yes"))
})


test_that("Output is a data frame :", {
  expect_s3_class(dataDesc::describe(iris, vars = 1:2, factor = 5), "data.frame")
})

test_that("Number of columns is right :", {
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, factor = 5)) == 7)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, factor = 5, include.p = FALSE)) == 6)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, factor = 5, include.test.name = TRUE)) == 8)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, factor = 5, merge.cols = FALSE)) == 15)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, factor = 5, merge.cols = FALSE, include.tot = FALSE)) == 12)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2)) == 3)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, include.n = FALSE)) == 2)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, merge.cols = FALSE)) == 5)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, include.p = TRUE, include.test.name = TRUE)) == 3)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, include.minmax = TRUE)) == 3)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, include.minmax = TRUE, merge.cols = FALSE)) == 6)
  expect_true(ncol(dataDesc::describe(iris, vars = 1:2, merge.cols = FALSE, include.conf = FALSE)) == 4)
})

test_that("colnames are coherent :", {
  expect_true(all(colnames(dataDesc::describe(iris, vars = 1:2, factor = 5)) ==
                c("Var", "N", "Total n (%) [IC95%]", "setosa n (%) [IC95%]",
                  "versicolor n (%) [IC95%]", "virginica n (%) [IC95%]", "pval")))
  expect_true(all(colnames(dataDesc::describe(iris, vars = 1:2, factor = 5, merge.cols = FALSE)) ==
                c("Var", "N", "Total n", "Total (%)", "Total IC 95%",
                  "setosa n", "setosa (%)", "setosa IC 95%",
                  "versicolor n", "versicolor (%)", "versicolor IC 95%",
                  "virginica n", "virginica (%)", "virginica IC 95%",
                  "pval")))
})

test_that("CI methods", {
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "waldcc"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "sisonglaz"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "cplus1"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "goodman"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "wald"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "wilson"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.cat = "boot"))

  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.num = "classic"))
  expect_no_error(dataDesc::describe(iris, vars = 1:2, factor = 5, conf.method.num = "boot"))
})

test_that("testing methods:", {
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, mean.test = "test"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, mean.test = "student"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, mean.test = "kruskal"))

  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.test = "test"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.test = "chi"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.test = "fisher"))
})

test_that("p-adjust methods:", {
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "BH"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "holm"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "hochberg"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "hommel"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "bonferroni"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "BY"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "fdr"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, p.adjust = TRUE, p.adjust.method = "none"))
})

test_that("na methods:", {
  expect_warning(dataDesc::describe(iris_na, vars = 1:5, factor = 5))


  expect_no_error(dataDesc::describe(iris_na, vars = 1:5, factor = 5, na.omit = FALSE))
  expect_no_error(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.str.default = "missing"))
  expect_no_error(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.str.default = "mode"))
  expect_no_error(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.str.default = "value", na.str.value = "setosa"))
  expect_no_error(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.num.default = "none"))
  expect_warning(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.num.default = "mean"))
  expect_warning(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.num.default = "med"))
  expect_warning(dataDesc::describe(iris_na, vars = 1:5, factor = 5,
                                     na.omit = FALSE, na.num.default = "value", na.num.value = 1000))
})

test_that("margins methods:", {
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, num.type = "mean"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, num.type = "both"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, num.type = "med"))

  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.type = "col.percent"))
  expect_no_error(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.type = "row.percent"))
  expect_warning(dataDesc::describe(iris, vars = 1:5, factor = 5, prop.type = "tot.percent"))
})

test_that("language :", {
  expect_equal(dataDesc::describe(iris, vars = 1:5, factor = 5, lang = "fr")[,1],
               c("Sepal.Length, moyenne (ET)", "Sepal.Width, moyenne (ET)",
                 "Petal.Length, moyenne (ET)", "Petal.Width, moyenne (ET)",
                 "Species", "  setosa", "  versicolor","  virginica"))
  expect_equal(dataDesc::describe(iris, vars = 1:5, factor = 5, lang = "en")[,1],
               c("Sepal.Length, mean (SD)", "Sepal.Width, mean (SD)",
                 "Petal.Length, mean (SD)", "Petal.Width, mean (SD)",
                 "Species", "  setosa", "  versicolor","  virginica"))

  expect_equal(colnames(dataDesc::describe(iris, vars = 1:2, factor = 5, lang = "en")),
                    c("Var", "N", "Total n (%) [CI95%]", "setosa n (%) [CI95%]",
                      "versicolor n (%) [CI95%]", "virginica n (%) [CI95%]", "pval"))
  expect_equal(colnames(dataDesc::describe(iris, vars = 1:2, factor = 5, merge.cols = FALSE, lang = "en")),
                    c("Var", "N", "Total n", "Total (%)", "Total CI 95%",
                      "setosa n", "setosa (%)", "setosa CI 95%",
                      "versicolor n", "versicolor (%)", "versicolor CI 95%",
                      "virginica n", "virginica (%)", "virginica CI 95%",
                      "pval"))
})
