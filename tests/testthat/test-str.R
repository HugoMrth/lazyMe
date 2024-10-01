test_that("Args are checked properly :", {
  expect_error(str_clean())
  expect_error(str_as_function())
  expect_error(str_index(string = "a"))
  expect_error(str_index(pattern = "a"))
})

test_that("Arg are handled properly when possible :", {
  expect_no_error(str_index(c("Bonjour", "un test de la fonction", "svp"), "o", mode = c("first", "all")))
})

test_that("Output type is right :", {
  expect_type(str_as_function("mean", 1:15), "double")
  expect_type(str_clean("    bONjour & au Revoir    é     "), "character")
  expect_type(str_index(c("Bonjour", "un test de la fonction", "svp"), "o"), "list")
  expect_type(str_index(c("Bonjour", "un test de la fonction", "svp"), "o")[[1]], "integer")
  expect_type(str_index("Bonjour, un test de la fonction svp", "o"), "list")
  expect_type(str_index("Bonjour, un test de la fonction svp", "o")[[1]], "integer")
  expect_type(str_index("Bonjour, un test de la fonction svp", "o", "first"), "integer")
})

test_that("Outupt are coherent :", {
  expect_equal(str_as_function("mean", 1:15), 8)

  expect_equal(str_clean("    bONjour & au Revoir    é     "), "bonjour & au revoir    e")

  expect_equal(str_index("Bonjour, un test de la fonction svp", "o", "first"), 2)
  expect_equal(str_index(c("Bonjour", "un test de la fonction", "svp"), "o", "first"), c(2, 16, -1))
})
