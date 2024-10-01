test_that("Works as intended :", {
  expect_true(is.defined(0))
  expect_true(is.defined(NA))
  expect_false(is.defined(NULL))

  expect_true(isnt.na(0))
  expect_false(isnt.na(NA))

  expect_true(all(c("a", "b", "c") %ni% c("d", "e", "f")))
  expect_true(all(not.in(c("a", "b", "c"), c("d", "e", "f"))))

  expect_equal(nna(iris$Species), 0)
  expect_equal(nna(rep(NA, 10)), 10)
  expect_equal(nnonna(iris$Species), 150)
  expect_equal(nnonna(rep(NA, 10)), 0)
  })


