test_that("Arg are check properly :", {
  expect_message(tidyDesc_binary(desc11))

  expect_no_error(tidyDesc_binary(desc11))
  expect_no_error(tidyDesc_censorLowFreq(desc21))
})

test_that("Output is a data frame :", {
  expect_s3_class(desc12, "data.frame")
  expect_s3_class(desc22, "data.frame")
})

test_that("Number of columns is right :", {
  expect_equal(colnames(desc11), colnames(desc12))
  expect_equal(rownames(desc11)[1:5], rownames(desc12))
  expect_equal(colnames(desc21), colnames(desc22))
  expect_equal(rownames(desc21), rownames(desc22))

  expect_true(all(desc22[6:8,3] == "< 10"))
  expect_equal(desc21, tidyDesc_censorLowFreq(desc21, threshold = 1))
  expect_equal(desc1, tidyDesc_binary(desc1))
  expect_equal(desc2, tidyDesc_binary(desc2))
  expect_equal(desc3, tidyDesc_binary(desc3))
  expect_equal(desc1, tidyDesc_censorLowFreq(desc1))
  expect_equal(desc2, tidyDesc_censorLowFreq(desc2, threshold = 0))
  expect_equal(desc3, tidyDesc_censorLowFreq(desc3, threshold = 0))
})
