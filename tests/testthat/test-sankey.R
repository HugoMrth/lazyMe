test_that("Output are as expected : ", {
  data <- data.frame(
    T1 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE),
    T2 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE),
    T3 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE)
  )

  plotData <- createSankeyData(data,
                               c("State 1", "State 2", "State 3"),
                               c("T1", "T2", "T3"))

  expect_type(plotData, "list")
  expect_equal(nrow(plotData$Nodes), 3*3)
  expect_equal(ncol(plotData$Nodes), 2)
  expect_equal(nrow(plotData$Links), 3*3*(3-1))
  expect_equal(ncol(plotData$Links), 4)
})
