test_that("getObsDataFromList creates a data.frame with appropriate values", {
  observedDataPaths <- list(
    A = "path/to/A.csv",
    B = "path/to/B.csv"
  )
  expect_equal(
    getObsDataFromList(observedDataPaths),
    data.frame(
      ID = c("A", "B"),
      Path = c("path/to/A.csv", "path/to/B.csv"),
      Type = "TimeProfile",
      row.names = NULL
    )
  )
})

test_that("getObsDataFromList can parse IDs from paths", {
  observedDataPaths <- c("path/to/A.csv", "path/to/B.csv")
  expect_equal(
    getObsDataFromList(observedDataPaths),
    data.frame(
      ID = c("A", "B"),
      Path = c("path/to/A.csv", "path/to/B.csv"),
      Type = "TimeProfile",
      row.names = NULL
    )
  )
})

test_that("getObsDataFromList use Type information if defined", {
  observedDataPaths <- list(
    "A" = "path/to/A.csv",
    "B" = "path/to/B.csv",
    "A-B-DDI" = list(Path = "path/to/A-B-DDI.csv", Type = "DDIRatio")
  )
  expect_equal(
    getObsDataFromList(observedDataPaths),
    data.frame(
      ID = c("A", "B", "A-B-DDI"),
      Path = c("path/to/A.csv", "path/to/B.csv", "path/to/A-B-DDI.csv"),
      Type = c("TimeProfile", "TimeProfile", "DDIRatio"),
      row.names = NULL
    )
  )
})
