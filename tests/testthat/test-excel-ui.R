testOutputFile <- "test-excel-ui.xlsx"

snapshotPaths <- list(
  "Raltegravir" = paste0(ospURL, "/Raltegravir-Model/v1.2/Raltegravir-Model.json"),
  "Atazanavir" = paste0(ospURL, "/Atazanavir-Model/v1.2/Atazanavir-Model.json")
)
observedDataPaths <- list(
  "A" = "path/to/A.csv",
  "B" = "path/to/B.csv",
  "A-B-DDI" = list(Path = "path/to/A-B-DDI.csv", Type = "DDIRatio")
)

test_that("excelUI stops if fileName is not Excel", {
  expect_error(
    excelUI(
      fileName = "test-excel-ui.txt",
      snapshotPaths = snapshotPaths, 
      observedDataPaths = observedDataPaths
    ),
    "(Provided file has extension)*(txt)*(while)*(xlsx)*(was expected instead)"
  )
})

test_that("excelUI stops if template does not exist or is not not Excel", {
  expect_error(
    excelUI(
      fileName = testOutputFile,
      snapshotPaths = snapshotPaths, 
      observedDataPaths = observedDataPaths,
      excelTemplate = "template.txt"
    ),
    "(Provided file has extension)*(txt)*(while)*(xlsx)*(was expected instead)"
  )
  expect_error(
    excelUI(
      fileName = testOutputFile,
      snapshotPaths = snapshotPaths, 
      observedDataPaths = observedDataPaths,
      excelTemplate = "template.xlsx"
    ),
    "(excelTemplate)*(template\\.xlsx)*(does not exist)"
  )
})

test_that("excelUI creates Excel workbook with appropriate tables in simple case", {
  excelUI(
    fileName = testOutputFile,
    snapshotPaths = snapshotPaths, 
    observedDataPaths = observedDataPaths
  )
  expect_true(file.exists(testOutputFile))
  expect_contains(readxl::excel_sheets(testOutputFile), c("Projects", "Simulations_Outputs", "ObsData"))
  
  projectData <- readxl::read_xlsx(testOutputFile, sheet = "Projects")
  simOutputData <- readxl::read_xlsx(testOutputFile, sheet = "Simulations_Outputs")
  obsData <- readxl::read_xlsx(testOutputFile, sheet = "ObsData")
  
  expect_equal(getProjectsFromList(snapshotPaths), as.data.frame(projectData))
  expect_equal(getSimulationsOutputsFromProjects(projectData), as.data.frame(simOutputData))
  expect_equal(getObsDataFromList(observedDataPaths), as.data.frame(obsData))
})
