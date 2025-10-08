test_that("getProjectsFromQualification handles empty Projects list", {
  qualificationContent <- list(Projects = list())
  
  result <- getProjectsFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("ID", "Path"))
  expect_type(result$ID, "character")
  expect_type(result$Path, "character")
})

test_that("getProjectsFromQualification handles NULL Projects", {
  qualificationContent <- list(Projects = NULL)
  
  result <- getProjectsFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("ID", "Path"))
})

test_that("getProjectsFromQualification works with valid Projects", {
  qualificationContent <- list(
    Projects = list(
      list(Id = "Project1", Path = "path/to/project1.json"),
      list(Id = "Project2", Path = "path/to/project2.json")
    )
  )
  
  result <- getProjectsFromQualification(qualificationContent)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$ID, c("Project1", "Project2"))
  expect_equal(result$Path, c("path/to/project1.json", "path/to/project2.json"))
})

test_that("getObsDataFromQualification handles empty ObservedDataSets list", {
  qualificationContent <- list(ObservedDataSets = list())
  
  result <- getObsDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("ID", "Path", "Type"))
  expect_type(result$ID, "character")
  expect_type(result$Path, "character")
  expect_type(result$Type, "character")
})

test_that("getObsDataFromQualification handles NULL ObservedDataSets", {
  qualificationContent <- list(ObservedDataSets = NULL)
  
  result <- getObsDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("ID", "Path", "Type"))
})

test_that("getObsDataFromQualification works with valid ObservedDataSets", {
  qualificationContent <- list(
    ObservedDataSets = list(
      list(Id = "Obs1", Path = "path/to/obs1.csv", Type = "TimeProfile"),
      list(Id = "Obs2", Path = "path/to/obs2.csv", Type = "DDIRatio")
    )
  )
  
  result <- getObsDataFromQualification(qualificationContent)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$ID, c("Obs1", "Obs2"))
  expect_equal(result$Type, c("TimeProfile", "DDIRatio"))
})

test_that("getBBDataFromQualification handles empty Projects list", {
  qualificationContent <- list(Projects = list())
  
  result <- getBBDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("Project", "BB-Type", "BB-Name", "Parent-Project"))
})

test_that("getBBDataFromQualification handles NULL Projects", {
  qualificationContent <- list(Projects = NULL)
  
  result <- getBBDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("getBBDataFromQualification handles Projects with no BuildingBlocks", {
  qualificationContent <- list(
    Projects = list(
      list(Id = "Project1", BuildingBlocks = NULL),
      list(Id = "Project2", BuildingBlocks = NULL)
    )
  )
  
  result <- getBBDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("Project", "BB-Type", "BB-Name", "Parent-Project"))
})

test_that("getBBDataFromQualification handles mixed Projects (some with BuildingBlocks, some without)", {
  qualificationContent <- list(
    Projects = list(
      list(Id = "Project1", BuildingBlocks = NULL),
      list(
        Id = "Project2",
        BuildingBlocks = list(
          list(Type = "Simulation", Name = "Sim1", Project = "")
        )
      )
    )
  )
  
  result <- getBBDataFromQualification(qualificationContent)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Project, "Project2")
  expect_equal(result$`BB-Type`, "Simulation")
  expect_equal(result$`BB-Name`, "Sim1")
})

test_that("getBBDataFromQualification works with valid Projects with BuildingBlocks", {
  qualificationContent <- list(
    Projects = list(
      list(
        Id = "Project1",
        BuildingBlocks = list(
          list(Type = "Individual", Name = "Individual1", Project = "Parent1")
        )
      ),
      list(
        Id = "Project2",
        BuildingBlocks = list(
          list(Type = "Simulation", Name = "Sim1", Project = ""),
          list(Type = "Compound", Name = "Compound1", Project = "Parent2")
        )
      )
    )
  )
  
  result <- getBBDataFromQualification(qualificationContent)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$Project, c("Project1", "Project2", "Project2"))
  expect_equal(result$`BB-Type`, c("Individual", "Simulation", "Compound"))
})
