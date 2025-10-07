#' @title getProjectsFromList
#' @description
#' Get a data.frame of project IDs and Paths/URLs
#' @param snapshotPaths
#' List of project snapshots given by their URL or relative path
#' @return data.frame with columns `ID` and `Path`
#' @export
#' @examples
#'
#' # Get the project data from a list of paths
#' snapshotPaths <- list(
#'   "Raltegravir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Raltegravir-Model",
#'     "v1.2",
#'     "Raltegravir-Model.json"
#'   ),
#'   "Atazanavir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Atazanavir-Model",
#'     "v1.2",
#'     "Atazanavir-Model.json"
#'   )
#' )
#' getProjectsFromList(snapshotPaths)
#'
getProjectsFromList <- function(snapshotPaths) {
  # If named list, use the names to get IDs
  snapshotIDs <- names(snapshotPaths)
  # If not, IDs = basename of the path
  if (is.null(snapshotIDs)) {
    snapshotIDs <- sapply(
      snapshotPaths,
      function(snapshotPath) {
        gsub(
          pattern = "(-Model.json|.json)",
          replacement = "",
          basename(snapshotPath)
        )
      }
    )
  }
  projectData <- data.frame(
    ID = snapshotIDs,
    Path = unlist(snapshotPaths),
    row.names = NULL
  )
  return(projectData)
}

#' @title getObsDataFromList
#' @description
#' Get a data.frame of observed data IDs and Paths/URLs
#' @param observedDataPaths
#' List of observed data paths and types
#' @return data.frame with columns `ID`, `Path` and `Type`
#' @export
#' @import ospsuite.utils
#' @examples
#' # Get the project data from a list of paths
#' observedDataPaths <- list(
#'   "A" = "ObsData/A.csv",
#'   "B" = "ObsData/B.csv",
#'   "A-B-DDI" = list(Path = "Projects/A-B-DDI.csv", Type = "DDIRatio")
#' )
#' getObsDataFromList(observedDataPaths)
#'
getObsDataFromList <- function(observedDataPaths) {
  if (ospsuite.utils::isEmpty(observedDataPaths)) {
    return(NULL)
  }
  # If named list, use the names to get IDs
  observedDataIDs <- names(observedDataPaths)
  # If not, IDs = basename of the path
  if (is.null(observedDataIDs)) {
    observedDataIDs <- sapply(
      observedDataPaths,
      function(observedDataPath) {
        gsub(
          pattern = ".csv",
          replacement = "",
          basename(observedDataPath)
        )
      }
    )
  }
  observedDataTypes <- lapply(
    observedDataPaths,
    function(observedDataPath) {
      if (ospsuite.utils::isOfType(observedDataPath, "list")) {
        obsData <- data.frame(
          Path = observedDataPath$Path,
          Type = observedDataPath$Type %||% "TimeProfile"
        )
        return(obsData)
      }
      obsData <- data.frame(
        Path = observedDataPath,
        Type = "TimeProfile"
      )
      return(obsData)
    }
  )
  observedDataTypes <- do.call(rbind, observedDataTypes)
  projectData <- data.frame(
    ID = observedDataIDs,
    observedDataTypes,
    row.names = NULL
  )
  return(projectData)
}

#' @title getBBFromSnapshot
#' @description
#' Get a list of simulations from a project snapshot
#' @param snapshotPath A path to a project snapshot (JSON file)
#' @param bbType A building block type (e.g. `"Simulations"`, `"Individuals"`)
#' @return A list of simulations information
#' @keywords internal
getBBFromSnapshot <- function(snapshotPath, bbType = "Simulations") {
  jsonlite::fromJSON(snapshotPath, simplifyVector = FALSE)[[bbType]]
}

#' @title getSimulationsFromSnapshot
#' @description
#' Get a list of simulations from a project snapshot
#' @param snapshotPath A data.frame of project snapshots
#' @return A list of simulations information
#' @keywords internal
getSimulationsFromSnapshot <- function(snapshotPath) {
  getBBFromSnapshot(snapshotPath, bbType = "Simulations")
}

#' @title getSimulationsOutputsFromProjects
#' @description
#' Get a data.frame of projects, simulations and outputs
#' @param projectData A data.frame of project snapshots
#' @return A data.frame with columns `Project`, `Simulation` and `Output`
#' @export
#' @examples
#'
#' # Get the project data from a list of paths
#' snapshotPaths <- list(
#'   "Raltegravir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Raltegravir-Model",
#'     "v1.2",
#'     "Raltegravir-Model.json"
#'   ),
#'   "Atazanavir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Atazanavir-Model",
#'     "v1.2",
#'     "Atazanavir-Model.json"
#'   )
#' )
#' projectData <- getProjectsFromList(snapshotPaths)
#'
#' # Get the simulations outputs from the projects
#' getSimulationsOutputsFromProjects(projectData)
#'
getSimulationsOutputsFromProjects <- function(projectData) {
  projectOutputsData <- data.frame()
  for (projectIndex in 1:nrow(projectData)) {
    snapshotSimulations <- getSimulationsFromSnapshot(projectData$Path[projectIndex])
    simOutputsData <- lapply(
      snapshotSimulations,
      function(snapshotSimulation) {
        outputSelections <- unlist(snapshotSimulation$OutputSelections)
        if (is.null(outputSelections)) {
          return(NULL)
        }
        data.frame(
          Project = projectData$ID[projectIndex],
          Simulation = snapshotSimulation$Name,
          Output = outputSelections,
          row.names = NULL
        )
      }
    )
    simOutputsData <- do.call(rbind, simOutputsData)
    projectOutputsData <- rbind(projectOutputsData, simOutputsData)
  }
  return(projectOutputsData)
}

#' @title getSimulationsObsDataFromProjects
#' @description
#' Get a data.frame of projects, simulations and observed data
#' @param projectData A data.frame of project snapshots
#' @return A data.frame with columns `Project`, `Simulation` and `ObservedData`
#' @export
#' @examples
#'
#' # Get the project data from a list of paths
#' snapshotPaths <- list(
#'   "Raltegravir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Raltegravir-Model",
#'     "v1.2",
#'     "Raltegravir-Model.json"
#'   ),
#'   "Atazanavir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Atazanavir-Model",
#'     "v1.2",
#'     "Atazanavir-Model.json"
#'   )
#' )
#' projectData <- getProjectsFromList(snapshotPaths)
#'
#' # Get the simulations Observed Data from the projects
#' getSimulationsObsDataFromProjects(projectData)
#'
getSimulationsObsDataFromProjects <- function(projectData) {
  projectObsData <- data.frame()
  for (projectIndex in 1:nrow(projectData)) {
    snapshotSimulations <- getSimulationsFromSnapshot(projectData$Path[projectIndex])
    simObsData <- lapply(
      snapshotSimulations,
      function(snapshotSimulation) {
        observedData <- unlist(snapshotSimulation$ObservedData)
        if (is.null(observedData)) {
          return(NULL)
        }
        data.frame(
          "Project" = projectData$ID[projectIndex],
          "Simulation" = snapshotSimulation$Name,
          "Observed Data" = observedData,
          row.names = NULL,
          check.names = FALSE
        )
      }
    )
    simObsData <- do.call(rbind, simObsData)
    projectObsData <- rbind(projectObsData, simObsData)
  }
  return(projectObsData)
}

#' @title getBBDataFromProjects
#' @description
#' Get a data.frame of projects, type, name and parent project
#' @param projectData A data.frame of project snapshots
#' @param qualificationProjects Optional: a data.frame of building blocks from a qualification plan
#' @return A data.frame with columns `Project`, `Simulation` and `Output`
#' @export
#' @examples
#'
#' # Get the project data from a list of paths
#' snapshotPaths <- list(
#'   "Raltegravir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Raltegravir-Model",
#'     "v1.2",
#'     "Raltegravir-Model.json"
#'   ),
#'   "Atazanavir" = file.path(
#'     "https://raw.githubusercontent.com",
#'     "Open-Systems-Pharmacology",
#'     "Atazanavir-Model",
#'     "v1.2",
#'     "Atazanavir-Model.json"
#'   )
#' )
#' projectData <- getProjectsFromList(snapshotPaths)
#'
#' # Get the simulations Observed Data from the projects
#' getBBDataFromProjects(projectData)
#'
getBBDataFromProjects <- function(projectData, qualificationProjects = NULL) {
  projectBBData <- data.frame()
  for (projectIndex in 1:nrow(projectData)) {
    # If qualificationProjects is provided, check if the project is already in it
    if (projectData$ID[projectIndex] %in% qualificationProjects) {
      next
    }
    for (bbType in AllBuildingBlocks) {
      snapshotBBs <- getBBFromSnapshot(
        projectData$Path[projectIndex],
        bbType = paste0(bbType, "s")
      )
      bbData <- lapply(
        snapshotBBs,
        function(snapshotBB) {
          data.frame(
            "Project" = projectData$ID[projectIndex],
            "BB-Type" = bbType,
            "BB-Name" = snapshotBB$Name,
            "Parent-Project" = "",
            row.names = NULL,
            check.names = FALSE
          )
        }
      )
      bbData <- do.call(rbind, bbData)
      projectBBData <- rbind(projectBBData, bbData)
    }
  }
  return(projectBBData)
}
