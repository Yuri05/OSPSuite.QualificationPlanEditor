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
  # Strip trailing '-Model.json' or '.json' from filenames to derive IDs
  if (is.null(snapshotIDs)) {
    snapshotIDs <- vapply(
      snapshotPaths,
      function(snapshotPath) {
        gsub(
          pattern = "(-Model\\.json|\\.json)$",
          replacement = "",
          basename(snapshotPath)
        )
      },
      FUN.VALUE = character(1)
    )
  }
  projectData <- data.frame(
    ID = snapshotIDs,
    Path = unlist(snapshotPaths),
    row.names = NULL,
    stringsAsFactors = FALSE
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
  # If not, IDs = basename of the path (strip trailing '.csv')
  if (is.null(observedDataIDs)) {
    observedDataIDs <- vapply(
      observedDataPaths,
      function(observedDataPath) {
        gsub(
          pattern = "\\.csv$",
          replacement = "",
          basename(observedDataPath)
        )
      },
      FUN.VALUE = character(1)
    )
  }
  observedDataTypes <- lapply(
    observedDataPaths,
    function(observedDataPath) {
      if (ospsuite.utils::isOfType(observedDataPath, "list")) {
        obsData <- data.frame(
          Path = observedDataPath$Path,
          Type = observedDataPath$Type %||% "TimeProfile",
          stringsAsFactors = FALSE
        )
        return(obsData)
      }
      obsData <- data.frame(
        Path = observedDataPath,
        Type = "TimeProfile",
        stringsAsFactors = FALSE
      )
      return(obsData)
    }
  )
  observedDataTypes <- do.call(rbind, observedDataTypes)
  projectData <- data.frame(
    ID = observedDataIDs,
    observedDataTypes,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  return(projectData)
}

#' @title getBBFromSnapshot
#' @description
#' Get a list of building blocks from a project snapshot
#' @param snapshotPath A path/URL to a project snapshot (JSON file)
#' @param bbType A building block type (e.g. `"Simulations"`, `"Individuals"`)
#' @return A list of building block information
#' @keywords internal
getBBFromSnapshot <- function(snapshotPath, bbType = "Simulations") {
  tryCatch(
    {
      snapshot <- jsonlite::fromJSON(snapshotPath, simplifyVector = FALSE)
      return(snapshot[[bbType]])
    },
    error = function(e) {
      cli::cli_abort("Failed to read snapshot from {.file {snapshotPath}}: {e$message}")
    }
  )
}

#' @title getSimulationsFromSnapshot
#' @description
#' Get a list of simulations from a project snapshot
#' @param snapshotPath A path/URL to a project snapshot (JSON file)
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
  # Accumulate all rows in a list for efficiency
  allRows <- list()
  
  for (projectIndex in seq_len(nrow(projectData))) {
    snapshotSimulations <- getSimulationsFromSnapshot(projectData$Path[projectIndex])
    
    if (ospsuite.utils::isEmpty(snapshotSimulations)) {
      next
    }
    
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
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
    
    # Filter out NULL entries
    simOutputsData <- Filter(Negate(is.null), simOutputsData)
    
    # Add non-empty results to allRows
    if (length(simOutputsData) > 0) {
      allRows[(length(allRows) + seq_along(simOutputsData))] <- simOutputsData
    }
  }
  
  # Bind all rows at once
  if (length(allRows) == 0) {
    return(data.frame(
      Project = character(0),
      Simulation = character(0),
      Output = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  return(do.call(rbind, allRows))
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
  # Accumulate all rows in a list for efficiency
  allRows <- list()
  
  for (projectIndex in seq_len(nrow(projectData))) {
    snapshotSimulations <- getSimulationsFromSnapshot(projectData$Path[projectIndex])
    
    if (ospsuite.utils::isEmpty(snapshotSimulations)) {
      next
    }
    
    simObsData <- lapply(
      snapshotSimulations,
      function(snapshotSimulation) {
        observedData <- unlist(snapshotSimulation$ObservedData)
        if (is.null(observedData)) {
          return(NULL)
        }
        data.frame(
          Project = projectData$ID[projectIndex],
          Simulation = snapshotSimulation$Name,
          ObservedData = observedData,
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
    
    # Filter out NULL entries
    simObsData <- Filter(Negate(is.null), simObsData)
    
    # Add non-empty results to allRows
    if (length(simObsData) > 0) {
      allRows <- c(allRows, simObsData)
    }
  }
  
  # Bind all rows at once
  if (length(allRows) == 0) {
    return(data.frame(
      Project = character(0),
      Simulation = character(0),
      ObservedData = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  return(do.call(rbind, allRows))
}

#' @title getBBDataFromProjects
#' @description
#' Get a data.frame of projects, type, name and parent project
#' @param projectData A data.frame of project snapshots
#' @param qualificationProjects Optional: a character vector of project IDs from a qualification plan
#' @return A data.frame with columns `Project`, `BB-Type`, `BB-Name`, `Parent-Project`
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
  # Accumulate all rows in a list for efficiency
  allRows <- list()
  
  for (projectIndex in seq_len(nrow(projectData))) {
    # If qualificationProjects is provided, check if the project is already in it
    if (projectData$ID[projectIndex] %in% qualificationProjects) {
      next
    }
    
    # Parse the snapshot once per project
    snapshot <- tryCatch(
      {
        jsonlite::fromJSON(projectData$Path[projectIndex], simplifyVector = FALSE)
      },
      error = function(e) {
        cli::cli_abort("Failed to read snapshot for project {.val {projectData$ID[projectIndex]}} from {.file {projectData$Path[projectIndex]}}: {e$message}")
      }
    )
    
    for (bbType in AllBuildingBlocks) {
      # Get building blocks using pluralized key
      bbKey <- paste0(bbType, "s")
      snapshotBBs <- snapshot[[bbKey]]
      
      # Guard against NULL or empty building block lists
      if (ospsuite.utils::isEmpty(snapshotBBs)) {
        next
      }
      
      bbData <- lapply(
        snapshotBBs,
        function(snapshotBB) {
          data.frame(
            Project = projectData$ID[projectIndex],
            "BB-Type" = bbType,
            "BB-Name" = snapshotBB$Name,
            "Parent-Project" = "",
            row.names = NULL,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
      )
      
      # Filter out NULL entries
      bbData <- Filter(Negate(is.null), bbData)
      
      # Add non-empty results to allRows
      if (length(bbData) > 0) {
        allRows <- c(allRows, bbData)
      }
    }
  }
  
  # Bind all rows at once
  if (length(allRows) == 0) {
    return(data.frame(
      Project = character(0),
      "BB-Type" = character(0),
      "BB-Name" = character(0),
      "Parent-Project" = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  return(do.call(rbind, allRows))
}
