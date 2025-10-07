# TODO:
# If a qualification plan is input
# Use getProjectsFromQualification to update current project list from snapshot IDs
# -> All data in the union default style
# -> Data from snapshots not in Union = Removed -> Red color
# -> Data from quali not in Union = New -> Green color ?
#   -> Data includes, projects, observed data sets, simulations, simulation outputs, simulation observed data
# Use quali data to fill remaining Excel sheets

#' @title excelUI
#' @param fileName Name of the Excel file to be created
#' @param snapshotPaths
#' List of project snapshots given by their URL or relative path
#' @param observedDataPaths
#' List of observed data sets (which are not included into the projects)
#' given by their URL or relative path
#' @param excelTemplate
#' Default excel template (only captions and lookup tables)
#' @param qualificationPlan
#' Optional: existing qualification plan.
#' If `NULL`, at least 1 project must be included into the projects list above
#' @param toRemove  Optional: projects to remove? Optional: observed data sets to remove?
#' @import openxlsx
#' @import jsonlite
#' @export
excelUI <- function(fileName = "qualification.xlsx",
                    snapshotPaths,
                    observedDataPaths,
                    excelTemplate = NULL,
                    qualificationPlan = NULL,
                    toRemove = NULL) {
  ospsuite.utils::validateIsFileExtension(fileName, "xlsx")
  excelTemplate <- excelTemplate %||%
    system.file("Qualification-Template.xlsx", package = "ospsuite.qualificationplaneditor")
  ospsuite.utils::validateIsFileExtension(excelTemplate, "xlsx")
  if(!file.exists(excelTemplate)){
    cli::cli_abort("excelTemplate: {.file {excelTemplate}} does not exist")
  }
  stopifnot(file.exists(excelTemplate))
  file.copy(from = excelTemplate, to = fileName, overwrite = TRUE)
  excelObject <- openxlsx::loadWorkbook(fileName)
  use_qualification <- !is.null(qualificationPlan)

  # MetaInfo ?

  # Projects
  projectData <- getProjectsFromList(snapshotPaths)
  # Qualification Plan provided
  qualificationProjects <- NULL
  if (use_qualification) {
    qualificationContent <- jsonlite::fromJSON(qualificationPlan, simplifyVector = FALSE)
    qualificationProjectData <- getProjectsFromQualification(qualificationContent)
    qualificationObservedData <- getObsDataFromQualification(qualificationContent)
    qualificationBBData <- getBBDataFromQualification(qualificationContent)

    qualificationProjects <- qualificationProjectData$ID
    commonProjects <- intersect(projectData$ID, qualificationProjects)
    # Merge to project data
    projectData <- merge.data.frame(projectData, qualificationProjectData, by = c("ID", "Path"), all = TRUE)
    projectStyles <- getQualificationStyles(
      data = projectData,
      commonProjects = commonProjects,
      qualificationProjects = qualificationProjects,
      projectVariable = "ID"
    )
  }

  writeDataToSheet(data = projectData, sheetName = "Projects", excelObject = excelObject)
  if (use_qualification) {
    styleQualificationCells(
      qualificationStyles = projectStyles,
      columnIndices = 1:ncol(projectData),
      sheetName = "Projects",
      excelObject = excelObject
    )
  }

  # Simulation Ouptuts
  simulationsOutputs <- getSimulationsOutputsFromProjects(projectData)
  writeDataToSheet(data = simulationsOutputs, sheetName = "Simulations_Outputs", excelObject = excelObject)
  if (use_qualification) {
    simulationsOutputStyles <- getQualificationStyles(
      data = simulationsOutputs,
      commonProjects = commonProjects,
      qualificationProjects = qualificationProjects
    )
    styleQualificationCells(
      qualificationStyles = simulationsOutputStyles,
      columnIndices = 1:ncol(simulationsOutputs),
      sheetName = "Simulations_Outputs",
      excelObject = excelObject
    )
  }

  # Simulations ObsData
  simulationsObsData <- getSimulationsObsDataFromProjects(projectData)
  writeDataToSheet(data = simulationsObsData, sheetName = "Simulations_ObsData", excelObject = excelObject)
  if (use_qualification) {
    simulationsObsDataStyles <- getQualificationStyles(
      data = simulationsObsData,
      commonProjects = commonProjects,
      qualificationProjects = qualificationProjects
    )
    styleQualificationCells(
      qualificationStyles = simulationsObsDataStyles,
      columnIndices = 1:ncol(simulationsObsData),
      sheetName = "Simulations_ObsData",
      excelObject = excelObject
    )
  }

  # Obs Data
  observedData <- getObsDataFromList(observedDataPaths)
  # Qualification Plan provided
  if (use_qualification) {
    commonObsData <- intersect(observedData$ID, qualificationObservedData$ID)
    # Merge to observed data data
    observedData <- merge.data.frame(observedData, qualificationObservedData, by = c("ID", "Path", "Type"), all = TRUE)
    obsDataStyles <- getQualificationStyles(
      data = observedData,
      commonProjects = commonObsData,
      qualificationProjects = qualificationObservedData$ID,
      projectVariable = "ID"
    )
  }
  writeDataToSheet(data = observedData, sheetName = "ObsData", excelObject = excelObject)
  # 3rd column uses a drop down list
  openxlsx::dataValidation(
    excelObject,
    sheet = "ObsData",
    cols = 3,
    rows = 1 + seq_len(nrow(observedData)),
    type = "list",
    value = "'Lookup'!$L$2:$L$4"
  )

  # BB
  bbData <- getBBDataFromProjects(projectData, qualificationProjects)
  if (use_qualification) {
    bbData <- merge.data.frame(
      bbData,
      qualificationBBData,
      by = c("Project", "BB-Type", "BB-Name", "Parent-Project"),
      all = TRUE
    )
  }
  writeDataToSheet(data = bbData, sheetName = "BB", excelObject = excelObject)
  # 3rd column uses a drop down list
  openxlsx::dataValidation(
    excelObject,
    sheet = "BB",
    cols = 4,
    rows = 1 + seq_len(nrow(bbData)),
    type = "list",
    value = paste0("'Projects'!$A$2:$A$", 1 + nrow(projectData))
  )
  if (use_qualification) {
    bbDataStyles <- getQualificationStyles(
      data = bbData,
      commonProjects = commonProjects,
      qualificationProjects = qualificationProjects
    )
    styleQualificationCells(
      qualificationStyles = bbDataStyles,
      columnIndices = 1:ncol(bbData),
      sheetName = "BB",
      excelObject = excelObject
    )
  }

  # Following only applies if Qualification Plan is provided
  if (use_qualification) {
    # Sim Param
    # writeDataToSheet(
    #  data = getQualificationSimParam(qualificationContent),
    #  sheetName = "SimParam",
    #  excelObject = excelObject
    # )

    # Comparison Time (CT) Profile
    writeDataToSheet(
      data = getQualificationCTProfile(qualificationContent),
      sheetName = "CT_Profile",
      excelObject = excelObject
    )

    # CT Mapping
    ctMapping <- getQualificationCTMapping(qualificationContent)
    writeDataToSheet(
      data = ctMapping,
      sheetName = "CT_Mapping",
      excelObject = excelObject
    )
    # Color CT Mapping
    for (ctIndex in seq_along(ctMapping$Color)) {
      openxlsx::addStyle(
        excelObject,
        sheet = "CT_Mapping",
        style = openxlsx::createStyle(
          fgFill = ctMapping$Color[ctIndex],
          fontColour = ctMapping$Color[ctIndex]
        ),
        rows = 1 + ctIndex,
        cols = 8
      )
    }

    # DDI Ratio
    ddiRatio <- getQualificationDDIRatio(qualificationContent)
    writeDataToSheet(data = ddiRatio, sheetName = "DDI_Ratio", excelObject = excelObject)
    # TODO: handle dataValidation
    # Color DDI Ratios
    for (ddiIndex in seq_along(ddiRatio[["Group Color"]])) {
      openxlsx::addStyle(
        excelObject,
        sheet = "DDI_Ratio",
        style = openxlsx::createStyle(
          fgFill = ddiRatio[["Group Color"]][ddiIndex],
          fontColour = ddiRatio[["Group Color"]][ddiIndex]
        ),
        rows = 1 + ddiIndex,
        cols = 8
      )
    }

    # DDI Ratio Mapping
    writeDataToSheet(
      data = getQualificationDDIRatioMapping(qualificationContent),
      sheetName = "DDI_Ratio_Mapping",
      excelObject = excelObject
    )

    # Sections
    writeDataToSheet(
      data = getQualificationSections(qualificationContent),
      sheetName = "Sections",
      excelObject = excelObject
    )

    # Inputs
    writeDataToSheet(
      data = data.frame(
        Project = NA,
        "BB-Type" = NA,
        "BB-Name" = NA,
        "Section Reference" = NA,
        check.names = FALSE
      ),
      sheetName = "Inputs",
      excelObject = excelObject
    )

    # Global Plot Settings
    globalPlotSettings <- formatPlotSettings(qualificationContent$Plots$PlotSettings)
    writeDataToSheet(
      data = globalPlotSettings,
      sheetName = "GlobalPlotSettings",
      excelObject = excelObject
    )

    # GlobalAxes DDI PreVsObs
    # TODO: check if this is required for all AxesSettings
    ddiAxesSettings <- lapply(qualificationContent$Plots$AxesSettings$DDIRatioPlotsPredictedVsObserved, as.data.frame)
    ddiAxesSettings <- do.call("rbind", ddiAxesSettings)
    writeDataToSheet(
      data = ddiAxesSettings,
      sheetName = "GlobalAxes_DDI_PredVsObs",
      excelObject = excelObject
    )
  }
  # Save the final workbook
  openxlsx::saveWorkbook(excelObject, file = fileName, overwrite = TRUE)
  return(invisible())
}


#' @title writeDataToSheet
#' @description
#' Write a data.frame to a specific sheet in an Excel file
#' @param data A data.frame
#' @param sheetName Name of the sheet to write to
#' @param excelObject An openxlsx workbook object
#' @import openxlsx
#' @keywords internal
writeDataToSheet <- function(data, sheetName, excelObject) {
  openxlsx::writeDataTable(
    excelObject,
    sheet = sheetName,
    x = data,
    headerStyle = excelOptions$headerStyle,
    withFilter = TRUE
  )
  openxlsx::freezePane(excelObject, sheet = sheetName, firstRow = TRUE)
  return(invisible())
}

#' @title excelOptions
#' @description
#' List of default Excel options
#' @import openxlsx
#' @export
excelOptions <- list(
  headerStyle = openxlsx::createStyle(
    fgFill = "#ADD8E6",
    textDecoration = "Bold",
    border = "Bottom",
    fontColour = "black"
  ),
  newProjectStyle = openxlsx::createStyle(
    fgFill = "#A3FFA3",
    fontColour = "black"
  ),
  deletedProjectStyle = openxlsx::createStyle(
    fgFill = "#FF8884",
    fontColour = "black"
  )
)

#' @title AllBuildingBlocks
#' @description
#' Allowed Building Blocks values
#' @keywords internal
AllBuildingBlocks <- c(
  "Individual",
  "Population",
  "Compound",
  "Protocol",
  "Event",
  "Formulation", "ObserverSet", "ExpressionProfile", "Simulation"
)
