# qualificationPath <- "C:/Design2Code/Qualification-Tests/qualification_plan.json"
# qualificationContent <- jsonlite::fromJSON(qualificationPath, simplifyVector = FALSE)

#' @title getProjectsFromQualification
#' @description
#' Get a data.frame of project IDs and Paths/URLs
#' @param qualificationContent Content of a qualification plan
#' @return data.frame with columns `ID` and `Path`
#' @export
getProjectsFromQualification <- function(qualificationContent) {
  projectData <- lapply(
    qualificationContent$Projects,
    function(projectContent) {
      data.frame(
        ID = projectContent$Id,
        Path = projectContent$Path,
        stringsAsFactors = FALSE
      )
    }
  )
  projectData <- do.call(rbind, projectData)
  return(projectData)
}

#' @title getObsDataFromQualification
#' @description
#' Get a data.frame of observed data IDs and Paths/URLs
#' @param qualificationContent Content of a qualification plan
#' @return data.frame with columns `ID`, `Path` and `Type`
#' @export
getObsDataFromQualification <- function(qualificationContent) {
  obsData <- lapply(
    qualificationContent$ObservedDataSets,
    function(obsDataContent) {
      data.frame(
        ID = obsDataContent$Id,
        Path = obsDataContent$Path,
        Type = obsDataContent$Type %||% "TimeProfile",
        stringsAsFactors = FALSE
      )
    }
  )
  obsData <- do.call(rbind, obsData)
  return(obsData)
}

#' @title getBBDataFromQualification
#' @description
#' Get a data.frame of projects, type, name and parent project
#' @param qualificationContent Content of a qualification plan
#' @return A data.frame with columns `Project`, `Simulation` and `Output`
#' @export
getBBDataFromQualification <- function(qualificationContent) {
  bbData <- lapply(
    qualificationContent$Projects,
    function(projectContent) {
      if (is.null(projectContent$BuildingBlocks)) {
        return()
      }
      projectBB <- do.call(rbind.data.frame, projectContent$BuildingBlocks)
      data.frame(
        "Project" = projectContent$Id,
        "BB-Type" = projectBB$Type,
        "BB-Name" = projectBB$Name,
        "Parent-Project" = projectBB$Project,
        row.names = NULL,
        check.names = FALSE
      )
    }
  )
  bbData <- do.call(rbind, bbData)
  return(bbData)
}

#' @title styleQualificationCells
#' @description
#' Apply qualification styles to cells in an Excel sheet
#' @param qualificationStyles A vector of qualification styles including the names `"New"` and `"Deleted"`
#' @param columnIndices Indices of the columns to apply the styles to
#' @param sheetName Name of the sheet to write to
#' @param excelObject An openxlsx workbook object
#' @import openxlsx
#' @keywords internal
styleQualificationCells <- function(qualificationStyles, columnIndices, sheetName, excelObject) {
  openxlsx::addStyle(
    excelObject,
    sheet = sheetName,
    style = excelOptions$newProjectStyle,
    rows = 1 + which(qualificationStyles %in% "New"),
    cols = columnIndices,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    excelObject,
    sheet = sheetName,
    style = excelOptions$deletedProjectStyle,
    rows = 1 + which(qualificationStyles %in% "Deleted"),
    cols = columnIndices,
    gridExpand = TRUE
  )
}

#' @title getQualificationStyles
#' @description Get qualification styles for a data.frame based on project membership
#' @param data A data.frame with a project variable
#' @param commonProjects A vector of project IDs that are common between the data and the qualification plan
#' @param qualificationProjects A vector of project IDs that are new in the qualification plan
#' @param projectVariable Name of the project variable in the data.frame
#' @keywords internal
getQualificationStyles <- function(data, commonProjects, qualificationProjects, projectVariable = "Project") {
  ifelse(
    data[[projectVariable]] %in% commonProjects,
    "Unchanged",
    ifelse(data[[projectVariable]] %in% qualificationProjects, "Deleted", "New")
  )
}

#' @title getQualificationSections
#' @description
#' Get a data.frame of qualification plan sections
#' @param qualificationContent Content of a qualification plan
#' @return A data.frame with `Section Reference`, `Title`, `Content` and `Parent Section` columns
#' @keywords internal
getQualificationSections <- function(qualificationContent) {
  return(parseSectionsToDataFrame(qualificationContent$Sections))
}

#' @title parseSectionsToDataFrame
#' @description
#' Parse qualification plan sections
#' @param sectionsIn A Section list including Reference, Title, Content and Sections fields
#' @param sectionsOut A data.frame to accumulate the parsed sections
#' @param parentSection A string representing the parent section reference
#' @return A data.frame
#' @keywords internal
parseSectionsToDataFrame <- function(sectionsIn, sectionsOut = data.frame(), parentSection = NA) {
  for (section in sectionsIn) {
    sectionOut <- data.frame(
      "Section Reference" = section$Reference,
      "Title" = section$Title,
      "Content" = section$Content %||% NA,
      "Parent Section" = parentSection,
      check.names = FALSE
    )
    sectionsOut <- rbind.data.frame(sectionsOut, sectionOut, stringsAsFactors = FALSE)
    # If subsections are included and not empty, update sectionsOut data.frame
    if (!is.null(section$Sections)) {
      sectionsOut <- parseSectionsToDataFrame(
        sectionsIn = section$Sections,
        sectionsOut = sectionsOut,
        parentSection = section$Reference
      )
    }
  }
  return(sectionsOut)
}


#' @title getQualificationSimParam
#' @description
#' Get a data.frame of project IDs and Paths/URLs
#' @param qualificationContent Content of a qualification plan
#' @return data.frame with columns `ID` and `Path`
#' @keywords internal
getQualificationSimParam <- function(qualificationContent) {
  # data.frame(
  #  Project	Parent Project	Parent Simulation	Path	TargetSimulation
  #
  # )
  return(data.frame())
}

#' @title getQualificationCTProfile
#' @description
#' Extract a data.frame containing comparison time (CT) profile information
#' from the qualification plan content
#' @param qualificationContent Content of a qualification plan
#' @return data.frame with columns
#' `Title`, `Section Reference`, `Simulation Duration`, `TimeUnit` and plot settings
#' @keywords internal
getQualificationCTProfile <- function(qualificationContent) {
  ctProfiles <- data.frame()
  for (ctPlot in qualificationContent$Plots$ComparisonTimeProfilePlots) {
    ctProfile <- cbind(
      data.frame(
        Title = ctPlot$Title,
        "Section Reference" = ctPlot$SectionReference,
        "Simulation Duration" = ctPlot$SimulationDuration,
        TimeUnit = ctPlot$TimeUnit,
        check.names = FALSE
      ),
      formatPlotSettings(ctPlot$PlotSettings),
      formatAxesSettings(ctPlot$AxesSettings)
    )
    ctProfiles <- rbind(ctProfiles, ctProfile)
  }
  return(ctProfiles)
}

#' @title getQualificationCTMapping
#' @description
#' Extract the comparison time (CT) mapping from a qualification plan,
#' returning a data.frame with mapping information for CT analysis.
#' @param qualificationContent Content of a qualification plan
#' @return A data.frame with columns
#' `Project`, `Simulation`, `Output` and relevant CT fields
#' @keywords internal
getQualificationCTMapping <- function(qualificationContent) {
  ctMappings <- data.frame()
  for (ctPlot in qualificationContent$Plots$ComparisonTimeProfilePlots) {
    for (outputMapping in ctPlot$OutputMappings) {
      ctMapping <- data.frame(
        Project = outputMapping$Project,
        Simulation = outputMapping$Simulation,
        Output = outputMapping$Output,
        "Observed data" = unlist(outputMapping$ObservedData) %||% NA,
        "Plot Title" = ctPlot$Title,
        StartTime = outputMapping$StartTime,
        TimeUnit = outputMapping$TimeUnit,
        Color = outputMapping$Color,
        Caption = outputMapping$Caption,
        Symbol = outputMapping$Symbol,
        check.names = FALSE
      )
      ctMappings <- rbind(ctMappings, ctMapping)
    }
  }
  return(ctMappings)
}

#' @title getQualificationDDIRatio
#' @description
#' Extract DDI ratio data from a qualification plan and return it as a data.frame with relevant columns
#' @param qualificationContent Content of a qualification plan
#' @return A data.frame with following columns:
#' `Title`, `Section Ref`, `PK-Parameter`, `Plot Type`, `Subunits`, `Artifacts` and legend settings
#' @keywords internal
getQualificationDDIRatio <- function(qualificationContent) {
  ddiRatios <- data.frame()
  for (ddiPlot in qualificationContent$Plots$DDIRatioPlots) {
    ddiPlotSettings <- list(
      Title = ddiPlot$Title,
      "Section Ref" = ddiPlot$SectionReference,
      "PK-Parameter" = unlist(ddiPlot$PKParameters),
      "Plot Type" = unlist(ddiPlot$PlotTypes),
      Subunits = unlist(ddiPlot$Subunits),
      Artifacts = unlist(ddiPlot$Artifacts),
      "Group Caption" = sapply(ddiPlot$Groups, function(group) group$Caption),
      "Group Color" = sapply(ddiPlot$Groups, function(group) group$Color),
      "Group Symbol" = sapply(ddiPlot$Groups, function(group) group$Symbol)
    )
    maxRows <- max(sapply(ddiPlotSettings, length))
    ddiPlotSettings <- sapply(
      ddiPlotSettings,
      function(ddiField) {
        ddiField <- c(ddiField, rep(NA, maxRows - length(ddiField)))
        return(ddiField)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
    ddiPlotSettings <- cbind(
      data.frame(ddiPlotSettings, check.names = FALSE),
      formatPlotSettings(ddiPlot$PlotSettings),
      formatAxesSettings(ddiPlot$AxesSettings)
    )
    ddiRatios <- rbind(ddiRatios, ddiPlotSettings)
  }
  return(ddiRatios)
}

#' @title getQualificationDDIRatioMapping
#' @description
#' Extract a data.frame mapping DDI ratio identifiers to relevant DDI Ratio fields
#' @param qualificationContent Content of a qualification plan
#' @return A data.frame with the following columns
#' `Project`, `Simulation_Control`, `Simulation_Treatment`, `Output` and control/treatment settings
#' @keywords internal
getQualificationDDIRatioMapping <- function(qualificationContent) {
  ddiMappings <- data.frame()
  for (ddiPlot in qualificationContent$Plots$DDIRatioPlots) {
    for (ddiGroup in ddiPlot$Groups) {
      for (ddiRatios in ddiGroup$DDIRatios) {
        ddiMapping <- data.frame(
          Project = ddiRatios$SimulationControl$Project,
          Simulation_Control = ddiRatios$SimulationControl$Simulation,
          "Control StartTime" = ddiRatios$SimulationControl$StartTime,
          "Control EndTime" = ddiRatios$SimulationControl$EndTime %||% NA,
          "Control TimeUnit" = ddiRatios$SimulationControl$TimeUnit,
          Simulation_Treatment = ddiRatios$SimulationDDI$Simulation,
          "Treatment StartTime" = ddiRatios$SimulationDDI$StartTime,
          "Treatment EndTime" = ddiRatios$SimulationDDI$EndTime %||% NA,
          "Treatment TimeUnit" = ddiRatios$SimulationDDI$TimeUnit,
          Output = ddiRatios$Output,
          "Plot Title" = ddiPlot$Title,
          "Group Title" = ddiGroup$Caption,
          "Observed data" = ddiRatios$ObservedData,
          ObsDataRecordID = ddiRatios$ObservedDataRecordId,
          check.names = FALSE
        )
        ddiMappings <- rbind(ddiMappings, ddiMapping)
      }
    }
  }
  return(ddiMappings)
}


#' @title formatPlotSettings
#' @description
#' Format plot settings into a standardized data.frame for further processing or reporting
#' @param plotSettings Content of a qualification plan
#' @return A data.frame with plot settings information
#' @keywords internal
formatPlotSettings <- function(plotSettings) {
  if (is.null(plotSettings)) {
    return(data.frame(
      ChartWidth = NA,
      ChartHeight = NA,
      AxisSize = NA,
      LegendSize = NA,
      OriginSize = NA,
      FontFamilyName = NA,
      WatermarkSize = NA
    ))
  }
  data.frame(
    ChartWidth = plotSettings$ChartWidth %||% 500,
    ChartHeight = plotSettings$ChartHeight %||% 400,
    AxisSize = plotSettings$Fonts$AxisSize %||% 11,
    LegendSize = plotSettings$Fonts$LegendSize %||% 9,
    OriginSize = plotSettings$Fonts$OriginSize %||% 11,
    FontFamilyName = plotSettings$Fonts$FontFamilyName %||% "Arial",
    WatermarkSize = plotSettings$Fonts$WatermarkSize %||% 40
  )
}

#' @title formatAxesSettings
#' @description
#' Format axes settings for use in qualification plans or reports.
#' @param axesSettings Content of a qualification plan
#' @return A data.frame with axes setting information
#' @keywords internal
formatAxesSettings <- function(axesSettings) {
  if (is.null(axesSettings)) {
    return(data.frame(
      X_Dimension = NA,
      X_GridLines = NA,
      X_Scaling = NA,
      Y_Dimension = NA,
      Y_GridLines = NA,
      Y_Scaling = NA
    ))
  }
  xAxesIndex <- which(sapply(axesSettings, function(axeSettings) {
    axeSettings$Type %in% "X"
  }))
  yAxesIndex <- which(sapply(axesSettings, function(axeSettings) {
    axeSettings$Type %in% "Y"
  }))
  xAxesSettings <- axesSettings[[xAxesIndex]]
  yAxesSettings <- axesSettings[[yAxesIndex]]
  axesSettingsData <- data.frame(
    X_Dimension = xAxesSettings$Dimension,
    X_GridLines = xAxesSettings$GridLines,
    X_Scaling = xAxesSettings$Scaling,
    Y_Dimension = yAxesSettings$Dimension,
    Y_GridLines = yAxesSettings$GridLines,
    Y_Scaling = yAxesSettings$Scaling
  )
  return(axesSettingsData)
}
