
# OSPSuite.QualificationPlanEditor

<!-- badges: start -->

  [![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/latest/total?label=%E2%AD%B3%20Downloads%20latest%20release)](https://github.com/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/releases/latest)
  [![](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/total?label=%E2%AD%B3%20Downloads%20total)](https://github.com/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/releases)

  [![build](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/main-workflow.yaml?logo=github&logoColor=white&label=Build)](https://github.com/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/actions/workflows/main-workflow.yaml)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor)
  [![Lint Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/lint.yaml?logo=githubactions&logoColor=white&label=lint)](https://github.com/Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor/actions/workflows/lint.yaml)

<!-- badges: end -->

Convert your qualification plan to Excel for easy editing, then convert back as a qualification plan with `{ospsuite.qualificationplaneditor}`.

## Installation

You can install the development version of `{ospsuite.qualificationplaneditor}` like so:

``` r
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.QualificationPlanEditor")
```

## Example

Here is a basic example showing how to include an updated project snapshot into your qualification plan:

``` r
## Load the ospsuite.qualificationplaneditor package
library(ospsuite.qualificationplaneditor)
ospPath <- "https://raw.githubusercontent.com/Open-Systems-Pharmacology"
excelQualification <- "Updated-Qualification.xlsx"

# List your updated snapshot projects
snapshotPaths <- list(
  "Compound A" = file.path(ospPath, "A-Model/vX.X/A-Model.json"),
  "Compound B" = file.path(ospPath, "B-Model/vX.X/B-Model.json"),
  "A-B-DDI" = file.path(ospPath, "A-B-DDI/vX.X/A-B-DDI.json")
)

# List your updated observed datasets
observedDataPaths <- list("DDI Ratios" = list(
    Path = file.path(ospPath, "Database-for-observed-data/vX.X/DDI.csv"),
    Type = "DDIRatio"
    ))

# Initial qualification plan
qualificationPlan <- file.path(ospPath, "A-Model/vY.Y/Qualification/qualification_plan.json")

# qualification plan converted to Excel
excelUI(
  fileName = excelQualification,
  snapshotPaths = snapshotPaths, 
  observedDataPaths = observedDataPaths,
  qualificationPlan = qualificationPlan
)

# Tip: this will open Excel (or Libre office) with the created Excel workbook
utils::browseURL(excelQualification)

```

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution &#128161;

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

`{ospsuite.qualificationplaneditor}` package is released under the [GPLv3 License](LICENSE).

All trademarks within this document belong to their legitimate owners.


