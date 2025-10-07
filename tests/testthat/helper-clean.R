# Files starting by "helper-", are automatically run before each test
# this helper will be used to clean files generated between each run

excelFilesToClean <- list.files(pattern = ".xlsx")
unlink(excelFilesToClean)

ospURL <- "https://raw.githubusercontent.com/Open-Systems-Pharmacology"
