# Cleaning the Work Environment
rm(list = ls())

cat("\014")
library("tictoc")
library("xlsx")
library("tidyverse")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("CPSDataReaderFunctions.R")



tic()
StartMonth = 9
StartYear = 1995

EndMonth = 7
EndYear = 2020
Diff = EndYear - StartYear + 1
MonthsNum = matrix(data = 1:12, nrow = 12, ncol = Diff)

ArchiveLocation = "C:/Users/Kelley_R/Desktop/CPS Microdata Storage/"


# This section confirms that the Ending dates are valid given the starting dates. If it fails, an error is returned to show ther user that there is a problem.
if (StartYear <= EndYear) {
  if ((StartYear == EndYear)&&(StartMonth <= EndMonth)) {
    MonthsNum[,1] = c(rep(NA,StartMonth - 1), StartMonth:EndMonth, rep(NA,12 - EndMonth))
  } else if (StartYear < EndYear) {
    MonthsNum[,1] = c(rep(NA,StartMonth - 1), StartMonth:12)
    MonthsNum[,Diff] = c(1:EndMonth, rep(NA,12 - EndMonth))
  } else {
    toc()
    stop("\n\nEndMonth value is less than StartMonth Value in first year. Correct before proceeding.\n\n")
  }
  MonthsName = sapply(1:Diff, function(x) month.abb[MonthsNum[,x]])
  Years = StartYear:EndYear
  FileDateName = sapply(1:Diff, function(x) str_c(str_to_lower(MonthsName[,x]), str_trunc(Years[x], width = 2, side = "left", ellipsis = "")))  
  FileDateNum = sapply(1:Diff, function(x) as.numeric(str_c(Years[x], str_trunc(str_c("0", MonthsNum[,x]), width = 2, side = "left", ellipsis = ""))))
  IDMax = Diff*12 - (StartMonth - 1) - (12 - EndMonth)
  if(IDMax>99) {
    IDVal = matrix(data = c(rep(NA,StartMonth - 1), str_c("00", 1:9), str_c("0", 10:99), as.character(100:IDMax), rep(NA,12 - EndMonth)), ncol = Diff)
  } else if ((IDMax<99)&&(IDMax>9)) {
    IDVal = matrix(data = c(rep(NA,StartMonth - 1), str_c("0", 1:9), as.character(10:IDMax), rep(NA,12 - EndMonth)), ncol = Diff)
  } else if (IDMax<9) {
    IDVal = matrix(data = c(rep(NA,StartMonth - 1), as.character(1:IDMax), rep(NA,12 - EndMonth)), ncol = Diff)
  }
} else {
  toc()
  stop("\n\nEndYear value is less than StartYear value. Correct before proceeding.\n\n")
}


# This is the code to get the Microdata files from the Census Bureau archive. They will be sent to the archive location specified above.  
FileSourcePrefix = str_c("https://www2.census.gov/programs-surveys/cps/datasets/", Years, "/basic/")
FileSourcePaths = as.vector(sapply(1:Diff, function(x)  str_c(FileSourcePrefix[x], FileDateName[,x], "pub.zip")))
FileDestinationPath = as.vector(sapply(1:Diff, function(x) str_c(ArchiveLocation, IDVal[,x], FileDateName[,x], "pub.zip")))
FileCheck = list.files(ArchiveLocation, full.names = TRUE)
FilesToDownload = setdiff(FileDestinationPath, FileCheck)
FilesToDownload = FilesToDownload[!is.na(FilesToDownload)]
FileSourcing = FileSourcePaths[sapply(seq_along(FilesToDownload), function (x) which(as.vector(FileDestinationPath) == FilesToDownload[x]))]
# Test
FileSourcing[1] = "https://www2.census.gov/programs-surveys/cps/datasets/2020/basic/1jun20pub.zip"

if (length(FilesToDownload) == 0) {
  write(FileDestinationPath[!is.na(FileDestinationPath)], file = "DownloadedFilePaths.txt")
  toc()
  cat("\n\nAll requested files already downloaded. You may continue.\n\n")
  
} else {
  
  DownloadActual = sapply(seq_along(FilesToDownload), function (x) FileDownloaderFunction(FileURL = FileSourcing[x], FileDest = FilesToDownload[x]))
  if(all(sapply(seq_along(DownloadActual), function (x) identical(DownloadActual[x],"0")))) {
    toc()
    cat("\n\nThe program has downloaded all files. You may continue.\n\n")  
  } else {
    FailedDownloads = FileSourcing[which(DownloadActual != "0")]
    DownloadErrors = DownloadActual[which(DownloadActual != "0")]
    toc()
    cat("\n\nThe program has completed downloading. There were some errors.\n\nPlease see the \"FailedDownloads\" variable for the error files and the \"DownloadErrors\" variable for details.\n\n")
  }
  
} 
rm(MonthsName, MonthsNum, Diff, DownloadActual, FileCheck, FileSourcePaths, FileSourcePrefix, IDMax)
rm(list=lsf.str())


.rs.restartR()
