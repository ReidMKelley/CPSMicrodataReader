rm(list = ls())

cat("\014")
library("tictoc")
library("xlsx")
library("tidyverse")
library("filesstrings")
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
    stop("EndMonth value is less than StartMonth Value in first year. Correct before proceeding.\n\n")
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
  stop("EndYear value is less than StartYear value. Correct before proceeding.\n\n")
}


# This is the code to get the Microdata files from the Census Bureau archive. They will be sent to the archive location specified above.  
FileSourcePrefix = str_c("https://www2.census.gov/programs-surveys/cps/datasets/", Years, "/basic/")
FileSourcePaths = as.vector(sapply(1:Diff, function(x)  str_c(FileSourcePrefix[x], FileDateName[,x], "pub.zip")))
FileDestinationPath = as.vector(sapply(1:Diff, function(x) str_c(ArchiveLocation, IDVal[,x], FileDateName[,x], "pub.zip")))
FileCheck = list.files(ArchiveLocation, full.names = TRUE)
FilesToDownload = setdiff(FileDestinationPath, FileCheck)
FilesToDownload = FilesToDownload[!is.na(FilesToDownload)]
FileSourcing = FileSourcePaths[sapply(seq_along(FilesToDownload), function (x) which(as.vector(FileDestinationPath) == FilesToDownload[x]))]
DownloadActual = sapply(seq_along(FilesToDownload), function (x) FileDownloaderFunction(FileURL = FileSourcing[x], FileDest = FilesToDownload[x]))

# DataDictionaries = loadWorkbook("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/DataDictionaryFilesFinalCopy.xlsx")
# X = getSheets(DataDictionaries)


# FileCleanup = setdiff(list.files(path = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage", full.names = TRUE), dir(path = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage", full.names = TRUE))
# RemovedFiles = file.remove(FileCleanup)




# FileExtraction = sapply(1:Diff, function(x) sapply(1:12, function(y) FileUnziper(FileToUnzip = FileDestinationPath[y,x], ExtractedFileDest = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage")))
# ExtractedFiles = sapply(1:Diff, function(x) str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/", IDVal[,x], FileDateName[,x], "pub.dat"))
# FileRenaming = sapply(1:Diff, function(x) file.rename(from = str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/", FileDateName[,x], "pub.dat"), to = ExtractedFiles[,x]))
# 
# DictionaryMonthConnection = tibble(OrderNumber = as.vector(IDVal), FileDateName = as.vector(FileDateName), FileDateNum = as.vector(FileDateNum), FileLocation = as.vector(ExtractedFiles))
# # DictionaryMonthConnection = tibble(OrderNumber = as.vector(IDVal), FileDateName = as.vector(FileDateName), FileDateNum = as.vector(FileDateNum), FileLocation = as.vector(ExtractedFiles), DataDictionary = as.vector(DictionaryDestPath))

# 
# 
# 
# 
# Test0 = CPSMicrodataReader(FileIn = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/sep95pub.cps", DataDictionaryIn = DataDictionary)
# 
# 
# 
# # FileInName = FileSourcePaths[1,1]
# # Test1 = CPSMicrodataReader(FileInName, DataDictionary)
# #
# # DataOut = list()
# # 
# # for (j in 1:12) {
# #   FileInVal = ExtractedFiles[j, 1]
# #   DataOut[[j]] = CPSMicrodataReader(FileIn = FileInVal, DataDictionaryIn = DataDictionary)# }
# # names(DataOut) = FileDateName
# 
# 
# ArchiveDirectoryName = str_replace(str_replace(str_trunc(as.character(Sys.time()), width = 16, side = "right", ellipsis = ""), pattern = " ", replacement = "_"), pattern = ":", replacement = "")
# ArchiveExtractedDirectory = str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/Archive/", ArchiveDirectoryName)
# D0 = dir.create(path = ArchiveExtractedDirectory)
# ArchiveZipDirectory = str_c("Z:/Reid/CPSMicrodataReading/MicrodataStorage/Archive/", ArchiveDirectoryName)
# D1 = dir.create(path = ArchiveZipDirectory)
# D01 = sapply(1:Diff, function(x) sapply(1:12, function(y) FileMover(FileToMove =  ExtractedFiles[y, x], DestinationOfFile = ArchiveExtractedDirectory)))
# D11 = sapply(1:Diff, function(x) sapply(1:12, function(y) FileMover(FileToMove =  FileDestinationPath[y, x], DestinationOfFile = ArchiveZipDirectory)))
# D12 = sapply(seq_along(DictionaryUniquePath), function(x) FileMover(FileToMove =  DictionaryUniquePath[x], DestinationOfFile = ArchiveZipDirectory))
# 

cat("\014")
toc()

# .rs.restartR()
