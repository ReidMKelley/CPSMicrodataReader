rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("Z:/Reid/CPSMicrodataReading/")
source("CPSDataReaderFunctions.R")


tic()
StartMonth = 1
StartYear = 2015

EndMonth = 12
EndYear = 2015
Diff = EndYear - StartYear + 1
MonthsNum = matrix(data = 1:12, nrow = 12, ncol = Diff)


if (StartYear <= EndYear) {
  if ((StartYear == EndYear)&&(StartMonth <= EndMonth)) {
    MonthsNum[,1] = c(rep(NA,StartMonth - 1), StartMonth:EndMonth, rep(NA,12 - EndMonth))
  } else if (StartYear < EndYear) {
    MonthsNum[,1] = c(rep(NA,StartMonth - 1), StartMonth:12)
    MonthsNum[,Diff] = c(1:EndMonth, rep(NA,12 - EndMonth))
  } else {
    stop("EndMonth value is less than StartMonth Value in first year. Correct before proceeding.")
  }
  MonthsName = sapply(1:Diff, function(x) month.abb[MonthsNum[,x]])
  Years = StartYear:EndYear
  FileDateName = sapply(1:Diff, function(x) str_c(str_to_lower(MonthsName[,x]), str_trunc(Years[x], width = 2, side = "left", ellipsis = "")))  
  FileDateNum = sapply(1:Diff, function(x) as.numeric(str_c(Years[x], str_trunc(str_c("0", MonthsNum[,x]), width = 2, side = "left", ellipsis = ""))))
} else {
  stop("EndYear value is less than StartYear value. Correct before proceeding.")
}

FileSourcePrefix = "https://thedataweb.rm.census.gov/pub/cps/basic/"
FilePrefixVals = DataFileLocVal(FileDateNum)
FileNameFinal = sapply(1:Diff, function(x) str_c())
FileSourcePaths = sapply(1:Diff, function(x) str_c(FileSourcePrefix, FilePrefixVals[,x], FileDateName[,x], "pub.zip"))
FileDestinationPath = sapply(1:Diff, function(x) str_c("Z:/Reid/CPSMicrodataReading/MicrodataStorage/", FileDateName[,x], "pub.zip"))
FileDownloads = sapply(1:Diff, function(x) sapply(1:12, function(y) download.file(url = FileSourcePaths[y,x], destfile = FileDestinationPath[y,x])))

FileCleanup = list.files(path = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage", full.names = TRUE)
RemovedFiles = file.remove(FileCleanup)
FileExtraction = sapply(1:Diff, function(x) sapply(1:12, function(y) unzip(FileDestinationPath[y,x], exdir = "Z:/Reid/CPSMicrodataReading/MicrodataStorage")))
ExtractedFiles = sapply(1:Diff, function(x) str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/", FileDateName[,x], "pub.dat"))





# DataDictionary = dct.parser(dct = "Z:/Reid/CPSMicrodataReading/cpsbjan2015.dct")
# DataDictionary$ColName = as.character(DataDictionary$ColName)
# DataDictionary$VarLabel = as.character(DataDictionary$VarLabel)
# DataDictionary$colClasses = str_replace_all(DataDictionary$colClasses,"raw", "integer")
# DataDictionary$EndPos = DataDictionary$StartPos + DataDictionary$ColWidth - 1
# 
# 
# 
# 
# 
# # Test0 = CPSMicrodataReader(FileIn = "Z:/Reid/CPSMicrodataReading/jan15pub.dat", DataDictionaryIn = DataDictionary)
# # FileInName = FileSourcePaths[1,1]
# # Test1 = CPSMicrodataReader(FileInName, DataDictionary)
# # 
# DataOut = list()
# 
# for (j in 1:12) {
#   FileInVal = ExtractedFiles[j, 1]
#   DataOut[[j]] = CPSMicrodataReader(FileIn = FileInVal, DataDictionaryIn = DataDictionary)
# }
# names(DataOut) = FileDateName
# 
# 
# 
# .rs.restartR()

ArchiveDirectoryName = str_replace(str_replace(str_trunc(as.character(Sys.time()), width = 16, side = "right", ellipsis = ""), pattern = " ", replacement = "_"), pattern = ":", replacement = "")
D0 = dir.create(path = str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/Archive/", ArchiveDirectoryName))
D1 = dir.create(path = str_c("Z:/Reid/CPSMicrodataReading/MicrodataStorage/Archive/", ArchiveDirectoryName))
toc()

