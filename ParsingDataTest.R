rm(list = ls())
options(java.parameters = "-Xmx1024m")
cat("\014")
library("tictoc")
library("xlsx")
library("tidyverse")
setwd("C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPSMicrodataReader")

DictionaryExcelWkbk = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPSMicrodataReader/DataDictionaryFilesFinalCopy.xlsx"
ColTypes = c("numeric", "character", "numeric", "character", "numeric", "character", "numeric", "character", "numeric", "character", "numeric", "numeric", "numeric", "character")
DictionaryIn = read.xlsx(file = DictionaryExcelWkbk, sheetIndex = 1, colIndex = 1:14, colClasses = ColTypes)
DictionaryIn = filter(DictionaryIn, !is.na(ColName))

DataIn = readRDS("C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataSep1995.rds")

A0 = filter(filter(DictionaryIn, Adjustment != "Remove"), Adjustment != "Delete")$ColName


AA = select(DataIn, -all_of(filter(DictionaryIn, Adjustment == "Remove")$ColName))

AA$hrsample[AA$hrsample == "-1"] = NA
AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
AA$hrsersuf[is.na(AA$hrsersuf)] = -1
AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")

AA = tibble(AA, hryear4 = 1900 + AA$hryear, hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum), gecmsanum = AA$gecmsa, gemsanum = AA$gemsa, geconum = AA$geco)

AA = select(AA, -all_of(filter(DictionaryIn, Adjustment == "Delete")$ColName))
AA = select(AA, A0)