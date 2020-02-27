rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("CPSDataReaderFunctions.R")

DataDictionary = dct.parser(dct = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/cpsbsep95.dct")
DataDictionary$ColName = as.character(DataDictionary$ColName)
DataDictionary$VarLabel = as.character(DataDictionary$VarLabel)
DataDictionary$colClasses = str_replace_all(DataDictionary$colClasses,"raw", "integer")
DataDictionary$EndPos = DataDictionary$StartPos + DataDictionary$ColWidth - 1

Test0 = CPSMicrodataReader(FileIn = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/sep95pub.cps", DataDictionaryIn = DataDictionary)


DataOut = saveRDS(Test0, file = "C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataSep1995.rds")