rm(list = ls())

cat("\014")

library("StataDCTutils")
library("readstata13")
library("tidyverse")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")
source("CPSDataReaderFunctions.R")

PrefixName = c("cpsbjan94", "cpsbapr94", "cpsbjun95", "cpsbsep95", "cpsbjan98", "cpsbjan03", "cpsbmay04", "cpsbaug05", "cpsbjan07", "cpsbjan09", "cpsbjan10", "cpsbmay12", "cpsbjan13", "cpsbapr2014", "cpsbjan2015")
DataDictionaries = list()

DataDictionaryPrefix = str_c("https://data.nber.org/data/progs/cps-basic/", PrefixName, ".dct")


DataDictionaryIn = dct.parser(dct = DataDictionaryPrefix[1])
DataDictionaryIn$ColName = as.character(DataDictionaryIn$ColName)
DataDictionaryIn$colClasses = as.character(DataDictionaryIn$colClasses)  
DataDictionaries[[1]] = DataDictionaryIn
UnchangingVariables = DataDictionaryIn$ColName
Testing = "A"


for (j in 2:15) {
  DataDictionaryIn = dct.parser(dct = DataDictionaryPrefix[j])
  DataDictionaryIn$ColName = as.character(DataDictionaryIn$ColName)
  DataDictionaryIn$colClasses = as.character(DataDictionaryIn$colClasses)  
  DataDictionaries[[j]] = DataDictionaryIn
  UnchangingVariables = intersect(DataDictionaryIn$ColName, UnchangingVariables)
}

ChangingVariables = lapply(1:15, function(x) setdiff(DataDictionaries[[x]]$ColName, UnchangingVariables))

names(DataDictionaries) = PrefixName
names(ChangingVariables) = PrefixName
cat("\014")
