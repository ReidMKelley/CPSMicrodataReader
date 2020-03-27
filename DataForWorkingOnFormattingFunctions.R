rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("ParsingFunctionBuilding.R")
Dictionary = dct.parser(dct = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/cpsbmay04.dct")
AA = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataMay2004.rds")
FormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/0CPSExampleDataJan2015Formatted.rds")

# A1 = ParserMay2004(AA)
# B1 = ParserJanuary1998(BB, DictionaryIn = Dictionary)
cat("\014")


