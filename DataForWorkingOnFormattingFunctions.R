rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("CPSMicrodataParsingFunctions.R")
Dictionary = dct.parser(dct = "cpsbsep95.dct")
AA = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataSep1995.rds")
FormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/0CPSExampleDataJan2015Formatted.rds")

BB = ParserSeptember1995(AA, DictionaryIn = Dictionary)

# cat("\014")

# .rs.restartR()

