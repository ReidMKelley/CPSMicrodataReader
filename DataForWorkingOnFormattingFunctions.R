rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("ParsingFunctionBuilding.R")
Dictionary = dct.parser(dct = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/cpsbjan98.dct")
AA = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataJan1998.rds")
FormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/0CPSExampleDataJan2015Formatted.rds")

BB = ParserJanuary1998(AA, DictionaryIn = Dictionary)

# cat("\014")

.rs.restartR()

