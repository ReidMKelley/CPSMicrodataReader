rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

source("ParsingFunctionBuilding.R")
Dictionary = dct.parser(dct = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/cpsbjan03.dct")
AA = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataJan2003.rds")
FormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/0CPSExampleDataJan2015Formatted.rds")

A1 = ParserJanuary2003(AA, DictionaryIn = Dictionary)
# B1 = ParserJanuary1998(BB, DictionaryIn = Dictionary)
cat("\014")


