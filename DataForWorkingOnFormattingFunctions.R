rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

AA = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataSep1995.rds")
FormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/0CPSExampleDataJan2015Formatted.rds")


cat("\014")

.rs.restartR()

