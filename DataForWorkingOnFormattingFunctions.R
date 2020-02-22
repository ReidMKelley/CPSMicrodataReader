rm(list = ls())

cat("\014")
library("tictoc")
library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("filesstrings")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")

AOriginalData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataJan1994.rds")
BFormattedData = readRDS("C:/Users/Kelley_R/Desktop/CPSMicrodataReader/UsefulData/CPSExampleDataJan2015Formatted.rds")


cat("\014")

.rs.restartR()