rm(list = ls())

cat("\014")

library("StataDCTutils")
library("readstata13")
library("tidyverse")
library("xlsx")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")
source("CPSDataReaderFunctions.R")

PrefixName = c("cpsbjan94", "cpsbapr94", "cpsbjun95", "cpsbsep95", "cpsbjan98", "cpsbjan03", "cpsbmay04", "cpsbaug05", "cpsbjan07", "cpsbjan09", "cpsbjan10", "cpsbmay12", "cpsbjan13","cpsbjan2014", "cpsbapr2014", "cpsbjan2015")
RecordLoc = c("199401-199403/jan94_mar94_dd.txt", "199404-199505/apr94_may95_dd.txt", "199506-199508/jun95_aug95_dd.txt", "199509-199712/sep95_dec97_dd.txt", "199801-/jan98dd.asc", "200301-/jan03dd.txt", "200405-/may04dd.txt", "200508-/augnov05dd.txt", "200701-/jan07dd.txt", "200901-/jan09dd.txt", "201001-/jan10dd.txt", "201205-/may12dd.txt", "201301-/January_2013_Record_Layout.txt", "201401-/January_2014_Record_Layout.txt", "201501-/January_2015_Record_Layout.txt", "201701-/January_2017_Record_Layout.txt")
RecordName = c("01Jan94DD.txt", "02Apr94DD.txt", "03Jun95DD.txt", "04Sep95DD.txt", "05Jan98DD.asc", "06Jan03DD.txt", "07May04DD.txt", "08Aug05DD.txt", "09Jan07DD.txt", "10Jan09DD.txt", "11Jan10DD.txt", "12May12DD.txt", "13Jan13DD.txt", "14Jan14DD.txt", "15Jan15DD.txt", "16Jan17DD.txt")
DataDictionaries = list()

DataDictionaryPrefix = str_c("https://data.nber.org/data/progs/cps-basic/", PrefixName, ".dct")
RecordLayoutLocation = str_c("http://thedataweb.rm.census.gov/pub/cps/basic/", RecordLoc)
RecordLayout = str_c("C:/Users/Kelley_R/Desktop/CPS Microdata Record Layouts/", RecordName)

RecordDownload = sapply(1:16, function(x) download.file(url = RecordLayoutLocation[x], destfile = RecordLayout[x]))
ExampleData = read.dta13(file = "Z:/Reid/CPSMicrodataReading/ExampleDatacpsb2015_1.dta")

DataDictionaryIn = dct.parser(dct = DataDictionaryPrefix[1])
DataDictionaryIn$ColName = as.character(DataDictionaryIn$ColName)
# DataDictionaryIn$colClasses = as.character(DataDictionaryIn$colClasses)
DataDictionaryIn$VarLabel = as.character(DataDictionaryIn$VarLabel)
DataDictionaryIn$colClasses = str_replace_all(DataDictionaryIn$colClasses,"raw", "integer")
DataDictionaryIn$EndPos = DataDictionaryIn$StartPos + DataDictionaryIn$ColWidth - 1

DataDictionaries[[1]] = DataDictionaryIn
UnchangingVariables = DataDictionaryIn$ColName
WB = createWorkbook(type = "xlsx")
AA = createSheet(WB, sheetName = PrefixName[1])
BB = addDataFrame(DataDictionaryIn, AA, showNA = TRUE, characterNA = "NA")


for (j in 2:16) {
  DataDictionaryIn = dct.parser(dct = DataDictionaryPrefix[j])
  DataDictionaryIn$ColName = as.character(DataDictionaryIn$ColName)
  DataDictionaryIn$colClasses = as.character(DataDictionaryIn$colClasses)
  DataDictionaryIn$VarLabel = as.character(DataDictionaryIn$VarLabel)
  DataDictionaryIn$colClasses = str_replace_all(DataDictionaryIn$colClasses,"raw", "integer")
  DataDictionaryIn$EndPos = DataDictionaryIn$StartPos + DataDictionaryIn$ColWidth - 1
  AA = createSheet(WB, sheetName = PrefixName[j])
  BB = addDataFrame(DataDictionaryIn, AA, showNA = TRUE, characterNA = "NA")
  
  
  DataDictionaries[[j]] = DataDictionaryIn
  UnchangingVariables = intersect(DataDictionaryIn$ColName, UnchangingVariables)
}

CC = saveWorkbook(WB, "C:/Users/Kelley_R/Desktop/CPSMicrodataReader/DataDictionaryFilesfromStata2.xlsx")
# ChangingVariables = lapply(1:16, function(x) setdiff(DataDictionaries[[x]]$ColName, UnchangingVariables))
# ImportantVariables = read_csv(file = "C:/Users/Kelley_R/Desktop/CPS Microdata Record Layouts/Book1.csv", col_names = FALSE)
# ImportantVariables = str_to_lower(ImportantVariables$X1)
# 
# 
# names(DataDictionaries) = PrefixName
# names(ChangingVariables) = PrefixName
# Test = ls()
# VariableCheck = tibble(PrefixName)
# T1 = setdiff(Test, c("DataDictionaries", "ImportantVariables", "UnchangingVariables", "ChangingVariables", "VariableCheck"))
# rm(list = T1)
# rm(Test, T1)
# cat("\014")
# 
# 
# # 
# # for (j in 1:16) {
# #   if (any(str_detect(DataDictionaries[[j]]$ColName, "prunedur"))) {
# #     VariableCheck[j, 2] = TRUE
# #   } else {
# #     VariableCheck[j, 2] = FALSE
# #   }
# # }
# # rm(j)
# 
# OtherVariables = c("hehousut", "hrintsta", "hrhtype", "hrlonglk", "gereg", "gestfips", "gediv", "gtcbsast", "gtmetsta", "gtcbsasz", "prtfage", "pthr", "ptot", "prchld")
# for (k in 1:length(OtherVariables)) {
#   for (j in 1:16) {
#     if (any(str_detect(DataDictionaries[[j]]$ColName, OtherVariables[k]))) {
#       VariableCheck[j, k+1] = TRUE
#     } else {
#       VariableCheck[j, k+1] = FALSE
#     }
#   }
# }
# colnames(VariableCheck) = c("DictionaryName", OtherVariables)
# rm(j, k)

