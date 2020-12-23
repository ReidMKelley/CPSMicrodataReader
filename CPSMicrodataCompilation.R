rm(list = ls())
options(java.parameters = "-Xmx1024m")
cat("\014")
library("tictoc")
library("xlsx")
library("tidyverse")
setwd("C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPSMicrodataReader")
source("CPSDataReaderFunctions.R")
source("ParsingFunctionBuilding.R")



# These four variables need to be filled in with appropriate values for the starting and ending months before running the script.
# The earliest available month is September 1995. The latest available month is currently August 2020. (As of September 14, 2020.)
StartMonth = 1
StartYear = 2017
EndMonth = 1
EndYear = 2017

# The archive location is a directory file that contains the previously downloaded copies of the Microdata. This needs to be filled in before running the script.
# These files are downloaded as zipped files from the Census website. Having a fixed archive allows for easy additions as months pass without requiring extra downloads.
ArchiveLocation = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPS Microdata Storage/"

UnzippedFileStorage = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPS Microdata Storage/UnzippedFiles"
                      

# This gives the full FilePath of the the ExcelWorkbook file that contains the Data Dictionary files as Excel Sheets.
# This needs to be set by the user for the program to work properly.
DictionaryExcelWkbk = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPSMicrodataReader/DataDictionaries/DataDictionaryFilesFinalCopy.xlsx"




# This reads in the text file that contains all the downloaded .DAT files with the actual microdata. It's location may need to be adjusted by the user.
DataSourceFileStrings = read_tsv("DownloadedFilePaths.txt", col_names = c("FileString"))





# After this point, to the end of the script, the user should not need to make any additional data code changes.

# This section confirms that the Ending dates are valid given the starting dates. If it fails, an error is returned to show ther user that there is a problem.
# The first If statement ensures that the EndYear value is at least as large as the StartYear Value.
if (StartYear <= EndYear) {
  # If the EndYear value is at least as large as the StartYear, we then have to compare the EndMonth to the StartMonth. The only potential issue is 
  # EndYear are the same and the StartMonth is larger than the EndMonth. If true we have a problem,
  # and so this then stops the script flow and sends an error to the screen.
  if ((StartYear == EndYear)&&(StartMonth > EndMonth)) {
    stop("\n\nEndMonth value is less than StartMonth Value in first year. Correct before proceeding.\n\n")
  }
  # The other potential problem is if the EndYear value is less than the StartYear value. The else in the original If-else statement handles this case.
  # Similiar to the above it stops the script flow and sends an error to the screen.
} else {
  stop("\n\nEndYear value is less than StartYear value. Correct before proceeding.\n\n")
}

# These variables set up the numbering system to easily order all the files from September 1995 onward. The number strings start with 001 for September 1995 and increase
# by 1 for each month. The IDMax gives the largest value for the selected set. The largest possible IDMax value is 300; this will change as months pass.

# The points variables are for putting together the sequence of numbers. The initial point is set at September 1995. The StartPoint and EndPoint values are scaled off this.
InitialPoint = 12*1995 + 8
StartPoint = 12*StartYear + StartMonth - InitialPoint
EndPoint = 12*EndYear + EndMonth - InitialPoint
IDMax = max(100, EndPoint)
# This gives a vector of all the points selected. It allows for choosing a start point later than September 1995. 
# It is inserted into the various name part vectors to select only the ones that match the chosen start and end dates.
IDPoints = StartPoint:EndPoint

# IDNums is the vector of strings for each ID Number - it forces them all to be 3 digit strings in order to make sorting the actual files in Windows Explorer easier.
IDNums = c(str_c("00", 1:9), str_c("0", 10:99), as.character(100:IDMax))
IDNums = IDNums[IDPoints]

# These variables are used to set up the urls and file paths that contain the data files to download. 

# MonthsNum is used to help write up the month-year name combinations for finding the appropriate files. 
MonthsNum = c(9:12, rep(1:12, (EndYear - 1995)))
MonthsNum = MonthsNum[IDPoints]

MonthsName = str_to_lower(month.abb[MonthsNum])

# The Year variable is a vector that gives the corresponding year number for each month since 1995. 
# The if statement existis to ensure that the EndYear = 1995 case is properly handled, as the repeat function will duplicate the 1995s in the vector [1996, 1995].
if(EndYear>1995) {
  Years = c(rep(1995,4), sapply(1996:EndYear, function(x) rep(x, 12)))  
} else {
  Years = rep(1995,4)
}
Years = Years[IDPoints]

# FileDateName sets up the month-year name combinations for each file.
FileDateName = str_c(IDNums, MonthsName, str_trunc(Years, width = 2, side = "left", ellipsis = ""))


FilesNeeded = DataSourceFileStrings$FileString[which(sapply(seq_along(DataSourceFileStrings$FileString), function(y) any(sapply(seq_along(FileDateName), function(x) str_detect(DataSourceFileStrings$FileString, FileDateName[x]))[y,])), TRUE)]
DictionaryToFileConnection = DictionaryConnector(EndMonthVal = EndMonth, EndYearVal = EndYear)
DictionaryToFileConnection = DictionaryToFileConnection[IDPoints,]

DictionariesNeeded = tibble(Nums = unique(DictionaryToFileConnection$DictionaryNum), Names = unique(DictionaryToFileConnection$DictionaryNames))
DictionaryFiles = vector(mode = "list", length = length(DictionariesNeeded$Nums))
names(DictionaryFiles) = DictionariesNeeded$Names

ColTypes = c("numeric", "character", "numeric", "character", "numeric", "character", "numeric", "character", "numeric", "character", "numeric", "numeric", "numeric", "character")
for (j in 1:length(DictionariesNeeded$Nums)) {
  DictionaryFiles[[j]] = read.xlsx(file = DictionaryExcelWkbk, sheetName = DictionariesNeeded$Names[j], colIndex = 1:14, colClasses = ColTypes)
  DictionaryFiles[[j]] = filter(DictionaryFiles[[j]], !is.na(ColName))
}


rm(j)

tic()
FileExtraction = sapply(seq_along(FilesNeeded), function(x) FileUnziper(FileToUnzip = FilesNeeded[x], ExtractedFileDest = UnzippedFileStorage))
ExtractedFiles = list.files(path = UnzippedFileStorage, full.names = TRUE)
ExtractedFileSuffixes = str_trunc(ExtractedFiles, 4, side = "left", ellipsis = "")
ExtractedFileRename = str_c(UnzippedFileStorage, "/", FileDateName, ExtractedFileSuffixes)
FileRenaming = file.rename(from = ExtractedFiles, to = ExtractedFileRename)

# # This section is specifically set up for my parsing function building. It should be removed after I am finished.
#  Testing the March data file.
Test0 = CPSMicrodataReader(FileIn = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPS Microdata Storage/UnzippedFiles/257jan17.dat", DataDictionaryIn = DictionaryFiles$Jan17Dictionary)
Test1 = ParserJanuary2017(DataIn = Test0, DataDictionaryIn = DictionaryFiles$Jan17Dictionary)
saveRDS(Test0, file = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPS Microdata Storage/UsefulData/CPSExampleDataUnformattedJan2017.rds")
saveRDS(Test1, file = "C:/Users/Kelley_R/OneDrive - US Department of Labor - BLS/Desktop/CPS Microdata Storage/UsefulData/CPSExampleDataFormattedJan2017.rds")




# DictionaryMonthConnection = tibble(OrderNumber = as.vector(IDVal), FileDateName = as.vector(FileDateName), FileDateNum = as.vector(FileDateNum), FileLocation = as.vector(ExtractedFiles))
# # DictionaryMonthConnection = tibble(OrderNumber = as.vector(IDVal), FileDateName = as.vector(FileDateName), FileDateNum = as.vector(FileDateNum), FileLocation = as.vector(ExtractedFiles), DataDictionary = as.vector(DictionaryDestPath))

# 
# 
# 
# 
# Test0 = CPSMicrodataReader(FileIn = "C:/Users/Kelley_R/Documents/CPSMicrodataStorage/001sep95pub.cps", DataDictionaryIn = DataDictionary)
# 
# 
# 
# # FileInName = FileSourcePaths[1,1]
# # Test1 = CPSMicrodataReader(FileInName, DataDictionary)
# #
# # DataOut = list()
# # 
# # for (j in 1:12) {
# #   FileInVal = ExtractedFiles[j, 1]
# #   DataOut[[j]] = CPSMicrodataReader(FileIn = FileInVal, DataDictionaryIn = DataDictionary)# }
# # names(DataOut) = FileDateName
# 
# 
# ArchiveDirectoryName = str_replace(str_replace(str_trunc(as.character(Sys.time()), width = 16, side = "right", ellipsis = ""), pattern = " ", replacement = "_"), pattern = ":", replacement = "")
# ArchiveExtractedDirectory = str_c("C:/Users/Kelley_R/Documents/CPSMicrodataStorage/Archive/", ArchiveDirectoryName)
# D0 = dir.create(path = ArchiveExtractedDirectory)
# ArchiveZipDirectory = str_c("Z:/Reid/CPSMicrodataReading/MicrodataStorage/Archive/", ArchiveDirectoryName)
# D1 = dir.create(path = ArchiveZipDirectory)
# D01 = sapply(1:Diff, function(x) sapply(1:12, function(y) FileMover(FileToMove =  ExtractedFiles[y, x], DestinationOfFile = ArchiveExtractedDirectory)))
# D11 = sapply(1:Diff, function(x) sapply(1:12, function(y) FileMover(FileToMove =  FileDestinationPath[y, x], DestinationOfFile = ArchiveZipDirectory)))
# D12 = sapply(seq_along(DictionaryUniquePath), function(x) FileMover(FileToMove =  DictionaryUniquePath[x], DestinationOfFile = ArchiveZipDirectory))
# 
# FileCleanup = file.remove(ExtractedFileRename)
cat("\014")
toc()

.rs.restartR()
