# Cleaning the Work Environment
rm(list = ls())
cat("\014")

# Initial package and function loading. Sets working directory.
library("tictoc")
library("xlsx")
library("tidyverse")
setwd("C:/Users/Kelley_R/Desktop/CPSMicrodataReader")
source("CPSDataReaderFunctions.R")



# These four variables need to be filled in with appropriate values for the starting and ending months before running the script.
# The earliest available month is September 1995. The latest available month is currently August 2020. (As of September 14, 2020.)
StartMonth = 9
StartYear = 1995
EndMonth = 7
EndYear = 2020

# The archive location is a directory file that contains the previously downloaded copies of the Microdata. This needs to be filled in before running the script.
# These files are downloaded as zipped files from the Census website. Having a fixed archive allows for easy additions as months pass without requiring extra downloads.

ArchiveLocation = "C:/Users/Kelley_R/Desktop/CPS Microdata Storage/"



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

# These vairables set up the numbering system to easily order all the files from September 1995 onward. The number strings start with 001 for September 1995 and increase
# by 1 for each month. The IDMax gives the largest value for the selected set. Currently the largest possible IDMax value is 299; this will change as months pass.

# The points variables are for putting together the sequence of numbers. The initial point is set at September 1995. The StartPoint and EndPoint values are scaled off this.
InitialPoint = 12*1995 + 9
StartPoint = 12*StartYear + StartMonth - InitialPoint + 1
EndPoint = 12*EndYear + EndMonth - InitialPoint + 1
IDMax = max(100, EndPoint)
# This gives a vector of all the points selected. It allows for choosing a start point later than September 1995. 
IDPoints = StartPoint:EndPoint

# IDNums is the vector of strings for each ID Number - it forces them all to be 3 digit strings in order to make sorting the actual files in Windows Explorer easier.
IDNums = c(str_c("00", 1:9), str_c("0", 10:99), as.character(100:IDMax))
IDNums = IDNums[IDPoints]

# These variables are used to set up the urls and file paths that contain the data files to download. 

# MonthsNum is used to help write up the month-year name combinations for finding the appropriate files. 
MonthsNum = c(9:12, rep(1:12, (EndYear - StartYear + 1)))
MonthsNum = MonthsNum[IDPoints]

MonthsName = str_to_lower(month.abb[MonthsNum])

Years = c(rep(1995,4), sapply(1996:EndYear, function(x) rep(x, 12)))
Years = Years[IDPoints]

# FileDateName sets up the month-year name combinations for each file.
FileDateName = str_c(MonthsName, str_trunc(Years, width = 2, side = "left", ellipsis = ""))





# This is the code to get the Microdata files from the Census Bureau archive. They will be sent to the archive location specified above.  

# These two variables set up the strings that contain the urls for downloading.The prefix sets up the years. 
FileSourcePaths = str_c("https://www2.census.gov/programs-surveys/cps/datasets/", Years, "/basic/", FileDateName, "pub.zip")
FileDestinationPath = str_c(ArchiveLocation, IDNums, FileDateName, "pub.zip")

# We only want to download files that are not currently in our individual archive. This saves time as the download from Census seems to take an unnecessarily long time.
# This then checks to see which files we actually need to download by checking the archive and removing files that we already have from our vector of files to download.
FileCheck = list.files(ArchiveLocation, full.names = TRUE)
FilesToDownload = setdiff(FileDestinationPath, FileCheck)
FilesToDownload = FilesToDownload[!is.na(FilesToDownload)]

# This writes out the file paths for all the files we wanted, from the initial to the final, regardless of whether we downloaded them at this point. This is for later use.
write(FileDestinationPath[!is.na(FileDestinationPath)], file = "DownloadedFilePaths.txt")

# We need to only run the download function if there are files that need downloading. Otherwise, R will spit out an unnecessary error.
# To avoid this, the If statement looks at the length of the FilesToDownLoad variable. If it is length 0 then there aren't any downloads needed.
if (length(FilesToDownload) == 0) {
  # If there are no downloads, this prints a message to the screen telling you that.
  cat("\n\nAll requested files already downloaded. You may continue.\n\n")
  
  # The Else statement then handles the cases where there are files to download.
} else {
  # This starts a timer to gage how long the download takes.
  tic()
  # This ensures that the source URLs for the files we want to download point to the appropriate files.
  FileSourcing = FileSourcePaths[sapply(seq_along(FilesToDownload), function (x) which(as.vector(FileDestinationPath) == FilesToDownload[x]))]
  
  # This runs the actual downloading. For each download that ran properly, it should output a 0 to the variable. If there is a failure it will output something else.
  DownloadActual = sapply(seq_along(FilesToDownload), function (x) FileDownloaderFunction(FileURL = FileSourcing[x], FileDest = FilesToDownload[x]))
  
  # We want to warn the user if there were any failures (particularly since the error messages to the screen for each individual file are suppressed). 
  # This If statement does that by going along the DownloadActual vector and checking that all elements are 0s. If so there were no errors.
  if(all(sapply(seq_along(DownloadActual), function (x) identical(DownloadActual[x],"0")))) {
    # This ends the timer. The time in seconds is displayed on the console.
    toc()
    # Since there were no errors, this prints out a message to the console saying that.
    cat("\n\nThe program has downloaded all files. You may continue.\n\n")
    
    # The Else statement deals with the case where there was at least one download error. It will then collect some information about the errors for troubleshooting.
  } else {
    # This variable gives the URLs for the files with download errors.
    FailedDownloads = FileSourcing[which(DownloadActual != "0")]
    # This variable collects the error messages that were stored from the download when the download failed. This is useful for troubleshooting.
    DownloadErrors = DownloadActual[which(DownloadActual != "0")]
    
    # This ends the timer. The time in seconds is displayed on the console.
    toc()
    # Since there was an error this prints a message to the console alerting the user of the existence of the errors.
    cat("\n\nThe program has completed downloading. There were some errors.\n\nPlease see the \"FailedDownloads\" variable for the error files and the \"DownloadErrors\" variable for details.\n\n")
  }
  # This cleans up some variables only used in the If statement.
  rm(DownloadActual, FileSourcing)
}

# A Tibble is stored that contains all the information about which files we wanted. It can be saved after the script is finished running.
FileInfo = tibble(MonthsName, Years, FileDateName, FileDestinationPath)

# This cleans up unneeded variables in the environment after everything is completed.
rm(MonthsName, MonthsNum, FileCheck, FileSourcePaths, IDMax, IDNums, IDPoints, StartPoint, EndPoint, InitialPoint, Years, FileDestinationPath, FileDateName, FilesToDownload)
rm(list=lsf.str())

# Lastly, we restart the session in order to ensure that all connections, which are no longer needed, are broken.
.rs.restartR()
