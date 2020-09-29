
# This function is set up to create a tibble that connects the ID number for the microdata file with the appropriate Dictionary number and name for it.
# The entire function will need to be updated whenever a new DataDicitonary is released.
DictionaryConnector = function(EndMonthVal, EndYearVal) {
  # EndPoint is used to select the ending value of the sequence. It is set so that if the chosen enddate is before August 2020, the tibble populates out to August 2020.
  # If the enddate is after August 2020, it extends the sequence out to that point.
  EndPoint = max(12*EndYear + EndMonth - (12*2020 + 8),0)
  
  # The IDs are the 3-digit numbers that uniquely identify each month of microdata from September 1995 onwards. 
  IDs = 1:(300+EndPoint)
  
  # DictionaryNum and DictionaryNames give the number and name of the appropriate data dictionary for the corresponding month given by the row ID value. 
  # EndPoint is used in selecting the length of the last set of repitions, hence the updating needs.
  DictionaryNum = c(rep(1,28),rep(2,60),rep(3,16),rep(4,15),rep(5,17),rep(6,24),rep(7,12),rep(8,28),rep(9,8),rep(10,12),rep(11,12),rep(12,24),rep(13,36),rep(14,8+EndPoint))
  DictionaryNames = c(rep("Sep95",28),rep("Jan98",60),rep("Jan03",16),rep("May04",15),rep("Aug05",17),rep("Jan07",24),rep("Jan09",12),rep("Jan10",28),rep("May12",8),rep("Jan13",12),rep("Jan14",12),rep("Jan15",24),rep("Jan17",36),rep("Jan20",8+EndPoint))
  
  # This groups everything and outputs the info to the main script in a Tibble.
  ConnectorOut = tibble(IDs, DictionaryNum, DictionaryNames)
  return(ConnectorOut)
}
  
  



CPSMicrodataReader = function(FileIn, DataDictionaryIn) {
  
  DataDictionaryIn = filter(DataDictionaryIn, Adjustment != "Add")
  Colspec = str_flatten(str_replace_all(str_trunc(DataDictionaryIn$colClasses, 1, side = "right", ellipsis = ""), "n", "d"))
  A = read_fwf(file = FileIn, fwf_positions(DataDictionaryIn$StartPos, end = DataDictionaryIn$EndPos, col_names = DataDictionaryIn$ColName), col_types = Colspec)
  
# Decimal Imputation for those columns that need it.
  AA = which(!is.na(DataDictionaryIn$Decimals))
  for(x in seq_along(AA)) {
    A[[AA[x]]] = DecimalImputation(A[[AA[x]]], DataDictionaryIn$Decimals[AA[x]])
  }
  
  DataOut =  A
  return(DataOut)
  
  
}

DecimalImputation = function(DataColIn, DecimalValueIn) {
  DivisorVal = 10^DecimalValueIn
  A = which(DataColIn == -1)
  a = setdiff(1:length(DataColIn), A)
  B = replace(DataColIn, A, NA)
  C = B/DivisorVal
  return(C)
}

DataFileLocVal = function(MonthNumberIn) {

  PrefixValu = c(199401, 199404, 199506, 199509, 199801, 200301, 200405, 200508, 200701, 200901, 201001, 201205, 201301, 201401, 201501, 201706, 1000000)
  PrefixList = c(199401, 199404, 199506, 199509, 199801, 200301, 200405, 200508, 200701, 200901, 201001, 201205, 201301, 201401, 201501, 201701)
  PrefixStrn = c("199401-199403/", "199404-199505/", "199506-199508/", "199509-199712/", "199801-/", "200301-/", "200405-/", "200508-/", "200701-/", "200901-/", "201001-/", "201205-/", "201301-/", "201401-/", "201501-/", "201701-/")
  CD = matrix(0, nrow = dim(MonthNumberIn)[1], ncol = dim(MonthNumberIn)[2])
  for (j in 1:16) {
    CC = sapply(1:dim(MonthNumberIn)[2], function(x) between(MonthNumberIn[,x], PrefixValu[j], PrefixValu[j+1] - 0.1))
    CD1 = sapply(1:dim(MonthNumberIn)[2], function(y) replace(CC[,y], which(CC[,y] == TRUE), PrefixList[j]))
    CD = CD + CD1
  }
  XX = sapply(1:dim(MonthNumberIn)[2], function(x) as.character(CD[,x]))
  for (j in 1:16) {
    YY = sapply(1:dim(MonthNumberIn)[2], function(z) str_replace(XX[,z], pattern = as.character(PrefixList[j]), replacement = PrefixStrn[j]))
    XX = YY
  }
     
  return(XX)
}

DataDictionaryLocVal = function(MonthNumberIn) {
  PrefixValu = c(199401, 199404, 199506, 199509, 199801, 200301, 200405, 200508, 200701, 200901, 201001, 201205, 201301, 201401, 201404, 201501, 201706, 1000000)
  PrefixList = c(199401, 199404, 199506, 199509, 199801, 200301, 200405, 200508, 200701, 200901, 201001, 201205, 201301, 201401, 201404, 201501, 201701)
  PrefixStrn = c("cpsbjan94.dct", "cpsbapr94.dct", "cpsbjun95.dct", "cpsbsep95.dct", "cpsbjan98.dct", "cpsbjan03.dct", "cpsbmay04.dct", "cpsbaug05.dct", "cpsbjan07.dct", "cpsbjan09.dct", "cpsbjan10.dct", "cpsbmay12.dct", "cpsbjan13.dct", "cpsbjan2014.dct", "cpsbapr2014.dct", "cpsbjan2015.dct", "cpsbjan2017.dct")
  CD = matrix(0, nrow = dim(MonthNumberIn)[1], ncol = dim(MonthNumberIn)[2])
  for (j in 1:17) {
    CC = sapply(1:dim(MonthNumberIn)[2], function(x) between(MonthNumberIn[,x], PrefixValu[j], PrefixValu[j+1] - 0.1))
    CD1 = sapply(1:dim(MonthNumberIn)[2], function(y) replace(CC[,y], which(CC[,y] == TRUE), PrefixList[j]))
    CD = CD + CD1
  }
  XX = sapply(1:dim(MonthNumberIn)[2], function(x) as.character(CD[,x]))
  for (j in 1:17) {
    YY = sapply(1:dim(MonthNumberIn)[2], function(z) str_replace(XX[,z], pattern = as.character(PrefixList[j]), replacement = PrefixStrn[j]))
    XX = YY
  }
  
  return(XX)
}

FileDownloaderFunction = function(FileURL, FileDest) {
  if((!is.na(FileURL))&&(!is.na(FileDest))) {
    FileOut = try(suppressWarnings(download.file(url = FileURL, destfile = FileDest)), silent = TRUE)
  } else {
    FileOut = NA
  }
  return(FileOut)
}


FileUnziper = function(FileToUnzip, ExtractedFileDest) {
  if((!is.na(FileToUnzip))&&(!is.na(ExtractedFileDest))) {
    FileOut = unzip(zipfile = FileToUnzip, exdir = ExtractedFileDest)
  } else {
    FileOut = NA
  }
  return(FileOut)
}
  
  
FileMover = function(FileToMove, DestinationOfFile) {
  if((!is.na(FileToMove))&&(!is.na(DestinationOfFile))) {
    FileOut = file.move(files = FileToMove, DestinationOfFile)
  } else {
    FileOut = NA
  }
  return(FileOut)
}