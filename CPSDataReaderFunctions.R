CPSMicrodataReader = function(FileIn, DataDictionaryIn) {
  
  
  Colspec = str_flatten(str_replace_all(str_trunc(DataDictionary$colClasses, 1, side = "right", ellipsis = ""), "n", "d"))
  A = read_fwf(file = FileIn, fwf_positions(DataDictionaryIn$StartPos, end = DataDictionaryIn$EndPos, col_names = DataDictionaryIn$ColName), col_types = Colspec)
  
# Decimal Imputation for those columns that need it.
  AA = which(!is.na(DataDictionaryIn$Decimals))
  for(x in seq_along(AA)) {
    A[[AA[x]]] = DecimalImputation(A[[AA[x]]], DataDictionaryIn$Decimals[AA[x]])
  }
  
  # 
  # # Adjusting the variables that have important factors to provide useful labels
  # A$hehousut = factor(A$hehousut, levels = 0:12, labels = c("OTHER UNIT", "HOUSE, APARTMENT, FLAT", "HU IN NONTRANSIENT HOTEL, MOTEL, ETC.", "HU PERMANENT IN TRANSIENT HOTEL, MOTEL", "HU IN ROOMING HOUSE", "MOBILE HOME OR TRAILER W/NO PERM. ROOM ADDED", "MOBILE HOME OR TRAILER W/1 OR MORE PERM. ROOMS ADDED", "HU NOT SPECIFIED ABOVE", "QUARTERS NOT HU IN ROOMING OR BRDING HS", "UNIT NOT PERM. IN TRANSIENT HOTL, MOTL", "UNOCCUPIED TENT SITE OR TRLR SITE", "STUDENT QUARTERS IN COLLEGE DORM", "OTHER UNIT NOT SPECIFIED ABOVE"))
  # A$hrintsta = factor(A$hrintsta, levels = 1:4, labels = c("INTERVIEW", "TYPE A NON-INTERVIEW", "TYPE B NON-INTERVIEW", "TYPE C NON-INTERVIEW"))
  # A$hrhtype = factor(A$hrhtype, levels = 0:10, labels = c("NON-INTERVIEW HOUSEHOLD", "HUSBAND/WIFE PRIMARY FAMILY (NEITHER AF)", "HUSB/WIFE PRIM. FAMILY (EITHER/BOTH AF)", "UNMARRIED CIVILIAN MALE-PRIM. FAM HHLDER", "UNMARRIED CIV. FEMALE-PRIM FAM HHLDER", "PRIMARY FAMILY HHLDER-RP IN AF, UNMAR.", "CIVILIAN MALE PRIMARY INDIVIDUAL", "CIVILIAN FEMALE PRIMARY INDIVIDUAL", "PRIMARY INDIVIDUAL HHLD-RP IN AF", "GROUP QUARTERS WITH FAMILY", "GROUP QUARTERS WITHOUT FAMILY"))
  # A$hrlonglk = factor(A$hrlonglk, levels = c(0,2,3), labels = c("MIS 1 OR REPLACEMENT HH (NO LINK)", "MIS 2-4 OR MIS 6-8", "MIS 5"))
  # A$gereg = factor(A$gereg, levels = 1:4, labels = c("NORTHEAST", "MIDWEST (FORMERLY NORTH CENTRAL)", "SOUTH", "WEST"))
  # A$gestfips = factor(A$gestfips)
  # A$gediv = factor(A$gediv, levels = 1:9, labels = c("NEW ENGLAND", "MIDDLE ATLANTIC", "EAST NORTH CENTRAL", "WEST NORTH CENTRAL", "SOUTH ATLANTIC", "EAST SOUTH CENTRAL", "WEST SOUTH CENTRAL", "MOUNTAIN", "PACIFIC"))
  # A$gtcbsast = factor(A$gtcbsast, levels = 1:4, labels = c("PRINCIPAL CITY", "BALANCE", "NONMETROPOLITAN", "NOT IDENTIFIED"))
  # A$gtmetsta = factor(A$gtmetsta, levels = 1:3, labels = c("METROPOLITAN", "NONMETROPOLITAN", "NOT IDENTIFIED"))
  # A$gtcbsasz = factor(A$gtcbsasz, levels = c(0,2:7), labels = c("NOT IDENTIFIED OR NONMETROPOLITAN", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  # A$prtfage = factor(A$prtfage, levels = 0:1, labels = c("NO TOP CODE", "TOP CODED VALUE FOR AGE"))
  # A$pthr = factor(A$pthr, levels = 0:1, labels = c("NOT TOPCODED", "TOPCODED"))
  # A$ptot = factor(A$ptot, levels = 0:1, labels = c("NOT TOPCODED", "TOPCODED"))
  # A$prchld = factor(A$prchld, levels = -1:15, labels = c("NIU (Not a parent)", "No own children under 18 years of age", "All own children 0- 2 years of age", "All own children 3- 5 years of age", "All own children 6-13 years of age", "All own children 14-17 years of age", "Own children 0- 2 and 3- 5 years of age (none 6-17)", "Own children 0- 2 and 6-13 years of age (none 3- 5 or 14-17)", "Own children 0- 2 and 14-17 years of age (none 3-13)", "Own children 3- 5 and 6-13 years of age (none 0- 2 or 14-17)", "Own children 3- 5 and 14-17 years of age (none 0- 2 or 6-13)", "Own children 6-13 and 14-17 years of age (none 0- 5)", "Own children 0- 2, 3- 5, and 6-13 years of age (none 14-17)", "Own children 0- 2, 3- 5, and 14-17 years of age (none 6-13)", "Own children 0- 2, 6-13, and 14-17 years of age (none 3- 5)", "Own children 3- 5, 6-13, and 14-17 years of age (none 0- 2)", "Own children from all age groups"))
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
    FileOut = download.file(url = FileURL, destfile = FileDest)
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