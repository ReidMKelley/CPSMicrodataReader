ParserJanuary1994 = function(AA, DictionaryIn) {
  
  # These functions format the Household Information section
  
  AA$hrhhid[AA$hrhhid == -1] = NA
  AA$huinttyp = factor(AA$huinttyp, levels = -1:2, labels = c(NA, "Noninterview/Indeterminate", "Personal", "Telephone"))
  AA$huprscnt[AA$huprscnt == -1] = NA
  AA$hurespli[AA$hurespli == -1] = NA
  AA$hufinal = factor(AA$hufinal, 
                      levels = c(-1, 0, 1, 2, 5, 24, 115, 200, 201, 202, 203, 204, 205, 210, 216, 217, 218, 219, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 240, 241, 242, 243, 244, 245, 246, 247, 248),
                      labels = c(NA, "New interview - Not contacted", "Fully complete CATI interview", "Partially completed CATI interview", "Labor force complete, Supplement incomplete - CATI", "HH occupied entirely by Armed Forces members",
                                 "Partial interview with callback panned - CATI", "New interview - Contacted", "CAPI Complete", "Callback needed", "Sufficient partial - Precloseout",
                                 "Sufficient partial - At closeout", "Labor force complete, - Suppl. incomplete - CAPI", "CAPI complete reinterview", "No one home", "Temporarily absent",
                                 "Refused", "Other occupied - Specify", "Armed Forces occupied or under age 14", "Temp. occupied w/persons with URE", "Vacant regular",
                                 "Vacant - Storage of HHLD furniture", "Unfit, to be demolished", "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent or trailer site",
                                 "Permit granted - Construction not started", "Other - Specify", "Demolished", "House or trailer moved", "Outside segment",
                                 "Converted to perm. business or storage", "Merged", "Condemned", "Built after April 1, 1980", "Unused serial no./listing sheet line",
                                 "Other - Specify"))
  AA$huspnish = factor(AA$huspnish, levels = c(-1, 1), labels = c(NA, "Spanish only language spoken"))
  AA$hetenure = factor(AA$hetenure, levels = c(-1, 1:3), labels = c(NA, "Owned or being bought by a HH member", "Rented for cash", "Occupied without payment of cash rent"))
  AA$hehousut = factor(AA$hehousut, levels = -1:12, labels = c(NA, "Other unit", "House, apartment, flat", "HU in nontransient hotel, motel, etc.", "HU permanent in transient hotel, motel",
                                                               "HU in rooming house", "Mobile home or trailer w/no perm. room added", "Mobile home or trailer w/1 or more perm. rooms added", "HU not specified above", "Quarters not HU in rooming or brding HS",
                                                               "Unit not perm. in transient hotl, motl", "Uoccupied tent site or trlr site", "Student quarters in college dorm", "Other unit not specified above"))
  AA$hetelhhd = factor(AA$hetelhhd, levels = c(-1, 1, 2), labels = c(NA, "Telephone present in house", "Telephone not present in the house"))
  AA$hetelavl = factor(AA$hetelavl, levels = c(-1, 1, 2), labels = c(NA, "Telephone elsewhere available for household to use", "Telephone not available eslewhere for household to use"))
  AA$hephoneo = factor(AA$hephoneo, levels = c(-1, 1, 2), labels = c(NA, "Telephone interview acceptable", "Telephone interview not acceptable"))
  AA$hufaminc = factor(AA$hufaminc, levels = c(-1, 1:14), labels = c(NA, "Less than $5,000", "5,000 to 7,499", "7,500 to 9,999", "10,000 to 12,499", 
                                                                    "12,500 to 14,999", "15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", 
                                                                    "35,000 to 39,999", "40,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 or more"))
  AA$hutypea = factor(AA$hutypea, levels = c(-1, 1:4), labels = c(NA, "No one home (NOH)", "Temporarily absent (TA)", "Refused (Ref)", "Other occupied - Specify"))
  AA$hutypb = factor(AA$hutypb, levels = c(-1, 1:9), labels = c(NA, "Vacant regular", "Temporarily occupied by persons w/ URE", "Vacant-storage of HHLD furniture", "Unfit or to be demolished", 
                                                                "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent site or trailer site", "Permit granted construction not started", "Other type B - Specify"))
  AA$hutypc = factor(AA$hutypc, levels = c(-1, 1:6,8:9), labels = c(NA, "Demolished", "House or trailer moved", "Outside segment", "Converted to perm. business or storage",
                                                                    "Merged", "Condemned", "Unused line of listing sheet", "Other - Specify"))
  AA$hwhhwgt[AA$hwhhwgt == -1] = NA
  AA$hrintsta = factor(AA$hrintsta, levels = c(-1, 1:4), labels = c(NA, "Interview", "Type A Non-interview", "Type B Non-interview", "Type C Non-interview"))
  AA$hrnumhou[AA$hrnumhou == -1] = NA
  AA$hrhtype = factor(AA$hrhtype, levels = c(-1:10), labels = c(NA, "Non-interview household", "Husband/Wife primary family (Neither AF)", "Husb/wife prim. family (Either/both AF)", "Unmarried civilian male-Prim. fam HHLDer",
                                                                "Unmarried civ. female-Prim. fam HHLDer", "Primary family HHLDer-RP in AF, Unmar.", "Civilian male primary individual", "Civilian female primary individual", "Primary individual HHLD-RP in AF",
                                                                "Group quarter with family", "Group quarters without family"))
  AA$hrmis[AA$hrmis == -1] = NA
  AA$hrmonth[AA$hrmonth == -1] = NA
  AA$hryear[AA$hyear == -1] = NA
  AA$hryear4 = 1900 + AA$hryear
  AA = AA %>% select(1:22, hryear4, everything())
  AA = select(AA, -hryear)
  AA$hrlonglk = factor(AA$hrlonglk, levels = c(-1, 0, 2, 3), labels = c(NA, "MIS 1 or replaement HH (No link)", "MIS 2-4 or MIS 6-8", "MIS 5"))
  
  # These set up the hrhhid2 variable for longitudinal linking
  AA$hrsample[AA$hrsample == "-1"] = NA
  AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
  AA$hrsersuf[is.na(AA$hrsersuf)] = -1
  AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")
  AA$hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum)
  AA = AA %>% select(1:23, hrhhid2,everything())
  
  AA$hubus = factor(AA$hubus, levels = c(-1, 1, 2), labels = c(NA, "Someone in HH has a business/farm", "No one in HH has a business/farm"))
  AA$hubusl1[AA$hubusl1 == -1] = NA
  AA$hubusl2[AA$hubusl2 == -1] = NA
  AA$hubusl3[AA$hubusl3 == -1] = NA
  AA$hubusl4[AA$hubusl4 == -1] = NA
  
  
  # These functions format the Geographic Information section
  AA$gereg = factor(AA$gereg, levels = c(-1, 1:4), labels = c(NA, "Northeast", "Midwest", "South", "West"))
  AA$gestcen = factor(AA$gestcen, levels = c(-1, 11:16, 21:23, 31:35, 41:47, 51:59, 61:64, 71:74, 81:88, 91:95), labels = c(NA, "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX", "MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI"))
  AA$gestfips[AA$gestfips == -1] = NA
  AA$gecmsa = factor(AA$gecmsa, levels = c(-1, 0, 7:97), labels = c(NA, "Not identified or Nonmetropolitan", str_c(7:97, " Specific CMSA Code")))
  AA$gemsa = factor(AA$gemsa, levels = c(-1, 0, 80:9360), labels = c(NA, "Not identified or nonmetropolitan", str_c(80:9360, " Specific MSA Code")))
  AA$geco = factor(AA$geco, levels = -1:810, labels = c(NA, "Not identified", str_c(1:810, " State-specific County Code")))
  AA$gemsast = factor(AA$gemsast, levels = c(-1, 1:4), labels = c(NA, "Central City", "Balance", "Nonmetropolitan", "Not identified"))
  AA$gemetsta = factor(AA$gemetsta, levels = c(-1, 1:3), labels = c("Metropolitan", "Nonmetropolitan", "Not identified"))
  AA$geindvcc = factor(AA$geindvcc, levels = -1:4, labels = c(NA, "Not identified, Nonmetropolitan, or Not a central city", str_c(1:4, " Specific central city code")))
  AA$gemsasz = factor(AA$gemsasz, levels = c(-1, 2:7), labels = c(NA, "Not identified or Nonmetropolitan", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  AA$gecmsasz = factor(AA$gecmsasz, levels = c(-1, 2:7), labels = c(NA, "Not identified or Nonmetropolitan", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  AA$hulensec[AA$hulensec == -1] = NA
  
  
  
  # These functions format the Personal Information Demographic section
  AA$proldrrp = factor(AA$proldrrp, levels = c(-1, 1:12), labels = c(NA, "Ref pers with other relatives in HH", "Ref pers with no other relatives in HH", "Spouse", "Child", "Grandchild", "Parent", "Brother/Sister", "Other relative", "Foster child", "Non-rel of Ref Per w/own rels in HH", "Partner/Roommate", "Non-rel of ref per w/no own rels in HH"))
  AA$pupelig = factor(AA$pupelig, levels = -1:12, labels = c(NA, "Eligible for interview", "Labor force fully complete", "Missing labor force data for person", "(Not used)", "Assigned if age is blank", "Armed forces member", "Under 15 years old", "Not a HH member", "Deleted", "Deceased", "End of list", "After end of list"))
  AA$perrp = factor(AA$perrp, levels = c(-1, 1:18), labels = c(NA, "Reference person w/Rels.", "Reference person w/o Rels.", "Spouse", "Child", "Grandchild", "Parent", "Brother/Sister", "Other Rel. of Reference person", "Foster child", "Nonrel. of Ref. person w/Rels.", "Not used", "Nonrel. of Ref. person w/o Rels.", "Unmarried partner w/Rels.", "Unmarried partner w/out Rels.", "Housemate/Roommate w/Rels.", "Housemate/Roommate w/out Rels.", "Roomer/Boarder w/ Rels.", "Roomer/Boarder w/out Rels."))
  AA$peparent = factor(AA$peparent, levels = c(-1, 1:99), labels = c("No parent", str_c(1:99, "Line Num of parent")))
  AA$peage = factor(AA$peage, levels = -1:90, labels = c(NA, str_c(0:89, " years old"), "90+ years old"))
  AA$ptage = factor(AA$ptage, levels = -1:1, labels = c(NA, "No top code", "Top coded value for age"))
  AA$pemaritl = factor(AA$pemaritl, levels = c(-1, 1:6), labels = c(NA, "Married - Spouse present", "Married - Spouse absent", "Widowed", "Divorced", "Separated", "Never married"))
  AA$pespouse[AA$pespouse == -1] = NA
  AA$pesex = factor(AA$pesex, levels = c(-1, 1, 2), labels = c(NA, "Male", "Female"))
  AA$puafever = factor(AA$puafever, levels = c(-1, 1, 2), labels = c(NA, "Served in the Armed Forces at some point", "Never served in the Armed Forces"))
  AA$peafwhen = factor(AA$peafwhen, levels = c(-1, 1:6), labels = c(NA, "Vietnam Era (8/64-4/75)", "Korean War (6/50-1/55)", "World War II (9/40-7/47)", "World War I (4/17-11/18)", "Other service (All other persiods)", "Nonveteran"))
  AA$peafnow = factor(AA$peafnow, levels = c(-1, 1, 2), labels = c(NA, "Currently in Armed Forces", "Not currently in Armed Forces"))
  AA$peeduca = factor(AA$peeduca, levels = c(-1, 31:46), labels = c(NA, "Less than 1st grade", "1st, 2nd, 3rd or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade no diploma", "High school grad-diploma or equivalent (GED)", "Some college but no degree", "Associate degree-Occupational/Vocational", "Associate degree-Academic program", "Bachelor's degree (Ex: BA, AB, BS)", "Master's degree (Ex: MA, MS, MEng, MEd, MSW)", "Professional School Deg (Ex: MD, DDS, DVM)", "Doctorate degree (Ex: PhD, EdD)"))
  AA$perace = factor(AA$perace, levels = c(-1, 1:5), labels = c(NA, "White", "Black", "American Indian, Aleut, Eskimo", "Asian or Pacific Islander", "Other - Specify"))
  AA$prorigin = factor(AA$prorigin,  levels = c(-1, 1:10), labels = c(NA, "Mexican American", "Chicano", "Mexican (Mexicano)", "Puerto Rican", "Cuban", "Central or South American", "Other Spanish", "All other", "Don't know", "NA"))
  AA$puchinhh = factor(AA$puchinhh, levels = c(-1, 1:9), labels = c(NA, "Person added", "Person added - URE", "Person undeleted", "Person died", "Deleted for reason other than death", "Person joined Armed Forces", "Person no longer in AF", "Change in demographic information"))
  AA$purelflg = factor(AA$purelflg, levels = -1:1, labels = c(NA, "Not owner or related to owner of business", "Owner of business or related to owner of business"))
  AA$pulineno[AA$plineno == -1] = NA
  AA$prfamnum = factor(AA$prfamnum, levels = -1:19, labels = c(NA, "Not a family member", "Primary family member only", "Subfamily No. 2 member", "Subfamily No. 3 member", "Subfamily No. 4 member", "Subfamily No. 5 member", "Subfamily No. 6 member", "Subfamily No. 7 member", "Subfamily No. 8 member", "Subfamily No. 9 member", "Subfamily No. 10 member", "Subfamily No. 11 member", "Subfamily No. 12 member", "Subfamily No. 13 member", "Subfamily No. 14 member", "Subfamily No. 15 member", "Subfamily No. 16 member", "Subfamily No. 17 member", "Subfamily No. 18 member", "Subfamily No. 19 member"))
  AA$prfamrel = factor(AA$prfamrel, levels = -1:4, labels = c(NA, "Not a family member", "Reference person", "Spouse", "Child", "Other relative (Primary Family & Unrel)"))
  AA$prfamtyp = factor(AA$prfamtyp, levels = c(-1, 1:5), labels = c(NA, "Primary family", "Primary individual", "Related subfamily", "Unrelated subfamily", "Secondary individual"))
  AA$prhspnon = factor(AA$prhspnon, levels = c(-1, 1, 2), labels = c(NA, "Hispanic", "Non-Hispanic"))
  AA$prmarsta = factor(AA$prmarsta, levels = c(-1, 1:7), labels = c(NA, "Married, civilian spouse present", "Married, Armed Forces spouse present", "Married, Spouse absent (Exc. Separated)", "Widowed", "Divorced", "Separated", "Never married"))
  AA$prpertyp = factor(AA$prpertyp, levels = c(-1, 1:3), labels = c(NA, "Child household member", "Adult civilian household member", "Adult armed forces household member"))
  AA$penatvty = factor(AA$penatvty, levels = c(-1, 57, 72, 96, 100:554, 555), labels = c(NA, "Person born in United States", "Person born in Puerto Rico", "Person born in U.S. Outlying Area", str_c(100:554, " Person born in Foreign Country or at sea - See Code list"), "Person born Abroad, country not known"))
  AA$pemntvty = factor(AA$pemntvty, levels = c(-1, 57, 72, 96, 100:554, 555), labels = c(NA, "Person's mother born in United States", "Person's mother born in Puerto Rico", "Person's mother born in U.S. Outlying Area", str_c(100:554, " Person's mother born in Foreign Country or at sea - See Code list"), "Person's mother born Abroad, country not known"))
  AA$pefntvty = factor(AA$pefntvty, levels = c(-1, 57, 72, 96, 100:554, 555), labels = c(NA, "Person's father born in United States", "Person's father born in Puerto Rico", "Person's father born in U.S. Outlying Area", str_c(100:554, " Person's father born in Foreign Country or at sea - See Code list"), "Person's father born Abroad, country not known"))
  AA$prcitshp = factor(AA$prcitshp, levels = c(-1, 1:5), labels = c(NA, "Native, Born in the United States", "Native, Born in Puerto Rico or U.S. Outlying Area", "Native, Born abroad of American parent or parents", "Foreign born, U.S. citizen by naturalization", "Foreign born, not a citizen of the United States"))
  # Need to look more into the allocation flag issue for this following variable
  AA$prcitflg[AA$prcitflg == -1] = NA
  if (AA$hryear4 == 1995) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:13, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1995"))
  } else if (AA$hryear4 == 1996) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:14, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1996"))
  } else if (AA$hryear4 == 1997) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:14, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1997"))
  } else if (AA$hryear4 == 1998) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:15, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1995", "Immigrant entered in 1996-1998"))
  }
  
  
  # These functions format the Personal Information Labor Force section
  AA$puslfprx = factor(AA$puslfprx, levels = c(-1, 1:3), labels = c(NA, "Labor Force Info collected by self", "Labor Force info collected by proxy", "Labor Force info collected by both self and proxy"))
  AA$pemlr = factor(AA$pemlr, levels = c(-1, 1:7), labels = c(NA, "Employed-At work", "Employed-Absent", "Unemployed-On layoff", "Unemployed-Looking", "Not in Labor Force-Retired", "Not in Labor Force-Disabled", "Not in labor force-Other"))
  AA$puwk = factor(AA$puwk, levels = c(-1,1:5), labels = c(NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  
   
  
}


# X = sort(unique(AA$hufinal))
# XX = sort(c(X, 0, 24, 200, 210))
# View(XX)
