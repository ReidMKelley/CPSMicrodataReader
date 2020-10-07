ParserSeptember1995 = function(DataIn, DataDictionaryIn) {
  
  # This eliminates all of the variables in the dataset that are labelled "Remove" in the Dictionary Files.
  AA = select(DataIn, -all_of(filter(DataDictionaryIn, Adjustment == "Remove")$ColName))
  
  # These format properly the three variables that go into forming the HRHHID2 variable.
  AA$hrsample[AA$hrsample == "-1"] = NA
  AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
  AA$hrsersuf[is.na(AA$hrsersuf)] = -1
  AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")
  # This adds the variables that the Dictionary says to add, properly applying the formulas to create them.
  AA = tibble(AA, hryear4 = 1900 + AA$hryear, hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum), gecmsanum = AA$gecmsa, gemsanum = AA$gemsa, geconum = AA$geco)
  # This removes all of the variables that are labelled "Delete" in the dataset. 
  AA = select(AA, -all_of(filter(DataDictionaryIn, Adjustment == "Delete")$ColName))
  # This reorders all of the remaining variables to match the Dictionary Order
  AA = select(AA, filter(filter(DataDictionaryIn, Adjustment != "Remove"), Adjustment != "Delete")$ColName)
  
  
  
  
  
  
  
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
  
  
  AA$hryear4[AA$hryear4 == -1] = NA
  AA$hrlonglk = factor(AA$hrlonglk, levels = c(-1, 0, 2, 3), labels = c(NA, "MIS 1 or replaement HH (No link)", "MIS 2-4 or MIS 6-8", "MIS 5"))
  AA$hubus = factor(AA$hubus, levels = c(-1, 1, 2), labels = c(NA, "Someone in HH has a business/farm", "No one in HH has a business/farm"))
  AA$hubusl1[AA$hubusl1 == -1] = NA
  AA$hubusl2[AA$hubusl2 == -1] = NA
  AA$hubusl3[AA$hubusl3 == -1] = NA
  AA$hubusl4[AA$hubusl4 == -1] = NA
  
  
  # These functions format the Geographic Information section
  AA$gereg = factor(AA$gereg, levels = c(-1, 1:4), labels = c(NA, "Northeast", "Midwest", "South", "West"))
  AA$gestcen = factor(AA$gestcen, levels = c(-1, 11:16, 21:23, 31:35, 41:47, 51:59, 61:64, 71:74, 81:88, 91:95), labels = c(NA, "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX", "MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI"))
  AA$gestfips[AA$gestfips == -1] = NA
  AA$gecmsa = factor(AA$gecmsa, levels = c(-3:-1, 0, 7:97), labels = c("Refused", "Don't Know", NA, "Not identified or Nonmetropolitan", str_c(7:97, " Specific CMSA Code")))
  AA$gemsa = factor(AA$gemsa, levels = c(-3:-1, 0, 80:9360), labels = c("Refused", "Don't Know", NA, "Not identified or nonmetropolitan", str_c(80:9360, " Specific MSA Code")))
  
  AA$geco = factor(AA$geco, levels = -3:810, labels = c("Refused", "Don't Know", NA, "Not identified", str_c(1:810, " State-specific County Code")))
  AA$gecmsanum[AA$gecmsanum <= -1] = NA
  AA$gemsanum[AA$gemsanum <= -1] = NA
  AA$geconum[AA$geconum <= -1] = NA
  AA$gemsast = factor(AA$gemsast, levels = c(-1, 1:4), labels = c(NA, "Central City", "Balance", "Nonmetropolitan", "Not identified"))
  
  
  AA$gemetsta = factor(AA$gemetsta, levels = c(-1, 1:3), labels = c(NA, "Metropolitan", "Nonmetropolitan", "Not identified"))
  AA$geindvcc = factor(AA$geindvcc, levels = -1:4, labels = c(NA, "Not identified, Nonmetropolitan, or Not a central city", str_c(1:4, " Specific central city code")))
  AA$gemsasz = factor(AA$gemsasz, levels = c(-1, 0, 2:7), labels = c(NA, "Not identified or Nonmetropolitan", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  
  
  
  # These functions format the Personal Information Demographic section
  AA$perrp = factor(AA$perrp, levels = c(-1, 1:18), labels = c(NA, "Reference person w/Rels.", "Reference person w/o Rels.", "Spouse", "Child", "Grandchild", "Parent", "Brother/Sister", "Other Rel. of Reference person", "Foster child", "Nonrel. of Ref. person w/Rels.", "Not used", "Nonrel. of Ref. person w/o Rels.", "Unmarried partner w/Rels.", "Unmarried partner w/out Rels.", "Housemate/Roommate w/Rels.", "Housemate/Roommate w/out Rels.", "Roomer/Boarder w/ Rels.", "Roomer/Boarder w/out Rels."))
  AA$peparent = factor(AA$peparent, levels = c(-1, 1:99), labels = c("No parent", str_c(1:99, " Line Num of parent")))
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
  AA$puchinhh = factor(AA$puchinhh, levels = c(-1, 1:7, 9), labels = c(NA, "Person added", "Person added - URE", "Person undeleted", "Person died", "Deleted for reason other than death", "Person joined Armed Forces", "Person no longer in AF", "Change in demographic information"))
  AA$pulineno[AA$pulineno == -1] = NA
  
  
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
  if (AA$hryear4[1] == 1995) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:13, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1995"))
  } else if (AA$hryear4[1] == 1996) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:14, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1996"))
  } else if (AA$hryear4[1] == 1997) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:14, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1997"))
  } else if (AA$hryear4[1] == 1998) {
    AA$prinusyr = factor(AA$prinusyr, levels = -1:15, labels = c("Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1995", "Immigrant entered in 1996-1998"))
  }
  
  
  # These functions format the Personal Information Labor Force section
  AA$puslfprx = factor(AA$puslfprx, levels = c(-1, 1:3), labels = c(NA, "Labor Force Info collected by self", "Labor Force info collected by proxy", "Labor Force info collected by both self and proxy"))
  AA$pemlr = factor(AA$pemlr, levels = c(-1, 1:7), labels = c(NA, "Employed-At work", "Employed-Absent", "Unemployed-On layoff", "Unemployed-Looking", "Not in Labor Force-Retired", "Not in Labor Force-Disabled", "Not in labor force-Other"))
  AA$puwk = factor(AA$puwk, levels = c(-1,1:5), labels = c(NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pubus1 = factor(AA$pubus1, levels = c(-1, 1, 2), labels = c(NA, "Person did unpaid work on family business/farm", "Person did not do unpaid work on family business/farm"))
  AA$pubus2ot = factor(AA$pubus2ot, levels = c(-1, 1, 2), labels = c(NA, "Person received payments or profits from the family business", "Person did not receive payments or profits from the family business"))
  
  
  AA$puretot = factor(AA$puretot, levels = c(-1, 1:3), labels = c(NA, "Yes, person who was reported retired last month and is retired now", "No, person who was reported retired last month is not retired now", "Person was not retired last month"))
  AA$pudis = factor(AA$pudis, levels = c(-1, 1:3), labels = c(NA, "Yes, person who was reported disabled last month and is disabled now", "No, person who was reported disabled last month is not disabled now", "Person was not disabled last month"))
  AA$peret1 = factor(AA$peret1, levels = c(-1, 1:3), labels = c(NA, "Yes, person who was reported retired last month wants a job (full or part-time)", "No, person who was reported retired last month does not want a job", "Person who was reported retired last month has a job now"))
  AA$pudis1 = factor(AA$pudis1, levels = c(-1, 1, 2), labels = c(NA, "Yes, person's disability prevents person from working in the next 6 months", "No, person's disability does not prevent person from accepting work in the next 6 months"))
  AA$pudis2 = factor(AA$pudis2, levels = c(-1, 1, 2), labels = c(NA, "Yes, person has a disability that prevents person from working", "No, person does not have a disability that prevents person from working"))
  
  
  AA$puabsot = factor(AA$puabsot, levels = c(-1, 1:5), labels = c(NA, "Yes, person had a full/part-time job last week", "No, person didn't have a full/part-time job last week", "Person was retired last week", "Person was disabled last week", "Person was unable to work last week"))
  AA$pulay = factor(AA$pulay, levels = c(-1, 1:5), labels = c(NA, "Yes, person is on layoff from a job", "No, person is not on layoff from a job", "Person is retired", "Person is disabled", "Person is unable to work"))
  AA$peabsrsn = factor(AA$peabsrsn, levels = c(-1, 1:14), labels = c(NA, "On layoff", "Slack work/Business conditions", "Waiting for a new job to begin", "Vacation/Personal days", "Own illness/Injury/Medical problems", 
                                                                     "Child care problems", "Other family/Personal obligation", "Maternity/Paternity leave", "Labor dispute", "Weather affected job", 
                                                                     "School/Training", "Civic/Military Duty", "Does not work in the business", "Other (Specify)"))
  AA$peabspdo = factor(AA$peabspdo, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pemjot = factor(AA$pemjot, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$pemjnum = factor(AA$pemjnum, levels = c(-1, 2:4), labels = c(NA, "2 jobs", "3 jobs", "4 or more jobs"))
  AA$pehrusl1[AA$pehrusl1 == -1] = NA
  AA$pehrusl2[AA$pehrusl2 == -1] = NA
  AA$pehrftpt = factor(AA$pehrftpt, levels = c(-1, 1:3), labels = c(NA, "Yes", "No", "Hours vary"))
  AA$pehruslt[AA$pehruslt == -1] = NA
  
  
  AA$pehrwant = factor(AA$pehrwant, levels = c(-1, 1:3), labels = c(NA, "Yes", "No", "Regular hours are Full-time"))
  AA$pehrrsn1 = factor(AA$pehrrsn1, levels = c(-1, 1:10), labels = c(NA, "Slack work/Business conditions", "Could only find Part-time work", "Seasonal work", "Child care problems", "Other family/Personal obligation", "Health/Medical limitations", "School/Training", "Retired/Social security limit on earnings", "Full-time workweek is less than 35 hrs", "Other - Specify"))
  AA$pehrrsn2 = factor(AA$pehrrsn2, levels = c(-1, 1:7), labels = c(NA, "Child care problems", "Other family/Personal obligations", "Health/Medical limitations", "School/Training", "Retired/Social Security limit on earnings", "Full-Time workweek less than 35 hours", "Other - Specify"))
  AA$pehrrsn3 = factor(AA$pehrrsn3, levels = c(-1, 1:13), labels = c(NA, "Slack work/Business conditions", "Seasonal work", "Job started or ended during week", "Vacation/Personal day", "Own illnes/Injury/Medical appointment", "Holiday (Legal or Religious)", "Child care problems", "Other family/Personal obligations", "Labor dispute", "Weather affected job", "School/training", "Civic/Military duty", "Other reason"))
  AA$puhroff1 = factor(AA$puhroff1, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$puhroff2[AA$puhroff2 == -1] = NA
  AA$puhrot1 = factor(AA$puhrot1, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$puhrot2[AA$puhrot2 == -1] = NA
  AA$pehract1[AA$pehract1 == -1] = NA
  
  
  AA$pehract2[AA$pehract2 == -1] = NA
  AA$pehractt[AA$pehractt == -1] = NA
  AA$pehravl = factor(AA$pehravl, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pulaydt = factor(AA$pulaydt, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pulay6m = factor(AA$pulay6m, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$pelayavl = factor(AA$pelayavl, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pulayavr = factor(AA$pulayavr, levels = c(-1, 1:3), labels = c(NA, "Own temporary illness", "Going to school", "Other"))
  AA$pelaylk = factor(AA$pelaylk, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pelaydur[AA$pelaydur == -1] = NA
  AA$pelayfto = factor(AA$pelayfto, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$pulk = factor(AA$pulk, levels = c(-1, 1:5), labels = c(NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pelkm1 = factor(AA$pelkm1, levels = c(-1, 1:13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                 "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                 "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkm2 = factor(AA$pulkm2, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                     "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                     "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm3 = factor(AA$pulkm3, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                     "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                     "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm4 = factor(AA$pulkm4, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                     "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                     "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkm5 = factor(AA$pulkm5, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                     "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                     "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm6 = factor(AA$pulkm6, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                     "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                     "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk1 = factor(AA$pulkdk1, levels = c(-1, 1:13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                   "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                   "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkdk2 = factor(AA$pulkdk2, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk3 = factor(AA$pulkdk3, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkdk4 = factor(AA$pulkdk4, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk5 = factor(AA$pulkdk5, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk6 = factor(AA$pulkdk6, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps1 = factor(AA$pulkps1, levels = c(-1, 1:13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                   "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                   "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkps2 = factor(AA$pulkps2, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkps3 = factor(AA$pulkps3, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps4 = factor(AA$pulkps4, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps5 = factor(AA$pulkps5, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps6 = factor(AA$pulkps6, levels = c(-1, 1:11, 13), labels = c(NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                       "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                       "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pelkavl = factor(AA$pelkavl, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pulkavr = factor(AA$pulkavr, levels = c(-1, 1:4), labels = c(NA, "Waiting for new job to begin", "Own temporary illness", "Going to school", "Other - Specify"))
  AA$pelkll1o = factor(AA$pelkll1o, levels = c(-1, 1:4), labels = c(NA, "Working", "School", "Left military service", "Something else"))
  AA$pelkll2o = factor(AA$pelkll2o, levels = c(-1, 1:3), labels = c(NA, "Lost job", "Quit job", "Temporary job ended"))
  AA$pelklwo = factor(AA$pelklwo, levels = c(-1, 1:3), labels = c(NA, "Last worked within the last 12 months", "Last worked more than 12 months ago", "Never worked previously"))
  
  
  AA$pelkdur[AA$pelkdur == -1] = NA
  AA$pelkfto = factor(AA$pelkfto, levels = c(-1, 1:3), labels = c(NA, "Yes", "No", "Doesn't matter"))
  AA$pedwwnto = factor(AA$pedwwnto, levels = c(-1, 1:5), labels = c(NA, "Yes, or maybe, it depends", "No", "Retired", "Disabled", "Unable to work"))
  AA$pedwrsn = factor(AA$pedwrsn, levels = c(-1, 1:11), labels = c(NA, "Believes no work available in area of expertise", "Couldn't find any work", "Lacks necessary schooling/training", "Employers think too young or too old",
                                                                   "Other types of discrimination", "Can't arrange child care", "Family responsibilities", "In school or other training", "Ill-health, physcial disability",
                                                                   "Transportation problems", "Other - Specify"))
  AA$pedwlko = factor(AA$pedwlko, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$pedwwk = factor(AA$pedwwk, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pedw4wk = factor(AA$pedw4wk, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pedwlkwk = factor(AA$pedwlkwk, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pedwavl = factor(AA$pedwavl, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pedwavr = factor(AA$pedwavr, levels = c(-1, 1:3), labels = c(NA, "Own temporary illness", "Going to school", "Other"))
  
  
  AA$pejhwko = factor(AA$pejhwko, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pujhdp1o = factor(AA$pujhdp1o, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$pejhrsn = factor(AA$pejhrsn, levels = c(-1, 1:8), labels = c(NA, "Personal/Family (Including Pregnancy)", "Return to school", "Health", "Retirement or old age", "Temp, Seasonal or intermittent job complete", "Slack work/business conditions", "Unsatisfactory work arrangements (Hrs, pay, etc.)", "Other - specify"))
  AA$pejhwant = factor(AA$pejhwant, levels = c(-1, 1, 2), labels = c(NA, "Yes, or it depends", "No"))
  AA$prabsrea = factor(AA$prabsrea, levels = c(-1, 1:40), labels = c(NA, "FT paid-Vacation", "FT paid-Own illness", "FT paid-Child care problems", "FT paid-Other family/Personal oblig.",
                                                                     "FT paid-Maternity/Paternity leave", "FT paid-Labor dispute", "FT paid-Weather affected job", "FT paid-School/Training", "FT paid-Civic/Military duty",
                                                                     "FT paid-Other", "FT unpaid-Vacation", "FT unpaid-Own illness", "FT unpaid-Child care problems", "FT unpaid-Other fam/Personal obligation",
                                                                     "FT unpaid-Maternity/Paternity leave", "FT unpaid-Labor dispute", "FT unpaid-Weather affected job", "FT unpaid-School/Training", "FT unpaid-Civic/Military duty",
                                                                     "FT unpaid-Other", "PT paid-Vacation", "PT paid-Own illness", "PT paid-Child care problems", "PT paid-Other family/Personal oblig.",
                                                                     "PT paid-Maternity/Paternity leave", "PT paid-Labor dispute", "PT paid-Weather affected job", "PT paid-School/Training", "PT paid-Civic/Military duty",
                                                                     "PT paid-Other", "PT unpaid-Vacation", "PT unpaid-Own illness", "PT unpaid-Child care problems", "PT unpaid-Other fam/Personal obligation",
                                                                     "PT unpaid-Maternity/Paternity leave", "PT unpaid-Labor dispute", "PT unpaid-Weather affected job", "PT unpaid-School/Training", "PT unpaid-Civic/Military duty",
                                                                     "PT unpaid-Other"))
  
  
  AA$prcivlf = factor(AA$prcivlf, levels = c(-1, 1, 2), labels = c(NA, "In Civilian Labor Force", "Not in Civilian Labor Force"))
  AA$prdisc = factor(AA$prdisc, levels = c(-1, 1:3), labels = c(NA, "Discouraged worker", "Conditionally interested", "Not available"))
  AA$premphrs = factor(AA$premphrs, levels = -1:22, labels = c(NA, "Unemployed and NILF", "W/job, Not at work-Illness", "W/job, not at work-Vacation", "W/job, not at work-Weather affected job",
                                                               "W/job, not at work-Labor dispute", "W/job, not at work-Child care problems", "W/job, not at work-Fam/Pers obligation", "W/job, not at work-Maternity/Paternity", "W/job, not at work-School/Training",
                                                               "W/job, not at work-Civic/Military duty", "W/job, not at work-Does not work in bus", "W/job, not at work-Other", "At work- 1-4 hrs", "At work- 5-14 hrs",
                                                               "At work- 15-21 hrs", "At work- 22-29 hrs", "At work- 30-34 hrs", "At work- 35-39 hrs", "At work- 40 hrs",
                                                               "At work- 41-47 hrs", "At work- 48 hrs", "At work- 49-59 hrs", "At work- 60 hrs or more"))
  AA$prempnot = factor(AA$prempnot, levels = c(-1, 1:4), labels = c(NA, "Employed", "Unemployed", "Not in the Labor Force (NILF)-discouraged", "Not in the Labor Force (NILF)-other"))
  AA$prexplf = factor(AA$prexplf, levels = c(-1, 1, 2), labels = c(NA, "Employed", "Unemployed"))
  
  
  AA$prftlf = factor(AA$prftlf, levels = c(-1, 1, 2), labels = c(NA, "Full Time Labor Force", "Part Time Labor Force"))
  AA$prhrusl = factor(AA$prhrusl, levels = c(-1, 1:8), labels = c(NA, "0-20 hrs", "21-34 hrs", "35-39 hrs", "40 hrs", "41-49 hrs", "50 or more hrs", "Varies-Full Time", "Varies-Part Time"))
  AA$prjobsea = factor(AA$prjobsea, levels = c(-1, 1:5), labels = c(NA, "Looked last 4 weeks - Not worked", "Looked last 4 weeks - Worked", "Looked last 4 weeks - Layoff", "Unavailable job seekers", "No recent job search"))
  AA$prpthrs = factor(AA$prpthrs, levels = -1:12, labels = c(NA, "Usually FT, PT for Noneconomic reasons", "Usu.FT, PT econ reasons; 1-4 hrs", "Usu.FT, PT econ reasons; 5-14 hrs", "Usu.FT, PT econ reasons; 15-29 hrs",
                                                             "Usu.FT, PT econ reasons; 30-34 hrs", "Usu.PT, econ reasons; 1-4 hrs", "Usu.PT, econ reasons; 5-14 hrs", "Usu.PT, econ reasons 15-29 hrs", "Usu.PT, econ reasons 30-34 hrs",
                                                             "Usu.PT, non-econ reasons; 1-4 hrs", "Usu.PT, non-econ reasons; 5-14 hrs", "Usu.PT, non-econ reasons; 15-29 hrs", "Usu.PT, non-econ reasons; 30-34 hrs"))
  AA$prptrea = factor(AA$prptrea, levels = c(-1, 1:23), labels = c(NA, "Usu. FT-Slack work/Business conditions", "Usu. FT-Seasonal work", "Usu. FT-Job started/ended during week", "Usu. FT-Vacation/Personal day",
                                                                   "Usu. FT-Own illness/Injury/Medical appointment", "Usu. FT-Holiday (Religious or Legal)", "Usu. FT-Child care problems", "Usu. FT-Other fam/Pers obligations", "Usu. FT-Labor dispute",
                                                                   "Usu. FT-Weather affected job", "Usu. FT-School/Training", "Usu. FT-Civic/Military Duty", "Usu. FT-Other reason", "Usu. PT-Slack work/Business conditions",
                                                                   "Usu. PT-Could only find PT work", "Usu. PT-Seasonal work", "Usu. PT-Child care problems", "Usu. PT-Other fam/Pers obligations", "Usu. PT-Health/Medical Limitations", 
                                                                   "Usu. PT-School/Training", "Usu. PT-Retired/S.S. limit on earnings", "Usu. PT-Workweek <35 hours", "Usu. PT-Other reason"))
  
  
  AA$prunedur[AA$prunedur == -1] = NA
  AA$pruntype = factor(AA$pruntype, levels = c(-1, 1:6), labels = c(NA, "Job loser/On layoff", "Other job loser", "Temportary job ended", "Job leaver", "Re-entrant", "New-entrant"))
  AA$prwksch = factor(AA$prwksch, levels = -1:4, labels = c(NA, "Not in Labor Force", "At work", "With job, not at work", "Unemployed, seeks FT", "Unemployed, seeks PT"))
  AA$prwkstat = factor(AA$prwkstat, levels = c(-1, 1:12), labels = c(NA, "Not in Labor Force", "FT hours (35+), usually FT", "PT for economic reasons, usually FT", "PT for non-economic reasons, usually FT",
                                                                     "Not at work, usually FT", "PT hrs, usually PT for economic reasons", "PT hrs, usually PT for non-economic reasons", "FT hours, usually PT for economic reasons", "FT hours, usually PT for non-economic reasons", 
                                                                     "Not at work, usually Part-Time", "Unemployed FT", "Unemployed PT"))
  AA$prwntjob = factor(AA$prwntjob, levels = c(-1, 1, 2), labels = c(NA, "Want a job", "Other Not in the Labor Force (NILF)"))
  
  
  AA$puiodp1 = factor(AA$puiodp1, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$puiodp2 = factor(AA$puiodp2, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$puiodp3 = factor(AA$puiodp3, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$peio1cow = factor(AA$peio1cow, levels = c(-1, 1:8), labels = c(NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay"))
  AA$puio1mfg = factor(AA$puio1mfg, levels = c(-1, 1:4), labels = c(NA, "Manufacturing", "Retail trade", "Wholesale trade", "Something else"))
  
  
  AA$peio1icd[AA$peio1icd == -1] = NA
  AA$peio1ocd[AA$peio1ocd == -1] = NA
  AA$peio2cow = factor(AA$peio2cow, levels = c(-1, 1:11), labels = c(NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay", "Unknown", "Government, Level unknown", "Self-employed, incorporation status unknown"))
  AA$puio2mfg = factor(AA$puio2mfg, levels = c(-1, 1:4), labels = c(NA, "Manufacturing", "Retail Trade", "Wholesale Trade", "Something else"))
  AA$peio2icd[AA$peio2icd == -1] = NA 
  
  
  AA$peio2ocd[AA$peio2ocd == -1] = NA
  AA$prioelg = factor(AA$prioelg, levels = -1:1, labels = c(NA, "Not eligible for edit", "Eligible for edit"))
  AA$pragna = factor(AA$pragna, levels = c(-1, 1, 2), labels = c(NA, "Agricultural", "Non-agricultural"))
  AA$prcow1 = factor(AA$prcow1, levels = c(-1, 1:6), labels = c(NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  AA$prcow2 = factor(AA$prcow2, levels = c(-1, 1:6), labels = c(NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  
  
  AA$prcowpg = factor(AA$prcowpg, levels = c(-1, 1, 2), labels = c(NA, "Private", "Government"))
  AA$prdtcow1 = factor(AA$prdtcow1, levels = c(-1, 1:11), labels = c(NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtcow2 = factor(AA$prdtcow2, levels = c(-1, 1:11), labels = c(NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtind1 = factor(AA$prdtind1, levels = c(-1, 1:52), labels = c(NA, "Goods producing-Agricultural services", "Goods producing-Other Agricultural", "Mining", "Construction", "Mfg-Lumber & Wood prods, excluding Furniture",
                                                                     "Mfg-Furniture & Fixtures", "Mfg-Stone, clay, concrete, Glass prods", "Mfg-Primary metals", "Mfg-Fabricated metals", "Mfg-Not specified metal industries",
                                                                     "Mfg-Machinery, excluding electrical", "Mfg-Electrical machinery, equipment supplies", "Mfg-Motor vehicles & equipment", "Mfg-aircraft & parts", "Mfg-Other transportation equipment",
                                                                     "Mfg-Professional & photo equipment, watches", "Mfg-Toys, amusement, & sporting goods", "Mfg-Miscellanious & NEC Mfg industries", "Mfg-Food & kindred products", "Mfg-Tobacco products",
                                                                     "Mfg-Textile mill products", "Mfg-Apparel & other finished textile PR", "Mfg-Paper & allied products", "Mfg-Printing, publishing & allied industries", "Mfg-Chemicals & allied products",
                                                                     "Mfg-Petroleum & coal products", "Mfg-Rubber & Miscellanious plastic products", "Mfg-Leather & leather products", "Transportation", "Communications",
                                                                     "Utilities & Sanitary services", "Wholesale trade", "Eating and drinking places", "Other retail trade", "Banking and other finance",
                                                                     "Insurance and Real estate", "Private household services", "Business services", "Automobile and repair services", "Personal services excluding private households",
                                                                     "Entertainment & recreation services", "Hospitals", "Health services, excluding hospitals", "Educational services", "Social services",
                                                                     "Other professional services", "Forestry & fisheries", "Justice, public order, & safety", "Admin of human resource programs", "National Security & Internal Affairs",
                                                                     "Other Public Administration", "Armed Forces"))
  AA$prdtind2 = factor(AA$prdtind2, levels = c(-1, 1:52), labels = c(NA, "Goods producing-Agricultural services", "Goods producing-Other Agricultural", "Mining", "Construction", "Mfg-Lumber & Wood prods, excluding Furniture",
                                                                     "Mfg-Furniture & Fixtures", "Mfg-Stone, clay, concrete, Glass prods", "Mfg-Primary metals", "Mfg-Fabricated metals", "Mfg-Not specified metal industries",
                                                                     "Mfg-Machinery, excluding electrical", "Mfg-Electrical machinery, equipment supplies", "Mfg-Motor vehicles & equipment", "Mfg-aircraft & parts", "Mfg-Other transportation equipment",
                                                                     "Mfg-Professional & photo equipment, watches", "Mfg-Toys, amusement, & sporting goods", "Mfg-Miscellanious & NEC Mfg industries", "Mfg-Food & kindred products", "Mfg-Tobacco products",
                                                                     "Mfg-Textile mill products", "Mfg-Apparel & other finished textile PR", "Mfg-Paper & allied products", "Mfg-Printing, publishing & allied industries", "Mfg-Chemicals & allied products",
                                                                     "Mfg-Petroleum & coal products", "Mfg-Rubber & Miscellanious plastic products", "Mfg-Leather & leather products", "Transportation", "Communications",
                                                                     "Utilities & Sanitary services", "Wholesale trade", "Eating and drinking places", "Other retail trade", "Banking and other finance",
                                                                     "Insurance and Real estate", "Private household services", "Business services", "Automobile and repair services", "Personal services excluding private households",
                                                                     "Entertainment & recreation services", "Hospitals", "Health services, excluding hospitals", "Educational services", "Social services",
                                                                     "Other professional services", "Forestry & fisheries", "Justice, public order, & safety", "Admin of human resource programs", "National Security & Internal Affairs",
                                                                     "Other Public Administration", "Armed Forces"))
  
  
  AA$prdtocc1 = factor(AA$prdtocc1, levels = c(-1, 1:46), labels = c(NA, "Officials & Administrators, Public Admin.", "Other executive, Admin. & Managerial", "Management related occupations", "Engineers", "Mathematical and Computer Scientists",
                                                                     "Natural Scientists", "Health diagnosing occupations", "Health assessment and treatment occupations", "Teachers, College and University", "Teachers, except College and University",
                                                                     "Lawyers and judges", "Other professional specialty occupations", "Health technologists and technicians", "Engineering and Science technicians", "Technicians, except Health, Engineering, and Science",
                                                                     "Supervisors and proprietors, sales occupations", "Sales reps, finance and business services", "Sales reps, Commodities, except Retail", "Sales workers, Retail & personal services", "Sales related occupations",
                                                                     "Supervisors, Administrative support", "Computer equipment operators", "Secretaries, stenographers, and typists", "Financial records processing", "Mail and message distribution",
                                                                     "Other Admin. support, including Clerical", "Private household service occupations", "Protective service", "Food service", "Health service",
                                                                     "Cleaning and building service", "Personal service", "Mechanics and repairers", "Construction trades", "Other precision production, craft, and repair",
                                                                     "Machine operator, and tenders, except precision", "Fabricators, assemblers, inspectors, samplers", "Motor vehicle operators", "Other transportation and material moving occupations", "Construction laborers",
                                                                     "Freight, Stock, & Materials Handlers", "Other handlers, Equipment cleaners, Helpers, Laborers", "Farm Operators and Managers", "Farm workers and related occupations", "Forestry and Fishing occupations",
                                                                     "Armed Forces"))
  AA$prdtocc2 = factor(AA$prdtocc2, levels = c(-1, 1:46), labels = c(NA, "Officials & Administrators, Public Admin.", "Other executive, Admin. & Managerial", "Management related occupations", "Engineers", "Mathematical and Computer Scientists",
                                                                     "Natural Scientists", "Health diagnosing occupations", "Health assessment and treatment occupations", "Teachers, College and University", "Teachers, except College and University",
                                                                     "Lawyers and judges", "Other professional specialty occupations", "Health technologists and technicians", "Engineering and Science technicians", "Technicians, except Health, Engineering, and Science",
                                                                     "Supervisors and proprietors, sales occupations", "Sales reps, finance and business services", "Sales reps, Commodities, except Retail", "Sales workers, Retail & personal services", "Sales related occupations",
                                                                     "Supervisors, Administrative support", "Computer equipment operators", "Secretaries, stenographers, and typists", "Financial records processing", "Mail and message distribution",
                                                                     "Other Admin. support, including Clerical", "Private household service occupations", "Protective service", "Food service", "Health service",
                                                                     "Cleaning and building service", "Personal service", "Mechanics and repairers", "Construction trades", "Other precision production, craft, and repair",
                                                                     "Machine operator, and tenders, except precision", "Fabricators, assemblers, inspectors, samplers", "Motor vehicle operators", "Other transportation and material moving occupations", "Construction laborers",
                                                                     "Freight, Stock, & Materials Handlers", "Other handlers, Equipment cleaners, Helpers, Laborers", "Farm Operators and Managers", "Farm workers and related occupations", "Forestry and Fishing occupations",
                                                                     "Armed Forces"))
  AA$premp = factor(AA$premp, levels = c(-1, 1), labels = c(NA, "Employed persons (Excluding farm & private households)"))
  AA$prmjind1 = factor(AA$prmjind1, levels = c(-1, 1:23), labels = c(NA, "Agriculture", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable Goods",
                                                                     "Transportation", "Communications", "Utilities and Sanitary services", "Wholesale trade", "Retail trade",
                                                                     "Finance, Insurance, and Real Estate", "Private Households", "Business, auto and repair services", "Personal services, excluding private households", "Entertainment and recreation services",
                                                                     "Hospitals", "Medical services, excluding Hospitals", "Educational services", "Social services", "Other professional services",
                                                                     "Forestry and Fisheries", "Public Administration", "Armed Forces"))
  AA$prmjind2 = factor(AA$prmjind2, levels = c(-1, 1:23), labels = c(NA, "Agriculture", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable Goods",
                                                                     "Transportation", "Communications", "Utilities and Sanitary services", "Wholesale trade", "Retail trade",
                                                                     "Finance, Insurance, and Real Estate", "Private Households", "Business, auto and repair services", "Personal services, excluding private households", "Entertainment and recreation services",
                                                                     "Hospitals", "Medical services, excluding Hospitals", "Educational services", "Social services", "Other professional services",
                                                                     "Forestry and Fisheries", "Public Administration", "Armed Forces"))
  
  
  AA$prmjocc1 = factor(AA$prmjocc1, levels = c(-1,1:14), labels = c(NA, "Executive, Administrative, & Managerial occupations", "Professional specialty occupations", "Technicians and related support occupations", "Sales occupations", "Administrative support occupations, including Clerical",
                                                                    "Private Household occupations", "Protective service occupations", "Service occupations, except Protective & HHLD", "Precision production, Craft & Repair occupations", "Machine operators, Assemblers, & Inspectors",
                                                                    "Transportation and Material moving occupations", "Handlers, Equipment cleaners, Helpers, Laborers", "Farming, Forestry, and Fishing occupations", "Armed Forces"))
  AA$prmjocc2 = factor(AA$prmjocc2, levels = c(-1,1:14), labels = c(NA, "Executive, Administrative, & Managerial occupations", "Professional specialty occupations", "Technicians and related support occupations", "Sales occupations", "Administrative support occupations, including Clerical",
                                                                    "Private Household occupations", "Protective service occupations", "Service occupations, except Protective & HHLD", "Precision production, Craft & Repair occupations", "Machine operators, Assemblers, & Inspectors",
                                                                    "Transportation and Material moving occupations", "Handlers, Equipment cleaners, Helpers, Laborers", "Farming, Forestry, and Fishing occupations", "Armed Forces"))
  AA$prmjocgr = factor(AA$prmjocgr, levels = c(-1, 1:4), labels = c(NA, "Managerial & Professional, Technical, Sales & Support occupations", "Service occupations", "Production, Craft, Repair, Operators", "Farming, Forestry, and Fishing occupations"))
  AA$prnagpws = factor(AA$prnagpws, levels = c(-1, 1), labels = c(NA, "Non-Ag Private Wage & Salary workers (excluding private HH)"))
  AA$prnagws = factor(AA$prnagws, levels = c(-1, 1), labels = c(NA, "Non-Ag Wage & Salary workers"))
  
  
  AA$prsjmj = factor(AA$prsjmj, levels = c(-1, 1, 2), labels = c(NA, "Single jobholder", "Multiple jobholder"))
  AA$prerelg = factor(AA$prerelg, levels = -1:1, labels = c(NA, "Not eligible for edit", "Eligible for edit"))
  AA$peernuot = factor(AA$peernuot, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$peernper = factor(AA$peernper, levels = c(-1, 1:7), labels = c(NA, "Hourly", "Weekly", "Bi-weekly", "Twice monthly", "Monthly", "Annually", "Other - Specify"))
  AA$peernrt = factor(AA$peernrt, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$peernhry = factor(AA$peernhry, levels = c(-1, 1, 2), labels = c(NA, "Hourly workers", "Nonhourly workers"))
  AA$puernh1c[AA$puernh1c == -1] = NA
  AA$peernh2[AA$peernh2 == -1] = NA
  AA$peernh1o[AA$peernh1o == -1] = NA
  AA$prernhly[AA$prernhly == -1] = NA
  
  
  AA$pthr = factor(AA$pthr, levels = -1:1, labels = c(NA, "Not topcoded", "Topcoded"))
  AA$peernhro[AA$peernhro == -1] = NA
  AA$prernwa[AA$prernwa == -1] = NA
  AA$ptwk = factor(AA$ptwk, levels = -1:1, labels = c(NA, "Not topcoded", "Topcoded"))
  AA$peern[AA$peern == -1] = NA
  
  
  AA$puern2[AA$puern2 == -1] = NA
  AA$ptot = factor(AA$ptot, levels = -1:1, labels = c(NA, "Not topcoded", "Topcoded"))
  AA$peernwkp[AA$peernwkp == -1] = NA
  AA$peernlab = factor(AA$peernlab, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$peerncov = factor(AA$peerncov, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  
  
  AA$penlfjh = factor(AA$penlfjh, levels = c(-1, 1:3), labels = c(NA, "Within the last 12 months", "More than 12 months ago", "Never worked"))
  AA$penlfret = factor(AA$penlfret, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$penlfact = factor(AA$penlfact, levels = c(-1, 1:6), labels = c(NA, "Disabled", "Ill", "In School", "Taking care of house or family", "In retirement", "Something else/other"))
  AA$peschenr = factor(AA$peschenr, levels = c(-1, 1, 2), labels = c(NA, "Yes", "No"))
  AA$peschft = factor(AA$peschft, levels = c(-1, 1, 2), labels = c(NA, "Full-Time", "Part-Time"))
  
  
  AA$peschlvl = factor(AA$peschlvl, levels = c(-1, 1, 2), labels = c(NA, "High school", "College or University"))
  AA$prnlfsch = factor(AA$prnlfsch, levels = c(-1, 1, 2), labels = c(NA, "In school", "Not in school"))
  AA$pwfmwgt[AA$pwfmwgt == -1] = NA
  AA$pwlgwgt[AA$pwlgwgt == -1] = NA
  AA$pworwgt[AA$pworwgt == -1] = NA
  
  
  AA$pwsswgt[AA$pwsswgt == -1] = NA
  AA$pwvetwgt[AA$pwvetwgt == -1] = NA
  
  
  # These functions format the Allocation Flags section
  AA$prwernal = factor(AA$prwernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$prhernal = factor(AA$prhernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtenure = factor(AA$hxtenure, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelhhd = factor(AA$hxtelhhd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelavl = factor(AA$hxtelavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$hxphoneo = factor(AA$hxphoneo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxinusyr = factor(AA$pxinusyr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrrp = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxparent = factor(AA$pxparent, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxage = factor(AA$pxage, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmaritl = factor(AA$pxmaritl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxspouse = factor(AA$pxspouse, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxsex = factor(AA$pxsex, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafwhen = factor(AA$pxafwhen, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafnow = factor(AA$pxafnow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxeduca = factor(AA$pxeduca, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrace = factor(AA$pxrace, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnatvty = factor(AA$pxnatvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmntvty = factor(AA$pxmntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxfntvty = factor(AA$pxfntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxorigin = factor(AA$pxorigin, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmlr = factor(AA$pxmlr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxret1 = factor(AA$pxret1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabsrsn = factor(AA$pxabsrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabspdo = factor(AA$pxabspdo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmjot = factor(AA$pxmjot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmjnum = factor(AA$pxmjnum, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl1 = factor(AA$pxhrusl1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl2 = factor(AA$pxhrusl2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrftpt = factor(AA$pxhrftpt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhruslt = factor(AA$pxhruslt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrwant = factor(AA$pxhrwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn1 = factor(AA$pxhrrsn1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn2 = factor(AA$pxhrrsn2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhract1 = factor(AA$pxhract1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhract2 = factor(AA$pxhract2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhractt = factor(AA$pxhractt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn3 = factor(AA$pxhrrsn3, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhravl = factor(AA$pxhravl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayavl = factor(AA$pxlayavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlaylk = factor(AA$pxlaylk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlaydur = factor(AA$pxlaydur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayfto = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkm1 = factor(AA$pxlkm1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkavl = factor(AA$pxlkavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlkll1o = factor(AA$pxlkll1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkll2o = factor(AA$pxlkll2o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlklwo = factor(AA$pxlklwo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkdur = factor(AA$pxlkdur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkfto = factor(AA$pxlkfto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwwnto = factor(AA$pxdwwnto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwrsn = factor(AA$pxdwrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwlko = factor(AA$pxdwlko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwwk = factor(AA$pxdwwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdw4wk = factor(AA$pxdw4wk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwlkwk = factor(AA$pxdwlkwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavl = factor(AA$pxdwavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavr = factor(AA$pxdwavr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhwko = factor(AA$pxjhwko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhrsn = factor(AA$pxjhrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxjhwant = factor(AA$pxjhwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1cow = factor(AA$pxio1cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1icd = factor(AA$pxio1icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1ocd = factor(AA$pxio1ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2cow = factor(AA$pxio2cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxio2icd = factor(AA$pxio2icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2ocd = factor(AA$pxio2ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernuot = factor(AA$pxernuot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernper = factor(AA$pxernper, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernh1o = factor(AA$pxernh1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernhro = factor(AA$pxernhro, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxern = factor(AA$pxern, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernwkp = factor(AA$pxernwkp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernrt = factor(AA$pxernrt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernhry = factor(AA$pxernhry, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernh2 = factor(AA$pxernh2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernlab = factor(AA$pxernlab, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxerncov = factor(AA$pxerncov, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfjh = factor(AA$pxnlfjh, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfret = factor(AA$pxnlfret, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxnlfact = factor(AA$pxnlfact, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschenr = factor(AA$pxschenr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschft = factor(AA$pxschft, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschlvl = factor(AA$pxschlvl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  return(AA)
}



ParserJanuary1998 = function(DataIn, DataDictionaryIn) {
  # This eliminates all of the variables in the dataset that are labelled "Remove" in the Dictionary Files.
  AA = select(DataIn, -all_of(filter(DataDictionaryIn, Adjustment == "Remove")$ColName))
  
  # These format properly the three variables that go into forming the HRHHID2 variable.
  AA$hrsample[AA$hrsample == "-1"] = NA
  AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
  AA$hrsersuf[is.na(AA$hrsersuf)] = -1
  AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")
  # This adds the variables that the Dictionary says to add, properly applying the formulas to create them.
  AA = tibble(AA, hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum), gecmsanum = AA$gecmsa, gemsanum = AA$gemsa, geconum = AA$geco)
  # This removes all of the variables that are labelled "Delete" in the dataset. 
  AA = select(AA, -all_of(filter(DataDictionaryIn, Adjustment == "Delete")$ColName))
  # This reorders all of the remaining variables to match the Dictionary Order
  AA = select(AA, filter(filter(DataDictionaryIn, Adjustment != "Remove"), Adjustment != "Delete")$ColName)
  
  
  # These functions format the Household Information section
  AA$hrhhid[AA$hrhhid == -1] = NA
  AA$hrmonth[AA$hrmonth == -1] = NA
  AA$hryear4[AA$hryear4 == -1] = NA
  AA$hurespli[AA$hurespli <= -1] = NA
  AA$hufinal = factor(AA$hufinal, 
                      levels = c(-3:-1, 0, 1, 2, 5, 24, 115, 200, 201, 202, 203, 204, 205, 210, 216, 217, 218, 219, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 240, 241, 242, 243, 244, 245, 246, 247, 248),
                      labels = c("Refused", "Don't Know", NA, "New interview - Not contacted", "Fully complete CATI interview", "Partially completed CATI interview", "Labor force complete, Supplement incomplete - CATI", "HH occupied entirely by Armed Forces members",
                                 "Partial interview with callback panned - CATI", "New interview - Contacted", "CAPI Complete", "Callback needed", "Sufficient partial - Precloseout",
                                 "Sufficient partial - At closeout", "Labor force complete, - Suppl. incomplete - CAPI", "CAPI complete reinterview", "No one home", "Temporarily absent",
                                 "Refused", "Other occupied - Specify", "Armed Forces occupied or under age 14", "Temp. occupied w/persons with URE", "Vacant regular",
                                 "Vacant - Storage of HHLD furniture", "Unfit, to be demolished", "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent or trailer site",
                                 "Permit granted - Construction not started", "Other - Specify", "Demolished", "House or trailer moved", "Outside segment",
                                 "Converted to perm. business or storage", "Merged", "Condemned", "Built after April 1, 1980", "Unused serial no./listing sheet line",
                                 "Other - Specify"))
  
  
  AA$hetenure = factor(AA$hetenure, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Owned or being bought by a HH member", "Rented for cash", "Occupied without payment of cash rent"))
  AA$hehousut = factor(AA$hehousut, levels = -3:12, labels = c("Refused", "Don't Know", NA, "Other unit", "House, apartment, flat", "HU in nontransient hotel, motel, etc.", "HU permanent in transient hotel, motel",
                                                               "HU in rooming house", "Mobile home or trailer w/no perm. room added", "Mobile home or trailer w/1 or more perm. rooms added", "HU not specified above", "Quarters not HU in rooming or brding HS",
                                                               "Unit not perm. in transient hotl, motl", "Uoccupied tent site or trlr site", "Student quarters in college dorm", "Other unit not specified above"))
  AA$hetelhhd = factor(AA$hetelhhd, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Telephone present in house", "Telephone not present in the house"))
  AA$hetelavl = factor(AA$hetelavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Telephone elsewhere available for household to use", "Telephone not available eslewhere for household to use"))
  AA$hephoneo = factor(AA$hephoneo, levels = c(-3:2), labels = c("Refused", "Don't Know", NA, "Unknown", "Telephone interview acceptable", "Telephone interview not acceptable"))
  
  
  AA$hufaminc = factor(AA$hufaminc, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "Less than $5,000", "5,000 to 7,499", "7,500 to 9,999", "10,000 to 12,499", 
                                                                        "12,500 to 14,999", "15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", 
                                                                        "35,000 to 39,999", "40,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 or more"))
  AA$hutypea = factor(AA$hutypea, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "No one home (NOH)", "Temporarily absent (TA)", "Refused (Ref)", "Other occupied - Specify"))
  AA$hutypb = factor(AA$hutypb, levels = c(-3:-1, 1:9), labels = c("Refused", "Don't Know", NA, "Vacant regular", "Temporarily occupied by persons w/ URE", "Vacant-storage of HHLD furniture", "Unfit or to be demolished", 
                                                                   "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent site or trailer site", "Permit granted construction not started", "Other type B - Specify"))
  AA$hutypc = factor(AA$hutypc, levels = c(-3:-1, 1:6,8:9), labels = c("Refused", "Don't Know", NA, "Demolished", "House or trailer moved", "Outside segment", "Converted to perm. business or storage",
                                                                       "Merged", "Condemned", "Unused line of listing sheet", "Other - Specify"))
  AA$hwhhwgt[AA$hwhhwgt == -1] = NA
  
  
  AA$hrintsta = factor(AA$hrintsta, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Interview", "Type A Non-interview", "Type B Non-interview", "Type C Non-interview"))
  AA$hrnumhou[AA$hrnumhou == -1] = NA
  AA$hrhtype = factor(AA$hrhtype, levels = c(-3:10), labels = c("Refused", "Don't Know", NA, "Non-interview household", "Husband/Wife primary family (Neither AF)", "Husb/wife prim. family (Either/both AF)", "Unmarried civilian male-Prim. fam HHLDer",
                                                                "Unmarried civ. female-Prim. fam HHLDer", "Primary family HHLDer-RP in AF, Unmar.", "Civilian male primary individual", "Civilian female primary individual", "Primary individual HHLD-RP in AF",
                                                                "Group quarter with family", "Group quarters without family"))
  AA$hrmis[AA$hrmis == -1] = NA
  AA$huinttyp = factor(AA$huinttyp, levels = -3:2, labels = c("Refused", "Don't Know", NA, "Noninterview/Indeterminate", "Personal", "Telephone"))
  
  
  AA$huprscnt[AA$huprscnt == -1] = NA
  AA$hrlonglk = factor(AA$hrlonglk, levels = c(-3:-1, 0, 2, 3), labels = c("Refused", "Don't Know", NA, "MIS 1 or replaement HH (No link)", "MIS 2-4 or MIS 6-8", "MIS 5"))
  AA$hubus = factor(AA$hubus, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Someone in HH has a business/farm", "No one in HH has a business/farm"))
  AA$hubusl1[AA$hubusl1 <= -1] = NA
  AA$hubusl2[AA$hubusl2 <= -1] = NA
  
  
  AA$hubusl3[AA$hubusl3 <= -1] = NA
  AA$hubusl4[AA$hubusl4 <= -1] = NA
  
  
  
  # These functions format the Geographic Information section
  AA$gereg = factor(AA$gereg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Northeast", "Midwest", "South", "West"))
  AA$gestcen = factor(AA$gestcen, levels = c(-3:-1, 11:16, 21:23, 31:35, 41:47, 51:59, 61:64, 71:74, 81:88, 91:95), labels = c("Refused", "Don't Know", NA, "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX", "MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI"))
  AA$gestfips[AA$gestfips == -1] = NA
  AA$gecmsa = factor(AA$gecmsa, levels = c(-3:-1, 0, 7:97), labels = c("Refused", "Don't Know", NA, "Not identified or Nonmetropolitan", str_c(7:97, " Specific CMSA Code")))
  AA$gemsa = factor(AA$gemsa, levels = c(-3:-1, 0, 80:9360), labels = c("Refused", "Don't Know", NA, "Not identified or nonmetropolitan", str_c(80:9360, " Specific MSA Code")))
  
  AA$geco = factor(AA$geco, levels = -3:810, labels = c("Refused", "Don't Know", NA, "Not identified", str_c(1:810, " State-specific County Code")))
  AA$gecmsanum[AA$gecmsanum <= -1] = NA
  AA$gemsanum[AA$gemsanum <= -1] = NA
  AA$geconum[AA$geconum <= -1] = NA
  
  
  AA$gemsast = factor(AA$gemsast, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Central City", "Balance", "Nonmetropolitan", "Not identified"))
  AA$gemetsta = factor(AA$gemetsta, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Metropolitan", "Nonmetropolitan", "Not identified"))
  AA$geindvcc = factor(AA$geindvcc, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not identified, Nonmetropolitan, or Not a central city", str_c(1:4, " Specific central city code")))
  AA$gemsasz = factor(AA$gemsasz, levels = c(-3:-1, 0, 2:7), labels = c("Refused", "Don't Know", NA, "Not identified or Nonmetropolitan", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  
  
  
  # These functions format the Personal Information Demographic section
  AA$perrp = factor(AA$perrp, levels = c(-3:-1, 1:18), labels = c("Refused", "Don't Know", NA, "Reference person w/Rels.", "Reference person w/o Rels.", "Spouse", "Child", "Grandchild", "Parent", "Brother/Sister", "Other Rel. of Reference person", "Foster child", "Nonrel. of Ref. person w/Rels.", "Not used", "Nonrel. of Ref. person w/o Rels.", "Unmarried partner w/Rels.", "Unmarried partner w/out Rels.", "Housemate/Roommate w/Rels.", "Housemate/Roommate w/out Rels.", "Roomer/Boarder w/ Rels.", "Roomer/Boarder w/out Rels."))
  AA$peparent = factor(AA$peparent, levels = c(-3:-1, 1:99), labels = c("Refused", "Don't Know", "No parent", str_c(1:99, " Line Num of parent")))
  AA$prtage = factor(AA$prtage, levels = -3:90, labels = c("Refused", "Don't Know", NA, str_c(0:89, " years old"), "90+ years old"))
  AA$prtfage = factor(AA$prtfage, levels = -3:1, labels = c("Refused", "Don't Know", NA, "No top code", "Top coded value for age"))
  AA$pemaritl = factor(AA$pemaritl, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Married - Spouse present", "Married - Spouse absent", "Widowed", "Divorced", "Separated", "Never married"))
  
  
  AA$pespouse[AA$pespouse == -1] = NA
  AA$pesex = factor(AA$pesex, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Male", "Female"))
  AA$puafever = factor(AA$puafever, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Served in the Armed Forces at some point", "Never served in the Armed Forces"))
  AA$peafwhen = factor(AA$peafwhen, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Vietnam Era (8/64-4/75)", "Korean War (6/50-1/55)", "World War II (9/40-7/47)", "World War I (4/17-11/18)", "Other service (All other persiods)", "Nonveteran"))
  AA$peafnow = factor(AA$peafnow, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Currently in Armed Forces", "Not currently in Armed Forces"))
  
  
  AA$peeduca = factor(AA$peeduca, levels = c(-3:-1, 31:46), labels = c("Refused", "Don't Know", NA, "Less than 1st grade", "1st, 2nd, 3rd or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade no diploma", "High school grad-diploma or equivalent (GED)", "Some college but no degree", "Associate degree-Occupational/Vocational", "Associate degree-Academic program", "Bachelor's degree (Ex: BA, AB, BS)", "Master's degree (Ex: MA, MS, MEng, MEd, MSW)", "Professional School Deg (Ex: MD, DDS, DVM)", "Doctorate degree (Ex: PhD, EdD)"))
  AA$perace = factor(AA$perace, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "White", "Black", "American Indian, Aleut, Eskimo", "Asian or Pacific Islander"))
  AA$prorigin = factor(AA$prorigin,  levels = c(-3:-1, 1:10), labels = c("Refused", "Don't Know", NA, "Mexican American", "Chicano", "Mexican (Mexicano)", "Puerto Rican", "Cuban", "Central or South American", "Other Spanish", "All other", "Don't know", "NA"))
  AA$puchinhh = factor(AA$puchinhh, levels = c(-3:-1, 1:7, 9), labels = c("Refused", "Don't Know", NA, "Person added", "Person added - URE", "Person undeleted", "Person died", "Deleted for reason other than death", "Person joined Armed Forces", "Person no longer in AF", "Change in demographic information"))
  AA$pulineno[AA$pulineno == -1] = NA
  
  
  AA$prfamnum = factor(AA$prfamnum, levels = -3:19, labels = c("Refused", "Don't Know", NA, "Not a family member", "Primary family member only", "Subfamily No. 2 member", "Subfamily No. 3 member", "Subfamily No. 4 member", "Subfamily No. 5 member", "Subfamily No. 6 member", "Subfamily No. 7 member", "Subfamily No. 8 member", "Subfamily No. 9 member", "Subfamily No. 10 member", "Subfamily No. 11 member", "Subfamily No. 12 member", "Subfamily No. 13 member", "Subfamily No. 14 member", "Subfamily No. 15 member", "Subfamily No. 16 member", "Subfamily No. 17 member", "Subfamily No. 18 member", "Subfamily No. 19 member"))
  AA$prfamrel = factor(AA$prfamrel, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not a family member", "Reference person", "Spouse", "Child", "Other relative (Primary Family & Unrel)"))
  AA$prfamtyp = factor(AA$prfamtyp, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Primary family", "Primary individual", "Related subfamily", "Unrelated subfamily", "Secondary individual"))
  AA$prhspnon = factor(AA$prhspnon, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Hispanic", "Non-Hispanic"))
  AA$prmarsta = factor(AA$prmarsta, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Married, civilian spouse present", "Married, Armed Forces spouse present", "Married, Spouse absent (Exc. Separated)", "Widowed", "Divorced", "Separated", "Never married"))
  
  
  AA$prpertyp = factor(AA$prpertyp, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Child household member", "Adult civilian household member", "Adult armed forces household member"))
  AA$penatvty = factor(AA$penatvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person born in United States", "Person born in Puerto Rico", "Person born in U.S. Outlying Area", str_c(100:554, " Person born in Foreign Country or at sea - See Code list"), "Person born Abroad, country not known"))
  AA$pemntvty = factor(AA$pemntvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person's mother born in United States", "Person's mother born in Puerto Rico", "Person's mother born in U.S. Outlying Area", str_c(100:554, " Person's mother born in Foreign Country or at sea - See Code list"), "Person's mother born Abroad, country not known"))
  AA$pefntvty = factor(AA$pefntvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person's father born in United States", "Person's father born in Puerto Rico", "Person's father born in U.S. Outlying Area", str_c(100:554, " Person's father born in Foreign Country or at sea - See Code list"), "Person's father born Abroad, country not known"))
  AA$prcitshp = factor(AA$prcitshp, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Native, Born in the United States", "Native, Born in Puerto Rico or U.S. Outlying Area", "Native, Born abroad of American parent or parents", "Foreign born, U.S. citizen by naturalization", "Foreign born, not a citizen of the United States"))
  
  
  # Need to look more into the allocation flag issue for this following variable
  AA$prcitflg[AA$prcitflg == -1] = NA
  AA$prinusyr = factor(AA$prinusyr, levels = -3:14, labels = c("Refused", "Don't Know", "Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1997"))
  
  
  
  # These functions format the Personal Information Labor Force section
  AA$puslfprx = factor(AA$puslfprx, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Labor Force Info collected by self", "Labor Force info collected by proxy", "Labor Force info collected by both self and proxy"))
  AA$pemlr = factor(AA$pemlr, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Employed-At work", "Employed-Absent", "Unemployed-On layoff", "Unemployed-Looking", "Not in Labor Force-Retired", "Not in Labor Force-Disabled", "Not in labor force-Other"))
  AA$puwk = factor(AA$puwk, levels = c(-3:-1,1:5), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pubus1 = factor(AA$pubus1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Person did unpaid work on family business/farm", "Person did not do unpaid work on family business/farm"))
  AA$pubus2ot = factor(AA$pubus2ot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Person received payments or profits from the family business", "Person did not receive payments or profits from the family business"))
  
  
  AA$puretot = factor(AA$puretot, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported retired last month and is retired now", "No, person who was reported retired last month is not retired now", "Person was not retired last month"))
  AA$pudis = factor(AA$pudis, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported disabled last month and is disabled now", "No, person who was reported disabled last month is not disabled now", "Person was not disabled last month"))
  AA$peret1 = factor(AA$peret1, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported retired last month wants a job (full or part-time)", "No, person who was reported retired last month does not want a job", "Person who was reported retired last month has a job now"))
  AA$pudis1 = factor(AA$pudis1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, person's disability prevents person from working in the next 6 months", "No, person's disability does not prevent person from accepting work in the next 6 months"))
  AA$pudis2 = factor(AA$pudis2, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, person has a disability that prevents person from working", "No, person does not have a disability that prevents person from working"))
  
  
  AA$puabsot = factor(AA$puabsot, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, person had a full/part-time job last week", "No, person didn't have a full/part-time job last week", "Person was retired last week", "Person was disabled last week", "Person was unable to work last week"))
  AA$pulay = factor(AA$pulay, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, person is on layoff from a job", "No, person is not on layoff from a job", "Person is retired", "Person is disabled", "Person is unable to work"))
  AA$peabsrsn = factor(AA$peabsrsn, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "On layoff", "Slack work/Business conditions", "Waiting for a new job to begin", "Vacation/Personal days", "Own illness/Injury/Medical problems", 
                                                                        "Child care problems", "Other family/Personal obligation", "Maternity/Paternity leave", "Labor dispute", "Weather affected job", 
                                                                        "School/Training", "Civic/Military Duty", "Does not work in the business", "Other (Specify)"))
  
  
  AA$peabspdo = factor(AA$peabspdo, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pemjot = factor(AA$pemjot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pemjnum = factor(AA$pemjnum, levels = c(-3:-1, 2:4), labels = c("Refused", "Don't Know", NA, "2 jobs", "3 jobs", "4 or more jobs"))
  AA$pehrusl1[AA$pehrusl1 == -1] = NA
  AA$pehrusl2[AA$pehrusl2 == -1] = NA
  
  
  AA$pehrftpt = factor(AA$pehrftpt, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Hours vary"))
  AA$pehruslt[AA$pehruslt == -1] = NA
  AA$pehrwant = factor(AA$pehrwant, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Regular hours are Full-time"))
  AA$pehrrsn1 = factor(AA$pehrrsn1, levels = c(-3:-1, 1:10), labels = c("Refused", "Don't Know", NA, "Slack work/Business conditions", "Could only find Part-time work", "Seasonal work", "Child care problems", "Other family/Personal obligation", "Health/Medical limitations", "School/Training", "Retired/Social security limit on earnings", "Full-time workweek is less than 35 hrs", "Other - Specify"))
  AA$pehrrsn2 = factor(AA$pehrrsn2, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Child care problems", "Other family/Personal obligations", "Health/Medical limitations", "School/Training", "Retired/Social Security limit on earnings", "Full-Time workweek less than 35 hours", "Other - Specify"))
  
  
  AA$pehrrsn3 = factor(AA$pehrrsn3, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Slack work/Business conditions", "Seasonal work", "Job started or ended during week", "Vacation/Personal day", "Own illnes/Injury/Medical appointment", "Holiday (Legal or Religious)", "Child care problems", "Other family/Personal obligations", "Labor dispute", "Weather affected job", "School/training", "Civic/Military duty", "Other reason"))
  AA$puhroff1 = factor(AA$puhroff1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puhroff2[AA$puhroff2 <= -1] = NA
  AA$puhrot1 = factor(AA$puhrot1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puhrot2[AA$puhrot2 <= -1] = NA
  
  
  AA$pehract1[AA$pehract1 <= -1] = NA
  AA$pehract2[AA$pehract2 <= -1] = NA
  AA$pehractt[AA$pehractt <= -1] = NA
  AA$pehravl = factor(AA$pehravl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulaydt = factor(AA$pulaydt, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$pulay6m = factor(AA$pulay6m, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pelayavl = factor(AA$pelayavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulayavr = factor(AA$pulayavr, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Own temporary illness", "Going to school", "Other"))
  AA$pelaylk = factor(AA$pelaylk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pelaydur[AA$pelaydur <= -1] = NA
  
  
  AA$pelayfto = factor(AA$pelayfto, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulk = factor(AA$pulk, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pelkm1 = factor(AA$pelkm1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                    "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                    "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkm2 = factor(AA$pulkm2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm3 = factor(AA$pulkm3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkm4 = factor(AA$pulkm4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm5 = factor(AA$pulkm5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm6 = factor(AA$pulkm6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk1 = factor(AA$pulkdk1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                      "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                      "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkdk2 = factor(AA$pulkdk2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkdk3 = factor(AA$pulkdk3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk4 = factor(AA$pulkdk4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk5 = factor(AA$pulkdk5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk6 = factor(AA$pulkdk6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps1 = factor(AA$pulkps1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                      "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                      "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  
  
  AA$pulkps2 = factor(AA$pulkps2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps3 = factor(AA$pulkps3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps4 = factor(AA$pulkps4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps5 = factor(AA$pulkps5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps6 = factor(AA$pulkps6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pelkavl = factor(AA$pelkavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulkavr = factor(AA$pulkavr, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Waiting for new job to begin", "Own temporary illness", "Going to school", "Other - Specify"))
  AA$pelkll1o = factor(AA$pelkll1o, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Working", "School", "Left military service", "Something else"))
  AA$pelkll2o = factor(AA$pelkll2o, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Lost job", "Quit job", "Temporary job ended"))
  AA$pelklwo = factor(AA$pelklwo, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Last worked within the last 12 months", "Last worked more than 12 months ago", "Never worked previously"))
  
  
  AA$pelkdur[AA$pelkdur == -1] = NA
  AA$pelkfto = factor(AA$pelkfto, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Doesn't matter"))
  AA$pedwwnto = factor(AA$pedwwnto, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, or maybe, it depends", "No", "Retired", "Disabled", "Unable to work"))
  AA$pedwrsn = factor(AA$pedwrsn, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Believes no work available in area of expertise", "Couldn't find any work", "Lacks necessary schooling/training", "Employers think too young or too old", "Other types of discrimination",
                                                                      "Can't arrange child care", "Family responsibilities", "In school or other training", "Ill-health, physcial disability", "Transportation problems", "Other - Specify"))
  AA$pedwlko = factor(AA$pedwlko, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$pedwwk = factor(AA$pedwwk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedw4wk = factor(AA$pedw4wk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwlkwk = factor(AA$pedwlkwk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwavl = factor(AA$pedwavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwavr = factor(AA$pedwavr, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Own temporary illness", "Going to school", "Other"))
  
  
  AA$pejhwko = factor(AA$pejhwko, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pujhdp1o = factor(AA$pujhdp1o, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pejhrsn = factor(AA$pejhrsn, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Personal/Family (Including Pregnancy)", "Return to school", "Health", "Retirement or old age", "Temp, Seasonal or intermittent job complete", "Slack work/business conditions", "Unsatisfactory work arrangements (Hrs, pay, etc.)", "Other - specify"))
  AA$pejhwant = factor(AA$pejhwant, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, or it depends", "No"))
  AA$prabsrea = factor(AA$prabsrea, levels = c(-3:-1, 1:40), labels = c("Refused", "Don't Know",NA, "FT paid-Vacation", "FT paid-Own illness", "FT paid-Child care problems", "FT paid-Other family/Personal oblig.",
                                                                        "FT paid-Maternity/Paternity leave", "FT paid-Labor dispute", "FT paid-Weather affected job", "FT paid-School/Training", "FT paid-Civic/Military duty",
                                                                        "FT paid-Other", "FT unpaid-Vacation", "FT unpaid-Own illness", "FT unpaid-Child care problems", "FT unpaid-Other fam/Personal obligation",
                                                                        "FT unpaid-Maternity/Paternity leave", "FT unpaid-Labor dispute", "FT unpaid-Weather affected job", "FT unpaid-School/Training", "FT unpaid-Civic/Military duty",
                                                                        "FT unpaid-Other", "PT paid-Vacation", "PT paid-Own illness", "PT paid-Child care problems", "PT paid-Other family/Personal oblig.",
                                                                        "PT paid-Maternity/Paternity leave", "PT paid-Labor dispute", "PT paid-Weather affected job", "PT paid-School/Training", "PT paid-Civic/Military duty",
                                                                        "PT paid-Other", "PT unpaid-Vacation", "PT unpaid-Own illness", "PT unpaid-Child care problems", "PT unpaid-Other fam/Personal obligation",
                                                                        "PT unpaid-Maternity/Paternity leave", "PT unpaid-Labor dispute", "PT unpaid-Weather affected job", "PT unpaid-School/Training", "PT unpaid-Civic/Military duty",
                                                                        "PT unpaid-Other"))
  
  # Stopped here
  AA$prcivlf = factor(AA$prcivlf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "In Civilian Labor Force", "Not in Civilian Labor Force"))
  AA$prdisc = factor(AA$prdisc, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Discouraged worker", "Conditionally interested", "Not available"))
  AA$premphrs = factor(AA$premphrs, levels = -3:22, labels = c("Refused", "Don't Know", NA, "Unemployed and NILF", "W/job, Not at work-Illness", "W/job, not at work-Vacation", "W/job, not at work-Weather affected job",
                                                               "W/job, not at work-Labor dispute", "W/job, not at work-Child care problems", "W/job, not at work-Fam/Pers obligation", "W/job, not at work-Maternity/Paternity", "W/job, not at work-School/Training",
                                                               "W/job, not at work-Civic/Military duty", "W/job, not at work-Does not work in bus", "W/job, not at work-Other", "At work- 1-4 hrs", "At work- 5-14 hrs",
                                                               "At work- 15-21 hrs", "At work- 22-29 hrs", "At work- 30-34 hrs", "At work- 35-39 hrs", "At work- 40 hrs",
                                                               "At work- 41-47 hrs", "At work- 48 hrs", "At work- 49-59 hrs", "At work- 60 hrs or more"))
  AA$prempnot = factor(AA$prempnot, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Employed", "Unemployed", "Not in the Labor Force (NILF)-discouraged", "Not in the Labor Force (NILF)-other"))
  AA$prexplf = factor(AA$prexplf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Employed", "Unemployed"))
  
  
  AA$prftlf = factor(AA$prftlf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Full Time Labor Force", "Part Time Labor Force"))
  AA$prhrusl = factor(AA$prhrusl, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "0-20 hrs", "21-34 hrs", "35-39 hrs", "40 hrs", "41-49 hrs", "50 or more hrs", "Varies-Full Time", "Varies-Part Time"))
  AA$prjobsea = factor(AA$prjobsea, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Looked last 4 weeks - Not worked", "Looked last 4 weeks - Worked", "Looked last 4 weeks - Layoff", "Unavailable job seekers", "No recent job search"))
  AA$prpthrs = factor(AA$prpthrs, levels = -3:12, labels = c("Refused", "Don't Know", NA, "Usually FT, PT for Noneconomic reasons", "Usu.FT, PT econ reasons; 1-4 hrs", "Usu.FT, PT econ reasons; 5-14 hrs", "Usu.FT, PT econ reasons; 15-29 hrs",
                                                             "Usu.FT, PT econ reasons; 30-34 hrs", "Usu.PT, econ reasons; 1-4 hrs", "Usu.PT, econ reasons; 5-14 hrs", "Usu.PT, econ reasons 15-29 hrs", "Usu.PT, econ reasons 30-34 hrs",
                                                             "Usu.PT, non-econ reasons; 1-4 hrs", "Usu.PT, non-econ reasons; 5-14 hrs", "Usu.PT, non-econ reasons; 15-29 hrs", "Usu.PT, non-econ reasons; 30-34 hrs"))
  AA$prptrea = factor(AA$prptrea, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Usu. FT-Slack work/Business conditions", "Usu. FT-Seasonal work", "Usu. FT-Job started/ended during week", "Usu. FT-Vacation/Personal day",
                                                                      "Usu. FT-Own illness/Injury/Medical appointment", "Usu. FT-Holiday (Religious or Legal)", "Usu. FT-Child care problems", "Usu. FT-Other fam/Pers obligations", "Usu. FT-Labor dispute",
                                                                      "Usu. FT-Weather affected job", "Usu. FT-School/Training", "Usu. FT-Civic/Military Duty", "Usu. FT-Other reason", "Usu. PT-Slack work/Business conditions",
                                                                      "Usu. PT-Could only find PT work", "Usu. PT-Seasonal work", "Usu. PT-Child care problems", "Usu. PT-Other fam/Pers obligations", "Usu. PT-Health/Medical Limitations", 
                                                                      "Usu. PT-School/Training", "Usu. PT-Retired/S.S. limit on earnings", "Usu. PT-Workweek <35 hours", "Usu. PT-Other reason"))
  
  
  AA$prunedur[AA$prunedur == -1] = NA
  AA$pruntype = factor(AA$pruntype, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Job loser/On layoff", "Other job loser", "Temportary job ended", "Job leaver", "Re-entrant", "New-entrant"))
  AA$prwksch = factor(AA$prwksch, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not in Labor Force", "At work", "With job, not at work", "Unemployed, seeks FT", "Unemployed, seeks PT"))
  AA$prwkstat = factor(AA$prwkstat, levels = c(-3:-1, 1:12), labels = c("Refused", "Don't Know", NA, "Not in Labor Force", "FT hours (35+), usually FT", "PT for economic reasons, usually FT", "PT for non-economic reasons, usually FT",
                                                                        "Not at work, usually FT", "PT hrs, usually PT for economic reasons", "PT hrs, usually PT for non-economic reasons", "FT hours, usually PT for economic reasons", "FT hours, usually PT for non-economic reasons", 
                                                                        "Not at work, usually Part-Time", "Unemployed FT", "Unemployed PT"))
  AA$prwntjob = factor(AA$prwntjob, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Want a job", "Other Not in the Labor Force (NILF)"))
  
  
  AA$puiodp1 = factor(AA$puiodp1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puiodp2 = factor(AA$puiodp2, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puiodp3 = factor(AA$puiodp3, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peio1cow = factor(AA$peio1cow, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay"))
  AA$puio1mfg = factor(AA$puio1mfg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Manufacturing", "Retail trade", "Wholesale trade", "Something else"))
  
  
  AA$peio1icd[AA$peio1icd == -1] = NA
  AA$peio1ocd[AA$peio1ocd == -1] = NA
  AA$peio2cow = factor(AA$peio2cow, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay"))
  AA$puio2mfg = factor(AA$puio2mfg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Manufacturing", "Retail Trade", "Wholesale Trade", "Something else"))
  AA$peio2icd[AA$peio2icd == -1] = NA 
  
  
  AA$peio2ocd[AA$peio2ocd == -1] = NA
  AA$prioelg = factor(AA$prioelg, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not eligible for edit", "Eligible for edit"))
  AA$pragna = factor(AA$pragna, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Agricultural", "Non-agricultural"))
  AA$prcow1 = factor(AA$prcow1, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  AA$prcow2 = factor(AA$prcow2, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  
  
  AA$prcowpg = factor(AA$prcowpg, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Private", "Government"))
  AA$prdtcow1 = factor(AA$prdtcow1, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtcow2 = factor(AA$prdtcow2, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtind1 = factor(AA$prdtind1, levels = c(-3:-1, 1:52), labels = c("Refused", "Don't Know", NA, "Goods producing-Agricultural services", "Goods producing-Other Agricultural", "Mining", "Construction", "Mfg-Lumber & Wood prods, excluding Furniture",
                                                                        "Mfg-Furniture & Fixtures", "Mfg-Stone, clay, concrete, Glass prods", "Mfg-Primary metals", "Mfg-Fabricated metals", "Mfg-Not specified metal industries",
                                                                        "Mfg-Machinery, excluding electrical", "Mfg-Electrical machinery, equipment supplies", "Mfg-Motor vehicles & equipment", "Mfg-aircraft & parts", "Mfg-Other transportation equipment",
                                                                        "Mfg-Professional & photo equipment, watches", "Mfg-Toys, amusement, & sporting goods", "Mfg-Miscellanious & NEC Mfg industries", "Mfg-Food & kindred products", "Mfg-Tobacco products",
                                                                        "Mfg-Textile mill products", "Mfg-Apparel & other finished textile PR", "Mfg-Paper & allied products", "Mfg-Printing, publishing & allied industries", "Mfg-Chemicals & allied products",
                                                                        "Mfg-Petroleum & coal products", "Mfg-Rubber & Miscellanious plastic products", "Mfg-Leather & leather products", "Transportation", "Communications",
                                                                        "Utilities & Sanitary services", "Wholesale trade", "Eating and drinking places", "Other retail trade", "Banking and other finance",
                                                                        "Insurance and Real estate", "Private household services", "Business services", "Automobile and repair services", "Personal services excluding private households",
                                                                        "Entertainment & recreation services", "Hospitals", "Health services, excluding hospitals", "Educational services", "Social services",
                                                                        "Other professional services", "Forestry & fisheries", "Justice, public order, & safety", "Admin of human resource programs", "National Security & Internal Affairs",
                                                                        "Other Public Administration", "Armed Forces"))
  AA$prdtind2 = factor(AA$prdtind2, levels = c(-3:-1, 1:52), labels = c("Refused", "Don't Know", NA, "Goods producing-Agricultural services", "Goods producing-Other Agricultural", "Mining", "Construction", "Mfg-Lumber & Wood prods, excluding Furniture",
                                                                        "Mfg-Furniture & Fixtures", "Mfg-Stone, clay, concrete, Glass prods", "Mfg-Primary metals", "Mfg-Fabricated metals", "Mfg-Not specified metal industries",
                                                                        "Mfg-Machinery, excluding electrical", "Mfg-Electrical machinery, equipment supplies", "Mfg-Motor vehicles & equipment", "Mfg-aircraft & parts", "Mfg-Other transportation equipment",
                                                                        "Mfg-Professional & photo equipment, watches", "Mfg-Toys, amusement, & sporting goods", "Mfg-Miscellanious & NEC Mfg industries", "Mfg-Food & kindred products", "Mfg-Tobacco products",
                                                                        "Mfg-Textile mill products", "Mfg-Apparel & other finished textile PR", "Mfg-Paper & allied products", "Mfg-Printing, publishing & allied industries", "Mfg-Chemicals & allied products",
                                                                        "Mfg-Petroleum & coal products", "Mfg-Rubber & Miscellanious plastic products", "Mfg-Leather & leather products", "Transportation", "Communications",
                                                                        "Utilities & Sanitary services", "Wholesale trade", "Eating and drinking places", "Other retail trade", "Banking and other finance",
                                                                        "Insurance and Real estate", "Private household services", "Business services", "Automobile and repair services", "Personal services excluding private households",
                                                                        "Entertainment & recreation services", "Hospitals", "Health services, excluding hospitals", "Educational services", "Social services",
                                                                        "Other professional services", "Forestry & fisheries", "Justice, public order, & safety", "Admin of human resource programs", "National Security & Internal Affairs",
                                                                        "Other Public Administration", "Armed Forces"))
  
  
  AA$prdtocc1 = factor(AA$prdtocc1, levels = c(-3:-1, 1:46), labels = c("Refused", "Don't Know", NA, "Officials & Administrators, Public Admin.", "Other executive, Admin. & Managerial", "Management related occupations", "Engineers", "Mathematical and Computer Scientists",
                                                                        "Natural Scientists", "Health diagnosing occupations", "Health assessment and treatment occupations", "Teachers, College and University", "Teachers, except College and University",
                                                                        "Lawyers and judges", "Other professional specialty occupations", "Health technologists and technicians", "Engineering and Science technicians", "Technicians, except Health, Engineering, and Science",
                                                                        "Supervisors and proprietors, sales occupations", "Sales reps, finance and business services", "Sales reps, Commodities, except Retail", "Sales workers, Retail & personal services", "Sales related occupations",
                                                                        "Supervisors, Administrative support", "Computer equipment operators", "Secretaries, stenographers, and typists", "Financial records processing", "Mail and message distribution",
                                                                        "Other Admin. support, including Clerical", "Private household service occupations", "Protective service", "Food service", "Health service",
                                                                        "Cleaning and building service", "Personal service", "Mechanics and repairers", "Construction trades", "Other precision production, craft, and repair",
                                                                        "Machine operator, and tenders, except precision", "Fabricators, assemblers, inspectors, samplers", "Motor vehicle operators", "Other transportation and material moving occupations", "Construction laborers",
                                                                        "Freight, Stock, & Materials Handlers", "Other handlers, Equipment cleaners, Helpers, Laborers", "Farm Operators and Managers", "Farm workers and related occupations", "Forestry and Fishing occupations",
                                                                        "Armed Forces"))
  AA$prdtocc2 = factor(AA$prdtocc2, levels = c(-3:-1, 1:46), labels = c("Refused", "Don't Know", NA, "Officials & Administrators, Public Admin.", "Other executive, Admin. & Managerial", "Management related occupations", "Engineers", "Mathematical and Computer Scientists",
                                                                        "Natural Scientists", "Health diagnosing occupations", "Health assessment and treatment occupations", "Teachers, College and University", "Teachers, except College and University",
                                                                        "Lawyers and judges", "Other professional specialty occupations", "Health technologists and technicians", "Engineering and Science technicians", "Technicians, except Health, Engineering, and Science",
                                                                        "Supervisors and proprietors, sales occupations", "Sales reps, finance and business services", "Sales reps, Commodities, except Retail", "Sales workers, Retail & personal services", "Sales related occupations",
                                                                        "Supervisors, Administrative support", "Computer equipment operators", "Secretaries, stenographers, and typists", "Financial records processing", "Mail and message distribution",
                                                                        "Other Admin. support, including Clerical", "Private household service occupations", "Protective service", "Food service", "Health service",
                                                                        "Cleaning and building service", "Personal service", "Mechanics and repairers", "Construction trades", "Other precision production, craft, and repair",
                                                                        "Machine operator, and tenders, except precision", "Fabricators, assemblers, inspectors, samplers", "Motor vehicle operators", "Other transportation and material moving occupations", "Construction laborers",
                                                                        "Freight, Stock, & Materials Handlers", "Other handlers, Equipment cleaners, Helpers, Laborers", "Farm Operators and Managers", "Farm workers and related occupations", "Forestry and Fishing occupations",
                                                                        "Armed Forces"))
  AA$premp = factor(AA$premp, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Employed persons (Excluding farm & private households)"))
  AA$prmjind1 = factor(AA$prmjind1, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Agriculture", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable Goods",
                                                                        "Transportation", "Communications", "Utilities and Sanitary services", "Wholesale trade", "Retail trade",
                                                                        "Finance, Insurance, and Real Estate", "Private Households", "Business, auto and repair services", "Personal services, excluding private households", "Entertainment and recreation services",
                                                                        "Hospitals", "Medical services, excluding Hospitals", "Educational services", "Social services", "Other professional services",
                                                                        "Forestry and Fisheries", "Public Administration", "Armed Forces"))
  AA$prmjind2 = factor(AA$prmjind2, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Agriculture", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable Goods",
                                                                        "Transportation", "Communications", "Utilities and Sanitary services", "Wholesale trade", "Retail trade",
                                                                        "Finance, Insurance, and Real Estate", "Private Households", "Business, auto and repair services", "Personal services, excluding private households", "Entertainment and recreation services",
                                                                        "Hospitals", "Medical services, excluding Hospitals", "Educational services", "Social services", "Other professional services",
                                                                        "Forestry and Fisheries", "Public Administration", "Armed Forces"))
  
  
  AA$prmjocc1 = factor(AA$prmjocc1, levels = c(-3:-1,1:14), labels = c("Refused", "Don't Know", NA, "Executive, Administrative, & Managerial occupations", "Professional specialty occupations", "Technicians and related support occupations", "Sales occupations", "Administrative support occupations, including Clerical",
                                                                       "Private Household occupations", "Protective service occupations", "Service occupations, except Protective & HHLD", "Precision production, Craft & Repair occupations", "Machine operators, Assemblers, & Inspectors",
                                                                       "Transportation and Material moving occupations", "Handlers, Equipment cleaners, Helpers, Laborers", "Farming, Forestry, and Fishing occupations", "Armed Forces"))
  AA$prmjocc2 = factor(AA$prmjocc2, levels = c(-3:-1,1:14), labels = c("Refused", "Don't Know", NA, "Executive, Administrative, & Managerial occupations", "Professional specialty occupations", "Technicians and related support occupations", "Sales occupations", "Administrative support occupations, including Clerical",
                                                                       "Private Household occupations", "Protective service occupations", "Service occupations, except Protective & HHLD", "Precision production, Craft & Repair occupations", "Machine operators, Assemblers, & Inspectors",
                                                                       "Transportation and Material moving occupations", "Handlers, Equipment cleaners, Helpers, Laborers", "Farming, Forestry, and Fishing occupations", "Armed Forces"))
  AA$prmjocgr = factor(AA$prmjocgr, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Managerial & Professional, Technical, Sales & Support occupations", "Service occupations", "Production, Craft, Repair, Operators", "Farming, Forestry, and Fishing occupations"))
  AA$prnagpws = factor(AA$prnagpws, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Non-Ag Private Wage & Salary workers (excluding private HH)"))
  AA$prnagws = factor(AA$prnagws, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Non-Ag Wage & Salary workers"))
  
  
  AA$prsjmj = factor(AA$prsjmj, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Single jobholder", "Multiple jobholder"))
  AA$prerelg = factor(AA$prerelg, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not eligible for edit", "Eligible for edit"))
  AA$peernuot = factor(AA$peernuot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peernper = factor(AA$peernper, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Hourly", "Weekly", "Bi-weekly", "Twice monthly", "Monthly", "Annually", "Other - Specify"))
  AA$peernrt = factor(AA$peernrt, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$peernhry = factor(AA$peernhry, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Hourly workers", "Nonhourly workers"))
  AA$puernh1c[AA$puernh1c <= -1] = NA
  AA$peernh2[AA$peernh2 <= -1] = NA
  AA$peernh1o[AA$peernh1o <= -1] = NA
  AA$prernhly[AA$prernhly <= -1] = NA
  
  
  AA$pthr = factor(AA$pthr, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  AA$peernhro[AA$peernhro <= -1] = NA
  AA$prernwa[AA$prernwa <= -1] = NA
  AA$ptwk = factor(AA$ptwk, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  AA$peern[AA$peern <= -1] = NA
  
  
  AA$puern2[AA$puern2 <= -1] = NA
  AA$ptot = factor(AA$ptot, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  AA$peernwkp[AA$peernwkp <= -1] = NA
  AA$peernlab = factor(AA$peernlab, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peerncov = factor(AA$peerncov, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$penlfjh = factor(AA$penlfjh, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Within the last 12 months", "More than 12 months ago", "Never worked"))
  AA$penlfret = factor(AA$penlfret, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$penlfact = factor(AA$penlfact, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Disabled", "Ill", "In School", "Taking care of house or family", "In retirement", "Something else/other"))
  AA$peschenr = factor(AA$peschenr, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peschft = factor(AA$peschft, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Full-Time", "Part-Time"))
  
  
  AA$peschlvl = factor(AA$peschlvl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "High school", "College or University"))
  AA$prnlfsch = factor(AA$prnlfsch, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "In school", "Not in school"))
  AA$pwfmwgt[AA$pwfmwgt <= -1] = NA
  AA$pwlgwgt[AA$pwlgwgt <= -1] = NA
  AA$pworwgt[AA$pworwgt <= -1] = NA
  
  
  AA$pwsswgt[AA$pwsswgt <= -1] = NA
  AA$pwvetwgt[AA$pwvetwgt <= -1] = NA
  
  
  if ((AA$hryear4[1] == 1998) | ((AA$hryear4[1] == 1999)&(AA$hrmonth[1] < 11))) {
    AA = select(AA, -c(prchld, prnmchld))
  } else {
    AA$prchld = factor(AA$prchld, levels = c(-3:15), labels = c("Refused", "Don't Know", "NIU (Not a parent)", "No own children under 18 years of age", "All own children 0-2 years of age",
                                                                "All own children 3-5 years of age", "All own children 6-13 years of age", "All own children 14-17 years of age", "Own children 0-2 and 3-5 years of age (none 6-17)", "Own children 0-2 and 6-13 years of age (none 3-5 or 14-17)",
                                                                "Own children 0-2 and 14-17 years of age (none 3-13)", "Own children 3-5 and 6-13 years of age (none 0-2 or 14-17)", "Own children 3-5 and 14-17 years of age (none 0-2 or 6-13)", "Own children 6-13 and 14-17 years of age (none 0-5)", "Own children 0-2, 3-5, and 6-13 years of age (none 14-17)",
                                                                "Own children 0-2, 3-5, and 14-17 years of age (none 6-13)", "Own children 0-2, 6-13, and 14-17 years of age (none 3-5)", "Own children 3-5, 6-13, and 14-17 years of age (none 0-2)", "Own children from all age groups"))
    AA$prnmchld[ AA$prnmchld <= -1] = NA
  }
  
  
  # These functions format the Allocation Flags section
  AA$prwernal = factor(AA$prwernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$prhernal = factor(AA$prhernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtenure = factor(AA$hxtenure, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelhhd = factor(AA$hxtelhhd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelavl = factor(AA$hxtelavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$hxphoneo = factor(AA$hxphoneo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxinusyr = factor(AA$pxinusyr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrrp = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxparent = factor(AA$pxparent, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxage = factor(AA$pxage, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmaritl = factor(AA$pxmaritl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxspouse = factor(AA$pxspouse, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxsex = factor(AA$pxsex, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafwhen = factor(AA$pxafwhen, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafnow = factor(AA$pxafnow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  AA$pxeduca = factor(AA$pxeduca, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrace = factor(AA$pxrace, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnatvty = factor(AA$pxnatvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmntvty = factor(AA$pxmntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxfntvty = factor(AA$pxfntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxorigin = factor(AA$pxorigin, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmlr = factor(AA$pxmlr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxret1 = factor(AA$pxret1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabsrsn = factor(AA$pxabsrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabspdo = factor(AA$pxabspdo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmjot = factor(AA$pxmjot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmjnum = factor(AA$pxmjnum, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl1 = factor(AA$pxhrusl1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl2 = factor(AA$pxhrusl2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrftpt = factor(AA$pxhrftpt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhruslt = factor(AA$pxhruslt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrwant = factor(AA$pxhrwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn1 = factor(AA$pxhrrsn1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn2 = factor(AA$pxhrrsn2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhract1 = factor(AA$pxhract1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhract2 = factor(AA$pxhract2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhractt = factor(AA$pxhractt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn3 = factor(AA$pxhrrsn3, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhravl = factor(AA$pxhravl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayavl = factor(AA$pxlayavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlaylk = factor(AA$pxlaylk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlaydur = factor(AA$pxlaydur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayfto = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkm1 = factor(AA$pxlkm1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkavl = factor(AA$pxlkavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlkll1o = factor(AA$pxlkll1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkll2o = factor(AA$pxlkll2o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlklwo = factor(AA$pxlklwo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkdur = factor(AA$pxlkdur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkfto = factor(AA$pxlkfto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwwnto = factor(AA$pxdwwnto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwrsn = factor(AA$pxdwrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwlko = factor(AA$pxdwlko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwwk = factor(AA$pxdwwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdw4wk = factor(AA$pxdw4wk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwlkwk = factor(AA$pxdwlkwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavl = factor(AA$pxdwavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavr = factor(AA$pxdwavr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhwko = factor(AA$pxjhwko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhrsn = factor(AA$pxjhrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxjhwant = factor(AA$pxjhwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1cow = factor(AA$pxio1cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1icd = factor(AA$pxio1icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1ocd = factor(AA$pxio1ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2cow = factor(AA$pxio2cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxio2icd = factor(AA$pxio2icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2ocd = factor(AA$pxio2ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernuot = factor(AA$pxernuot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernper = factor(AA$pxernper, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernh1o = factor(AA$pxernh1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernhro = factor(AA$pxernhro, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxern = factor(AA$pxern, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernwkp = factor(AA$pxernwkp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernrt = factor(AA$pxernrt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernhry = factor(AA$pxernhry, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernh2 = factor(AA$pxernh2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernlab = factor(AA$pxernlab, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxerncov = factor(AA$pxerncov, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfjh = factor(AA$pxnlfjh, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfret = factor(AA$pxnlfret, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxnlfact = factor(AA$pxnlfact, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschenr = factor(AA$pxschenr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschft = factor(AA$pxschft, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschlvl = factor(AA$pxschlvl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$qstnum[AA$qstnum <= -1] = NA
  AA$occurnum[AA$occurnum <= -1] = NA
  AA$pedipged = factor(AA$pedipged, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't know", "Not in Universe", "Graduation from high school", "GED or other equivalent"))
  AA$pehgcomp = factor(AA$pehgcomp, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't know", "Not in Universe", "Less than 1st grade", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade, NO DIPLOMA"))
  AA$pecyc = factor(AA$pecyc, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't know", "Not in Universe", "Less than 1 year (includes 0 years completed)", "The first, or Freshman year", "The second, or Sophmore year", "The third, or Junior year", "Four or more years"))
  
  
  # These variables remain uncertain as to their options
  AA$pxdipged = factor(AA$pxdipged, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhgcomp = factor(AA$pxhgcomp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxcyc = factor(AA$pxcyc, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pwcmpwgt[AA$pwcmpwgt <= -1] = NA
  
  return(AA)
}



ParserJanuary2003 = function(DataIn, DataDictionaryIn) {
  
  # This eliminates all of the variables in the dataset that are labelled "Remove" in the Dictionary Files.
  AA = select(DataIn, -all_of(filter(DataDictionaryIn, Adjustment == "Remove")$ColName))
  
  # These format properly the three variables that go into forming the HRHHID2 variable.
  AA$hrsample[AA$hrsample == "-1"] = NA
  AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
  AA$hrsersuf[is.na(AA$hrsersuf)] = -1
  AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")
  # This adds the variables that the Dictionary says to add, properly applying the formulas to create them.
  AA = tibble(AA, hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum), gecmsanum = AA$gecmsa, gemsanum = AA$gemsa, geconum = AA$geco)
  # This removes all of the variables that are labelled "Delete" in the dataset. 
  AA = select(AA, -all_of(filter(DataDictionaryIn, Adjustment == "Delete")$ColName))
  # This reorders all of the remaining variables to match the Dictionary Order
  AA = select(AA, filter(filter(DataDictionaryIn, Adjustment != "Remove"), Adjustment != "Delete")$ColName)
  
  
  
  
  
  
  # These functions format the Household Information section
  AA$hrhhid[AA$hrhhid == -1] = NA
  AA$hrmonth[AA$hrmonth == -1] = NA
  AA$hryear4[AA$hryear4 == -1] = NA
  AA$hurespli[AA$hurespli <= -1] = NA
  AA$hufinal = factor(AA$hufinal, 
                      levels = c(-3:-1, 0, 1, 2, 5, 24, 115, 200, 201, 202, 203, 204, 205, 210, 216, 217, 218, 219, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 240, 241, 242, 243, 244, 245, 246, 247, 248),
                      labels = c("Refused", "Don't Know", NA, "New interview - Not contacted", "Fully complete CATI interview", "Partially completed CATI interview", "Labor force complete, Supplement incomplete - CATI", "HH occupied entirely by Armed Forces members",
                                 "Partial interview with callback panned - CATI", "New interview - Contacted", "CAPI Complete", "Callback needed", "Sufficient partial - Precloseout",
                                 "Sufficient partial - At closeout", "Labor force complete, - Suppl. incomplete - CAPI", "CAPI complete reinterview", "No one home", "Temporarily absent",
                                 "Refused", "Other occupied - Specify", "Armed Forces occupied or under age 14", "Temp. occupied w/persons with URE", "Vacant regular",
                                 "Vacant - Storage of HHLD furniture", "Unfit, to be demolished", "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent or trailer site",
                                 "Permit granted - Construction not started", "Other - Specify", "Demolished", "House or trailer moved", "Outside segment",
                                 "Converted to perm. business or storage", "Merged", "Condemned", "Built after April 1, 1980", "Unused serial no./listing sheet line",
                                 "Other - Specify"))
  
  
  AA$hetenure = factor(AA$hetenure, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Owned or being bought by a HH member", "Rented for cash", "Occupied without payment of cash rent"))
  AA$hehousut = factor(AA$hehousut, levels = -3:12, labels = c("Refused", "Don't Know", NA, "Other unit", "House, apartment, flat", "HU in nontransient hotel, motel, etc.", "HU permanent in transient hotel, motel",
                                                               "HU in rooming house", "Mobile home or trailer w/no perm. room added", "Mobile home or trailer w/1 or more perm. rooms added", "HU not specified above", "Quarters not HU in rooming or brding HS",
                                                               "Unit not perm. in transient hotl, motl", "Uoccupied tent site or trlr site", "Student quarters in college dorm", "Other unit not specified above"))
  AA$hetelhhd = factor(AA$hetelhhd, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Telephone present in house", "Telephone not present in the house"))
  AA$hetelavl = factor(AA$hetelavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Telephone elsewhere available for household to use", "Telephone not available eslewhere for household to use"))
  AA$hephoneo = factor(AA$hephoneo, levels = c(-3:2), labels = c("Refused", "Don't Know", NA, "Unknown", "Telephone interview acceptable", "Telephone interview not acceptable"))
  
  
  if ((AA$hryear4[1] == 2003)|(AA$hrmonth[1] < 10)) {
    AA$hufaminc = factor(AA$hufaminc, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "Less than $5,000", "5,000 to 7,499", "7,500 to 9,999", "10,000 to 12,499", 
                                                                          "12,500 to 14,999", "15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", 
                                                                          "35,000 to 39,999", "40,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 or more"))
  } else {
    AA$hufaminc = factor(AA$hufaminc, levels = c(-3:-1, 1:16), labels = c("Refused", "Don't Know", NA, "Less than $5,000", "5,000 to 7,499", "7,500 to 9,999", "10,000 to 12,499", 
                                                                          "12,500 to 14,999", "15,000 to 19,999", "20,000 to 24,999", "25,000 to 29,999", "30,000 to 34,999", 
                                                                          "35,000 to 39,999", "40,000 to 49,999", "50,000 to 59,999", "60,000 to 74,999", "75,000 to 99,999",
                                                                          "100,000 to 149,999", "150,000 or more"))
  }
  
  
  AA$hutypea = factor(AA$hutypea, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "No one home (NOH)", "Temporarily absent (TA)", "Refused (Ref)", "Other occupied - Specify"))
  AA$hutypb = factor(AA$hutypb, levels = c(-3:-1, 1:9), labels = c("Refused", "Don't Know", NA, "Vacant regular", "Temporarily occupied by persons w/ URE", "Vacant-storage of HHLD furniture", "Unfit or to be demolished", 
                                                                   "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent site or trailer site", "Permit granted construction not started", "Other type B - Specify"))
  AA$hutypc = factor(AA$hutypc, levels = c(-3:-1, 1:6,8:9), labels = c("Refused", "Don't Know", NA, "Demolished", "House or trailer moved", "Outside segment", "Converted to perm. business or storage",
                                                                       "Merged", "Condemned", "Unused line of listing sheet", "Other - Specify"))
  AA$hwhhwgt[AA$hwhhwgt == -1] = NA
  AA$hrintsta = factor(AA$hrintsta, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Interview", "Type A Non-interview", "Type B Non-interview", "Type C Non-interview"))
  
  
  AA$hrnumhou[AA$hrnumhou == -1] = NA
  AA$hrhtype = factor(AA$hrhtype, levels = c(-3:10), labels = c("Refused", "Don't Know", NA, "Non-interview household", "Husband/Wife primary family (Neither AF)", "Husb/wife prim. family (Either/both AF)", "Unmarried civilian male-Prim. fam HHLDer",
                                                                "Unmarried civ. female-Prim. fam HHLDer", "Primary family HHLDer-RP in AF, Unmar.", "Civilian male primary individual", "Civilian female primary individual", "Primary individual HHLD-RP in AF",
                                                                "Group quarter with family", "Group quarters without family"))
  AA$hrmis[AA$hrmis == -1] = NA
  AA$huinttyp = factor(AA$huinttyp, levels = -3:2, labels = c("Refused", "Don't Know", NA, "Noninterview/Indeterminate", "Personal", "Telephone"))
  AA$huprscnt[AA$huprscnt == -1] = NA
  
  
  AA$hrlonglk = factor(AA$hrlonglk, levels = c(-3:-1, 0, 2, 3), labels = c("Refused", "Don't Know", NA, "MIS 1 or replaement HH (No link)", "MIS 2-4 or MIS 6-8", "MIS 5"))
  AA$hubus = factor(AA$hubus, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Someone in HH has a business/farm", "No one in HH has a business/farm"))
  AA$hubusl1[AA$hubusl1 <= -1] = NA
  AA$hubusl2[AA$hubusl2 <= -1] = NA
  AA$hubusl3[AA$hubusl3 <= -1] = NA
  
  
  AA$hubusl4[AA$hubusl4 <= -1] = NA
  
  
  
  # These functions format the Geographic Information section
  AA$gereg = factor(AA$gereg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Northeast", "Midwest", "South", "West"))
  AA$gestcen = factor(AA$gestcen, levels = c(-3:-1, 11:16, 21:23, 31:35, 41:47, 51:59, 61:64, 71:74, 81:88, 91:95), labels = c("Refused", "Don't Know", NA, "ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS", "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX", "MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI"))
  AA$gestfips[AA$gestfips == -1] = NA
  AA$gecmsa = factor(AA$gecmsa, levels = c(-3:-1, 0, 7:97), labels = c("Refused", "Don't Know", NA, "Not identified or Nonmetropolitan", str_c(7:97, " Specific CMSA Code")))
  AA$gemsa = factor(AA$gemsa, levels = c(-3:-1, 0, 80:9360), labels = c("Refused", "Don't Know", NA, "Not identified or nonmetropolitan", str_c(80:9360, " Specific MSA Code")))
  
  
  AA$geco = factor(AA$geco, levels = -3:810, labels = c("Refused", "Don't Know", NA, "Not identified", str_c(1:810, " State-specific County Code")))
  AA$gecmsanum[AA$gecmsanum <= -1] = NA
  AA$gemsanum[AA$gemsanum <= -1] = NA
  AA$geconum[AA$geconum <= -1] = NA
  AA$gemsast = factor(AA$gemsast, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Central City", "Balance", "Nonmetropolitan", "Not identified"))
  
  
  AA$gemetsta = factor(AA$gemetsta, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Metropolitan", "Nonmetropolitan", "Not identified"))
  AA$geindvcc = factor(AA$geindvcc, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not identified, Nonmetropolitan, or Not a central city", str_c(1:4, " Specific central city code")))
  AA$gemsasz = factor(AA$gemsasz, levels = c(-3:-1, 0, 2:7), labels = c("Refused", "Don't Know", NA, "Not identified or Nonmetropolitan", "100,000 - 249,999", "250,000 - 499,999", "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999", "5,000,000+"))
  
  
  
  # These functions format the Personal Information Demographic section
  AA$perrp = factor(AA$perrp, levels = c(-3:-1, 1:18), labels = c("Refused", "Don't Know", NA, "Reference person w/Rels.", "Reference person w/o Rels.", "Spouse", "Child", "Grandchild", "Parent", "Brother/Sister", "Other Rel. of Reference person", "Foster child", "Nonrel. of Ref. person w/Rels.", "Not used", "Nonrel. of Ref. person w/o Rels.", "Unmarried partner w/Rels.", "Unmarried partner w/out Rels.", "Housemate/Roommate w/Rels.", "Housemate/Roommate w/out Rels.", "Roomer/Boarder w/ Rels.", "Roomer/Boarder w/out Rels."))
  AA$peparent = factor(AA$peparent, levels = c(-3:-1, 1:99), labels = c("Refused", "Don't Know", "No parent", str_c(1:99, " Line Num of parent")))
  AA$prtage = factor(AA$prtage, levels = -3:90, labels = c("Refused", "Don't Know", NA, str_c(0:89, " years old"), "90+ years old"))
  AA$prtfage = factor(AA$prtfage, levels = -3:1, labels = c("Refused", "Don't Know", NA, "No top code", "Top coded value for age"))
  AA$pemaritl = factor(AA$pemaritl, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Married - Spouse present", "Married - Spouse absent", "Widowed", "Divorced", "Separated", "Never married"))
  
  
  AA$pespouse[AA$pespouse == -1] = NA
  AA$pesex = factor(AA$pesex, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Male", "Female"))
  AA$puafever = factor(AA$puafever, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Served in the Armed Forces at some point", "Never served in the Armed Forces"))
  AA$peafwhen = factor(AA$peafwhen, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Vietnam Era (8/64-4/75)", "Korean War (6/50-1/55)", "World War II (9/40-7/47)", "World War I (4/17-11/18)", "Other service (All other persiods)", "Nonveteran"))
  AA$peafnow = factor(AA$peafnow, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Currently in Armed Forces", "Not currently in Armed Forces"))
  
  
  AA$peeduca = factor(AA$peeduca, levels = c(-3:-1, 31:46), labels = c("Refused", "Don't Know", NA, "Less than 1st grade", "1st, 2nd, 3rd or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade no diploma", "High school grad-diploma or equivalent (GED)", "Some college but no degree", "Associate degree-Occupational/Vocational", "Associate degree-Academic program", "Bachelor's degree (Ex: BA, AB, BS)", "Master's degree (Ex: MA, MS, MEng, MEd, MSW)", "Professional School Deg (Ex: MD, DDS, DVM)", "Doctorate degree (Ex: PhD, EdD)"))
  AA$prdtrace = factor(AA$prdtrace, levels = c(-3:-1, 1:21), labels = c("Refused", "Don't Know", NA, "White Only", "Black Only", "American Indian, Alaskan Native Only", "Asian Only", "Hawaiian/Pacific Islander Only", "White-Black", "White-AI", "White-Asian", "White-Hawaiian", "Black-AI", "Black-Asian", "Black-HP", "AI-Asian", "Asian-HP", "W-B-AI", "W-B-A", "W-AI-A", "W-A-HP", "W-B-AI-A", "2 or 3 Races", "4 or 5 Races"))
  AA$prdthsp = factor(AA$prdthsp,  levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Mexican American", "Puerto Rican", "Cuban", "Central/South American", "Other Spanish"))
  AA$puchinhh = factor(AA$puchinhh, levels = c(-3:-1, 1:7, 9), labels = c("Refused", "Don't Know", NA, "Person added", "Person added - URE", "Person undeleted", "Person died", "Deleted for reason other than death", "Person joined Armed Forces", "Person no longer in AF", "Change in demographic information"))
  AA$pulineno[AA$pulineno == -1] = NA
  
  
  AA$prfamnum = factor(AA$prfamnum, levels = -3:19, labels = c("Refused", "Don't Know", NA, "Not a family member", "Primary family member only", "Subfamily No. 2 member", "Subfamily No. 3 member", "Subfamily No. 4 member", "Subfamily No. 5 member", "Subfamily No. 6 member", "Subfamily No. 7 member", "Subfamily No. 8 member", "Subfamily No. 9 member", "Subfamily No. 10 member", "Subfamily No. 11 member", "Subfamily No. 12 member", "Subfamily No. 13 member", "Subfamily No. 14 member", "Subfamily No. 15 member", "Subfamily No. 16 member", "Subfamily No. 17 member", "Subfamily No. 18 member", "Subfamily No. 19 member"))
  AA$prfamrel = factor(AA$prfamrel, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not a family member", "Reference person", "Spouse", "Child", "Other relative (Primary Family & Unrel)"))
  AA$prfamtyp = factor(AA$prfamtyp, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Primary family", "Primary individual", "Related subfamily", "Unrelated subfamily", "Secondary individual"))
  AA$pehspnon = factor(AA$pehspnon, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Hispanic", "Non-Hispanic"))
  AA$prmarsta = factor(AA$prmarsta, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Married, civilian spouse present", "Married, Armed Forces spouse present", "Married, Spouse absent (Exc. Separated)", "Widowed", "Divorced", "Separated", "Never married"))
  
  
  AA$prpertyp = factor(AA$prpertyp, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Child household member", "Adult civilian household member", "Adult armed forces household member"))
  AA$penatvty = factor(AA$penatvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person born in United States", "Person born in Puerto Rico", "Person born in U.S. Outlying Area", str_c(100:554, " Person born in Foreign Country or at sea - See Code list"), "Person born Abroad, country not known"))
  AA$pemntvty = factor(AA$pemntvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person's mother born in United States", "Person's mother born in Puerto Rico", "Person's mother born in U.S. Outlying Area", str_c(100:554, " Person's mother born in Foreign Country or at sea - See Code list"), "Person's mother born Abroad, country not known"))
  AA$pefntvty = factor(AA$pefntvty, levels = c(-3:-1, 57, 72, 96, 100:554, 555), labels = c("Refused", "Don't Know", NA, "Person's father born in United States", "Person's father born in Puerto Rico", "Person's father born in U.S. Outlying Area", str_c(100:554, " Person's father born in Foreign Country or at sea - See Code list"), "Person's father born Abroad, country not known"))
  AA$prcitshp = factor(AA$prcitshp, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Native, Born in the United States", "Native, Born in Puerto Rico or U.S. Outlying Area", "Native, Born abroad of American parent or parents", "Foreign born, U.S. citizen by naturalization", "Foreign born, not a citizen of the United States"))
  
  # Need to look more into the allocation flag issue for this following variable
  AA$prcitflg[AA$prcitflg == -1] = NA
  AA$prinusyr = factor(AA$prinusyr, levels = -3:14, labels = c("Refused", "Don't Know", "Not in universe (Born in U.S.)", "Not foreign born", "Immigrant entered before 1950", "Immigrant entered in 1950-1959", "Immigrant entered in 1960-1964", "Immigrant entered in 1965-1969", "Immigrant entered in 1970-1974", "Immigrant entered in 1975-1979", "Immigrant entered in 1980-1981", "Immigrant entered in 1982-1983", "Immigrant entered in 1984-1985", "Immigrant entered in 1986-1987", "Immigrant entered in 1988-1989", "Immigrant entered in 1990-1991", "Immigrant entered in 1992-1993", "Immigrant entered in 1994-1997"))
  
  
  # These functions format the Personal Information Labor Force section
  AA$puslfprx = factor(AA$puslfprx, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Labor Force Info collected by self", "Labor Force info collected by proxy", "Labor Force info collected by both self and proxy"))
  AA$pemlr = factor(AA$pemlr, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Employed-At work", "Employed-Absent", "Unemployed-On layoff", "Unemployed-Looking", "Not in Labor Force-Retired", "Not in Labor Force-Disabled", "Not in labor force-Other"))
  AA$puwk = factor(AA$puwk, levels = c(-3:-1,1:5), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pubus1 = factor(AA$pubus1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Person did unpaid work on family business/farm", "Person did not do unpaid work on family business/farm"))
  AA$pubus2ot = factor(AA$pubus2ot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Person received payments or profits from the family business", "Person did not receive payments or profits from the family business"))
  
  
  AA$puretot = factor(AA$puretot, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported retired last month and is retired now", "No, person who was reported retired last month is not retired now", "Person was not retired last month"))
  AA$pudis = factor(AA$pudis, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported disabled last month and is disabled now", "No, person who was reported disabled last month is not disabled now", "Person was not disabled last month"))
  AA$peret1 = factor(AA$peret1, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes, person who was reported retired last month wants a job (full or part-time)", "No, person who was reported retired last month does not want a job", "Person who was reported retired last month has a job now"))
  AA$pudis1 = factor(AA$pudis1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, person's disability prevents person from working in the next 6 months", "No, person's disability does not prevent person from accepting work in the next 6 months"))
  AA$pudis2 = factor(AA$pudis2, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, person has a disability that prevents person from working", "No, person does not have a disability that prevents person from working"))
  
  
  AA$puabsot = factor(AA$puabsot, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, person had a full/part-time job last week", "No, person didn't have a full/part-time job last week", "Person was retired last week", "Person was disabled last week", "Person was unable to work last week"))
  AA$pulay = factor(AA$pulay, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, person is on layoff from a job", "No, person is not on layoff from a job", "Person is retired", "Person is disabled", "Person is unable to work"))
  AA$peabsrsn = factor(AA$peabsrsn, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "On layoff", "Slack work/Business conditions", "Waiting for a new job to begin", "Vacation/Personal days", "Own illness/Injury/Medical problems", 
                                                                        "Child care problems", "Other family/Personal obligation", "Maternity/Paternity leave", "Labor dispute", "Weather affected job", 
                                                                        "School/Training", "Civic/Military Duty", "Does not work in the business", "Other (Specify)"))
  
  
  AA$peabspdo = factor(AA$peabspdo, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pemjot = factor(AA$pemjot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pemjnum = factor(AA$pemjnum, levels = c(-3:-1, 2:4), labels = c("Refused", "Don't Know", NA, "2 jobs", "3 jobs", "4 or more jobs"))
  AA$pehrusl1[AA$pehrusl1 == -1] = NA
  AA$pehrusl2[AA$pehrusl2 == -1] = NA
  
  
  AA$pehrftpt = factor(AA$pehrftpt, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Hours vary"))
  AA$pehruslt[AA$pehruslt == -1] = NA
  AA$pehrwant = factor(AA$pehrwant, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Regular hours are Full-time"))
  AA$pehrrsn1 = factor(AA$pehrrsn1, levels = c(-3:-1, 1:10), labels = c("Refused", "Don't Know", NA, "Slack work/Business conditions", "Could only find Part-time work", "Seasonal work", "Child care problems", "Other family/Personal obligation", "Health/Medical limitations", "School/Training", "Retired/Social security limit on earnings", "Full-time workweek is less than 35 hrs", "Other - Specify"))
  AA$pehrrsn2 = factor(AA$pehrrsn2, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Child care problems", "Other family/Personal obligations", "Health/Medical limitations", "School/Training", "Retired/Social Security limit on earnings", "Full-Time workweek less than 35 hours", "Other - Specify"))
  
  
  AA$pehrrsn3 = factor(AA$pehrrsn3, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Slack work/Business conditions", "Seasonal work", "Job started or ended during week", "Vacation/Personal day", "Own illnes/Injury/Medical appointment", "Holiday (Legal or Religious)", "Child care problems", "Other family/Personal obligations", "Labor dispute", "Weather affected job", "School/training", "Civic/Military duty", "Other reason"))
  AA$puhroff1 = factor(AA$puhroff1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puhroff2[AA$puhroff2 <= -1] = NA
  AA$puhrot1 = factor(AA$puhrot1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puhrot2[AA$puhrot2 <= -1] = NA
  
  
  AA$pehract1[AA$pehract1 <= -1] = NA
  AA$pehract2[AA$pehract2 <= -1] = NA
  AA$pehractt[AA$pehractt <= -1] = NA
  AA$pehravl = factor(AA$pehravl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulaydt = factor(AA$pulaydt, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$pulay6m = factor(AA$pulay6m, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pelayavl = factor(AA$pelayavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulayavr = factor(AA$pulayavr, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Own temporary illness", "Going to school", "Other"))
  AA$pelaylk = factor(AA$pelaylk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pelaydur[AA$pelaydur <= -1] = NA
  
  
  AA$pelayfto = factor(AA$pelayfto, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulk = factor(AA$pulk, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Retired", "Disabled", "Unable to work"))
  AA$pelkm1 = factor(AA$pelkm1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                    "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                    "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkm2 = factor(AA$pulkm2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm3 = factor(AA$pulkm3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkm4 = factor(AA$pulkm4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm5 = factor(AA$pulkm5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkm6 = factor(AA$pulkm6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                        "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                        "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk1 = factor(AA$pulkdk1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                      "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                      "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  AA$pulkdk2 = factor(AA$pulkdk2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pulkdk3 = factor(AA$pulkdk3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk4 = factor(AA$pulkdk4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk5 = factor(AA$pulkdk5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkdk6 = factor(AA$pulkdk6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps1 = factor(AA$pulkps1, levels = c(-3:-1, 1:13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                      "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                      "Looked at ads", "Attended job training programs/courses", "Nothing", "Other passive"))
  
  
  AA$pulkps2 = factor(AA$pulkps2, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps3 = factor(AA$pulkps3, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps4 = factor(AA$pulkps4, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps5 = factor(AA$pulkps5, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  AA$pulkps6 = factor(AA$pulkps6, levels = c(-3:-1, 1:11, 13), labels = c("Refused", "Don't Know", NA, "Contacted employer directly/Interview", "Contacted public employment agency", "Contacted private employment agency", "Contacted friends or relatives",
                                                                          "Contacted school/univrsity empl center", "Sent out resumes/filled out application", "Checked union/professional registers", "Placed or answered ads", "Other active",
                                                                          "Looked at ads", "Attended job training programs/courses", "Other passive"))
  
  
  AA$pelkavl = factor(AA$pelkavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pulkavr = factor(AA$pulkavr, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Waiting for new job to begin", "Own temporary illness", "Going to school", "Other - Specify"))
  AA$pelkll1o = factor(AA$pelkll1o, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Working", "School", "Left military service", "Something else"))
  AA$pelkll2o = factor(AA$pelkll2o, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Lost job", "Quit job", "Temporary job ended"))
  AA$pelklwo = factor(AA$pelklwo, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Last worked within the last 12 months", "Last worked more than 12 months ago", "Never worked previously"))
  
  
  AA$pelkdur[AA$pelkdur == -1] = NA
  AA$pelkfto = factor(AA$pelkfto, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Yes", "No", "Doesn't matter"))
  AA$pedwwnto = factor(AA$pedwwnto, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Yes, or maybe, it depends", "No", "Retired", "Disabled", "Unable to work"))
  AA$pedwrsn = factor(AA$pedwrsn, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Believes no work available in area of expertise", "Couldn't find any work", "Lacks necessary schooling/training", "Employers think too young or too old", "Other types of discrimination",
                                                                      "Can't arrange child care", "Family responsibilities", "In school or other training", "Ill-health, physcial disability", "Transportation problems", "Other - Specify"))
  AA$pedwlko = factor(AA$pedwlko, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$pedwwk = factor(AA$pedwwk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedw4wk = factor(AA$pedw4wk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwlkwk = factor(AA$pedwlkwk, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwavl = factor(AA$pedwavl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pedwavr = factor(AA$pedwavr, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Own temporary illness", "Going to school", "Other"))
  
  
  AA$pejhwko = factor(AA$pejhwko, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pujhdp1o = factor(AA$pujhdp1o, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$pejhrsn = factor(AA$pejhrsn, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Personal/Family (Including Pregnancy)", "Return to school", "Health", "Retirement or old age", "Temp, Seasonal or intermittent job complete", "Slack work/business conditions", "Unsatisfactory work arrangements (Hrs, pay, etc.)", "Other - specify"))
  AA$pejhwant = factor(AA$pejhwant, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes, or it depends", "No"))
  AA$prabsrea = factor(AA$prabsrea, levels = c(-3:-1, 1:40), labels = c("Refused", "Don't Know", NA, "FT paid-Vacation", "FT paid-Own illness", "FT paid-Child care problems", "FT paid-Other family/Personal oblig.",
                                                                        "FT paid-Maternity/Paternity leave", "FT paid-Labor dispute", "FT paid-Weather affected job", "FT paid-School/Training", "FT paid-Civic/Military duty",
                                                                        "FT paid-Other", "FT unpaid-Vacation", "FT unpaid-Own illness", "FT unpaid-Child care problems", "FT unpaid-Other fam/Personal obligation",
                                                                        "FT unpaid-Maternity/Paternity leave", "FT unpaid-Labor dispute", "FT unpaid-Weather affected job", "FT unpaid-School/Training", "FT unpaid-Civic/Military duty",
                                                                        "FT unpaid-Other", "PT paid-Vacation", "PT paid-Own illness", "PT paid-Child care problems", "PT paid-Other family/Personal oblig.",
                                                                        "PT paid-Maternity/Paternity leave", "PT paid-Labor dispute", "PT paid-Weather affected job", "PT paid-School/Training", "PT paid-Civic/Military duty",
                                                                        "PT paid-Other", "PT unpaid-Vacation", "PT unpaid-Own illness", "PT unpaid-Child care problems", "PT unpaid-Other fam/Personal obligation",
                                                                        "PT unpaid-Maternity/Paternity leave", "PT unpaid-Labor dispute", "PT unpaid-Weather affected job", "PT unpaid-School/Training", "PT unpaid-Civic/Military duty",
                                                                        "PT unpaid-Other"))
  
  
  AA$prcivlf = factor(AA$prcivlf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "In Civilian Labor Force", "Not in Civilian Labor Force"))
  AA$prdisc = factor(AA$prdisc, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Discouraged worker", "Conditionally interested", "Not available"))
  AA$premphrs = factor(AA$premphrs, levels = -3:22, labels = c("Refused", "Don't Know", NA, "Unemployed and NILF", "W/job, Not at work-Illness", "W/job, not at work-Vacation", "W/job, not at work-Weather affected job",
                                                               "W/job, not at work-Labor dispute", "W/job, not at work-Child care problems", "W/job, not at work-Fam/Pers obligation", "W/job, not at work-Maternity/Paternity", "W/job, not at work-School/Training",
                                                               "W/job, not at work-Civic/Military duty", "W/job, not at work-Does not work in bus", "W/job, not at work-Other", "At work- 1-4 hrs", "At work- 5-14 hrs",
                                                               "At work- 15-21 hrs", "At work- 22-29 hrs", "At work- 30-34 hrs", "At work- 35-39 hrs", "At work- 40 hrs",
                                                               "At work- 41-47 hrs", "At work- 48 hrs", "At work- 49-59 hrs", "At work- 60 hrs or more"))
  AA$prempnot = factor(AA$prempnot, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Employed", "Unemployed", "Not in the Labor Force (NILF)-discouraged", "Not in the Labor Force (NILF)-other"))
  AA$prexplf = factor(AA$prexplf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Employed", "Unemployed"))
  
  
  AA$prftlf = factor(AA$prftlf, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Full Time Labor Force", "Part Time Labor Force"))
  AA$prhrusl = factor(AA$prhrusl, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "0-20 hrs", "21-34 hrs", "35-39 hrs", "40 hrs", "41-49 hrs", "50 or more hrs", "Varies-Full Time", "Varies-Part Time"))
  AA$prjobsea = factor(AA$prjobsea, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't Know", NA, "Looked last 4 weeks - Not worked", "Looked last 4 weeks - Worked", "Looked last 4 weeks - Layoff", "Unavailable job seekers", "No recent job search"))
  AA$prpthrs = factor(AA$prpthrs, levels = -3:12, labels = c("Refused", "Don't Know", NA, "Usually FT, PT for Noneconomic reasons", "Usu.FT, PT econ reasons; 1-4 hrs", "Usu.FT, PT econ reasons; 5-14 hrs", "Usu.FT, PT econ reasons; 15-29 hrs",
                                                             "Usu.FT, PT econ reasons; 30-34 hrs", "Usu.PT, econ reasons; 1-4 hrs", "Usu.PT, econ reasons; 5-14 hrs", "Usu.PT, econ reasons 15-29 hrs", "Usu.PT, econ reasons 30-34 hrs",
                                                             "Usu.PT, non-econ reasons; 1-4 hrs", "Usu.PT, non-econ reasons; 5-14 hrs", "Usu.PT, non-econ reasons; 15-29 hrs", "Usu.PT, non-econ reasons; 30-34 hrs"))
  AA$prptrea = factor(AA$prptrea, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Usu. FT-Slack work/Business conditions", "Usu. FT-Seasonal work", "Usu. FT-Job started/ended during week", "Usu. FT-Vacation/Personal day",
                                                                      "Usu. FT-Own illness/Injury/Medical appointment", "Usu. FT-Holiday (Religious or Legal)", "Usu. FT-Child care problems", "Usu. FT-Other fam/Pers obligations", "Usu. FT-Labor dispute",
                                                                      "Usu. FT-Weather affected job", "Usu. FT-School/Training", "Usu. FT-Civic/Military Duty", "Usu. FT-Other reason", "Usu. PT-Slack work/Business conditions",
                                                                      "Usu. PT-Could only find PT work", "Usu. PT-Seasonal work", "Usu. PT-Child care problems", "Usu. PT-Other fam/Pers obligations", "Usu. PT-Health/Medical Limitations", 
                                                                      "Usu. PT-School/Training", "Usu. PT-Retired/S.S. limit on earnings", "Usu. PT-Workweek <35 hours", "Usu. PT-Other reason"))
  
  
  AA$prunedur[AA$prunedur == -1] = NA
  AA$pruntype = factor(AA$pruntype, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Job loser/On layoff", "Other job loser", "Temportary job ended", "Job leaver", "Re-entrant", "New-entrant"))
  AA$prwksch = factor(AA$prwksch, levels = -3:4, labels = c("Refused", "Don't Know", NA, "Not in Labor Force", "At work", "With job, not at work", "Unemployed, seeks FT", "Unemployed, seeks PT"))
  AA$prwkstat = factor(AA$prwkstat, levels = c(-3:-1, 1:12), labels = c("Refused", "Don't Know", NA, "Not in Labor Force", "FT hours (35+), usually FT", "PT for economic reasons, usually FT", "PT for non-economic reasons, usually FT",
                                                                        "Not at work, usually FT", "PT hrs, usually PT for economic reasons", "PT hrs, usually PT for non-economic reasons", "FT hours, usually PT for economic reasons", "FT hours, usually PT for non-economic reasons", 
                                                                        "Not at work, usually Part-Time", "Unemployed FT", "Unemployed PT"))
  AA$prwntjob = factor(AA$prwntjob, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Want a job", "Other Not in the Labor Force (NILF)"))
  
  
  AA$puiodp1 = factor(AA$puiodp1, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puiodp2 = factor(AA$puiodp2, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$puiodp3 = factor(AA$puiodp3, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peio1cow = factor(AA$peio1cow, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay"))
  AA$puio1mfg = factor(AA$puio1mfg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Manufacturing", "Retail trade", "Wholesale trade", "Something else"))
  
  
  AA$peio2cow = factor(AA$peio2cow, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't Know", NA, "Government - Federal", "Government - State", "Government - Local", "Private, for profit", "Private, nonprofit", "Self-employed, incorporated", "Self-employed, unincorporated", "Without pay"))
  AA$puio2mfg = factor(AA$puio2mfg, levels = c(-3:-1, 1:4), labels = c("Refused", "Don't Know", NA, "Manufacturing", "Retail Trade", "Wholesale Trade", "Something else"))
  AA$prioelg = factor(AA$prioelg, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not eligible for edit", "Eligible for edit"))
  AA$pragna = factor(AA$pragna, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Agricultural", "Non-agricultural"))
  AA$prcow1 = factor(AA$prcow1, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  
  
  AA$prcow2 = factor(AA$prcow2, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Federal Govt", "State Govt", "Local Govt", "Private (Incl. Self-employed Incorp.)", "Self-employed unincorp.", "Without Pay"))
  AA$prcowpg = factor(AA$prcowpg, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Private", "Government"))
  AA$prdtcow1 = factor(AA$prdtcow1, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtcow2 = factor(AA$prdtcow2, levels = c(-3:-1, 1:11), labels = c("Refused", "Don't Know", NA, "Agri., Wage & Salary, Private", "Agri., Wage & Salary, Government", "Agri., Self-employed", "Agri., Unpaid", "Nonag, WS, Private, Private HHLDs", "Nonag, WS, Private, Other Private", "Nonag, WS, Govt, Federal", "Nonag, WS, Govt, State", "Nonag, WS, Govt, Local", "Nonag, Self-employed", "Nonag, unpaid"))
  AA$prdtind1 = factor(AA$prdtind1, levels = c(-3:-1, 1:52), labels = c("Refused", "Don't Know", NA, "Agriculture", "Forestry, logging, fishing, hunting, and trapping", "Mining", "Construction", "Nonmetallic mineral product manufacturing",
                                                                        "Primary metals and fabricated metal products", "Machinery manufacturing", "Computer and electronic product manufacturing", "Electrical equipment, appliance manufacturing", "Transportation equipment manufacturing", 
                                                                        "Wood products", "Furniture and fixtures manufacturing", "Miscellaneous and not specified manufacturing", "Food manufacturing", "Beverage and tobacco products",
                                                                        "Textile, apparel, and leather manufacturing", "Paper and printing", "Petroleum and coal products manufacturing", "Chemical manufacturing", "Plastics and rubber products",
                                                                        "Wholesale trade", "Retail trade", "Transportation and warehousing", "Utilities", "Publishing industries (except internet)",
                                                                        "Motion picture and sound recording industries", "Broadcasting (except internet)", "Internet publishing and broadcasting", "Telecommunications", "Internet service providers and data processing services",
                                                                        "Other information services", "Finance", "Insurance", "Real estate", "Rental and leasing service",
                                                                        "Professional and technical services", "Management of companies and enterprises", "Adminitrative and support services", "Waste management and remediation services", "Educational services",
                                                                        "Hospitals", "Health care services, except hospitals", "Social assistance", "Arts, entertainment, and recreation", "Accommodation",
                                                                        "Food services and drinking places", "Repair and maintenance", "Personal and laundry services", "Membership associations and organizations", "Private households",
                                                                        "Public administration", "Armed forces"))
  
  
  AA$prdtind2 = factor(AA$prdtind2, levels = c(-3:-1, 1:52), labels = c("Refused", "Don't Know", NA, "Agriculture", "Forestry, logging, fishing, hunting, and trapping", "Mining", "Construction", "Nonmetallic mineral product manufacturing",
                                                                        "Primary metals and fabricated metal products", "Machinery manufacturing", "Computer and electronic product manufacturing", "Electrical equipment, appliance manufacturing", "Transportation equipment manufacturing", 
                                                                        "Wood products", "Furniture and fixtures manufacturing", "Miscellaneous and not specified manufacturing", "Food manufacturing", "Beverage and tobacco products",
                                                                        "Textile, apparel, and leather manufacturing", "Paper and printing", "Petroleum and coal products manufacturing", "Chemical manufacturing", "Plastics and rubber products",
                                                                        "Wholesale trade", "Retail trade", "Transportation and warehousing", "Utilities", "Publishing industries (except internet)",
                                                                        "Motion picture and sound recording industries", "Broadcasting (except internet)", "Internet publishing and broadcasting", "Telecommunications", "Internet service providers and data processing services",
                                                                        "Other information services", "Finance", "Insurance", "Real estate", "Rental and leasing service",
                                                                        "Professional and technical services", "Management of companies and enterprises", "Adminitrative and support services", "Waste management and remediation services", "Educational services",
                                                                        "Hospitals", "Health care services, except hospitals", "Social assistance", "Arts, entertainment, and recreation", "Accommodation",
                                                                        "Food services and drinking places", "Repair and maintenance", "Personal and laundry services", "Membership associations and organizations", "Private households",
                                                                        "Public administration", "Armed forces"))
  AA$prdtocc1 = factor(AA$prdtocc1, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Management occupations", "Business and financial operations occupations", "Computer and mathematical science occupations", "Architecture and engineering occupations", "Life, physical, and social science occupations",
                                                                        "Community and social service occupations", "Legal occupations", "Education, training, and library occupations", "Arts, design, entertainment, sports, and media occupations", "Healthcare practitioner and technical occupations",
                                                                        "Healthcare support occupations", "Protective service occupations", "Food preparation and serving related occupations", "Building and grounds cleaning and maintenance occupations", "Personal care and service occupations",
                                                                        "Sales and related occupations", "Office and administrative support occupations", "Farming, fishing, and forestry occupations", "Construction and extraction occupations", "Insatllation, maintenance, and repair occupations",
                                                                        "Production occupations", "Transportation and material moving occupations", "Armed Forces"))
  AA$prdtocc2 = factor(AA$prdtocc2, levels = c(-3:-1, 1:23), labels = c("Refused", "Don't Know", NA, "Management occupations", "Business and financial operations occupations", "Computer and mathematical science occupations", "Architecture and engineering occupations", "Life, physical, and social science occupations",
                                                                        "Community and social service occupations", "Legal occupations", "Education, training, and library occupations", "Arts, design, entertainment, sports, and media occupations", "Healthcare practitioner and technical occupations",
                                                                        "Healthcare support occupations", "Protective service occupations", "Food preparation and serving related occupations", "Building and grounds cleaning and maintenance occupations", "Personal care and service occupations",
                                                                        "Sales and related occupations", "Office and administrative support occupations", "Farming, fishing, and forestry occupations", "Construction and extraction occupations", "Insatllation, maintenance, and repair occupations",
                                                                        "Production occupations", "Transportation and material moving occupations", "Armed Forces"))
  AA$premp = factor(AA$premp, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Employed persons (Excluding farm & private households)"))
  AA$prmjind1 = factor(AA$prmjind1, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "Agriculture, forestry, fishing, and hunting", "Mining", "Construction", "Manufacturing", "Wholesale and retail trade",
                                                                        "Transportation and utilities", "Information", "Financial activites", "Professional and business services", "Educational and health services",
                                                                        "Leisure and hospitality", "Other services", "Public administration", "Armed Forces"))
  
  
  AA$prmjind2 = factor(AA$prmjind2, levels = c(-3:-1, 1:14), labels = c("Refused", "Don't Know", NA, "Agriculture, forestry, fishing, and hunting", "Mining", "Construction", "Manufacturing", "Wholesale and retail trade",
                                                                        "Transportation and utilities", "Information", "Financial activites", "Professional and business services", "Educational and health services",
                                                                        "Leisure and hospitality", "Other services", "Public administration", "Armed Forces"))
  AA$prmjocc1 = factor(AA$prmjocc1, levels = c(-3:-1,1:11), labels = c("Refused", "Don't Know", NA, "Management, business, and financial occupations", "Professional and related occupations", "Service occupations", "Sales and related occupations", "Office and administrative support occupations",
                                                                       "Farming, fishing, and forestry occupations", "Construction and extraction occupations", "Installation, maintenance, and repair occupations", "Production occupations", "Transportation and material moving occupations", "Armed Forces"))
  AA$prmjocc2 = factor(AA$prmjocc2, levels = c(-3:-1,1:11), labels = c("Refused", "Don't Know", NA, "Management, business, and financial occupations", "Professional and related occupations", "Service occupations", "Sales and related occupations", "Office and administrative support occupations",
                                                                       "Farming, fishing, and forestry occupations", "Construction and extraction occupations", "Installation, maintenance, and repair occupations", "Production occupations", "Transportation and material moving occupations", "Armed Forces"))
  AA$prmjocgr = factor(AA$prmjocgr, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Managerial, professional, and related occupations", "Service occupations", "Sales and office occupations", "Farming, forestry, and fishing occupations", "Construction and maintenance occupations", "Production, transportation, and material moving occupations", "Arned Forces"))
  AA$prnagpws = factor(AA$prnagpws, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Non-Ag Private Wage & Salary workers (excluding private HH)"))
  
  
  AA$prnagws = factor(AA$prnagws, levels = c(-3:-1, 1), labels = c("Refused", "Don't Know", NA, "Non-Ag Wage & Salary workers"))
  AA$prsjmj = factor(AA$prsjmj, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Single jobholder", "Multiple jobholder"))
  AA$prerelg = factor(AA$prerelg, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not eligible for edit", "Eligible for edit"))
  AA$peernuot = factor(AA$peernuot, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peernper = factor(AA$peernper, levels = c(-3:-1, 1:7), labels = c("Refused", "Don't Know", NA, "Hourly", "Weekly", "Bi-weekly", "Twice monthly", "Monthly", "Annually", "Other - Specify"))
  
  AA$peernrt = factor(AA$peernrt, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$peernhry = factor(AA$peernhry, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Hourly workers", "Nonhourly workers"))
  AA$puernh1c[AA$puernh1c <= -1] = NA
  AA$peernh2[AA$peernh2 <= -1] = NA
  AA$peernh1o[AA$peernh1o <= -1] = NA
  
  
  AA$prernhly[AA$prernhly <= -1] = NA
  AA$pthr = factor(AA$pthr, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  AA$peernhro[AA$peernhro <= -1] = NA
  AA$prernwa[AA$prernwa <= -1] = NA
  AA$ptwk = factor(AA$ptwk, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  
  
  AA$peern[AA$peern <= -1] = NA
  AA$puern2[AA$puern2 <= -1] = NA
  AA$ptot = factor(AA$ptot, levels = -3:1, labels = c("Refused", "Don't Know", NA, "Not topcoded", "Topcoded"))
  AA$peernwkp[AA$peernwkp <= -1] = NA
  AA$peernlab = factor(AA$peernlab, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$peerncov = factor(AA$peerncov, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$penlfjh = factor(AA$penlfjh, levels = c(-3:-1, 1:3), labels = c("Refused", "Don't Know", NA, "Within the last 12 months", "More than 12 months ago", "Never worked"))
  AA$penlfret = factor(AA$penlfret, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  AA$penlfact = factor(AA$penlfact, levels = c(-3:-1, 1:6), labels = c("Refused", "Don't Know", NA, "Disabled", "Ill", "In School", "Taking care of house or family", "In retirement", "Something else/other"))
  AA$peschenr = factor(AA$peschenr, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Yes", "No"))
  
  
  AA$peschft = factor(AA$peschft, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "Full-Time", "Part-Time"))
  AA$peschlvl = factor(AA$peschlvl, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "High school", "College or University"))
  AA$prnlfsch = factor(AA$prnlfsch, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't Know", NA, "In school", "Not in school"))
  AA$pwfmwgt[AA$pwfmwgt <= -1] = NA
  AA$pwlgwgt[AA$pwlgwgt <= -1] = NA
  
  
  AA$pworwgt[AA$pworwgt <= -1] = NA
  AA$pwsswgt[AA$pwsswgt <= -1] = NA
  AA$pwvetwgt[AA$pwvetwgt <= -1] = NA
  AA$prchld = factor(AA$prchld, levels = c(-3:15), labels = c("Refused", "Don't Know", "NIU (Not a parent)", "No own children under 18 years of age", "All own children 0-2 years of age",
                                                              "All own children 3-5 years of age", "All own children 6-13 years of age", "All own children 14-17 years of age", "Own children 0-2 and 3-5 years of age (none 6-17)", "Own children 0-2 and 6-13 years of age (none 3-5 or 14-17)",
                                                              "Own children 0-2 and 14-17 years of age (none 3-13)", "Own children 3-5 and 6-13 years of age (none 0-2 or 14-17)", "Own children 3-5 and 14-17 years of age (none 0-2 or 6-13)", "Own children 6-13 and 14-17 years of age (none 0-5)", "Own children 0-2, 3-5, and 6-13 years of age (none 14-17)",
                                                              "Own children 0-2, 3-5, and 14-17 years of age (none 6-13)", "Own children 0-2, 6-13, and 14-17 years of age (none 3-5)", "Own children 3-5, 6-13, and 14-17 years of age (none 0-2)", "Own children from all age groups"))
  AA$prnmchld[ AA$prnmchld <= -1] = NA
  
  # These functions format the Allocation Flags section
  AA$prwernal = factor(AA$prwernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$prhernal = factor(AA$prhernal, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtenure = factor(AA$hxtenure, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelhhd = factor(AA$hxtelhhd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$hxtelavl = factor(AA$hxtelavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$hxphoneo = factor(AA$hxphoneo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxinusyr = factor(AA$pxinusyr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrrp = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxparent = factor(AA$pxparent, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxage = factor(AA$pxage, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmaritl = factor(AA$pxmaritl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxspouse = factor(AA$pxspouse, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxsex = factor(AA$pxsex, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafwhen = factor(AA$pxafwhen, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxafnow = factor(AA$pxafnow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxeduca = factor(AA$pxeduca, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxrace1 = factor(AA$pxrace1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnatvty = factor(AA$pxnatvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmntvty = factor(AA$pxmntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxfntvty = factor(AA$pxfntvty, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhspnon = factor(AA$pxhspnon, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmlr = factor(AA$pxmlr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxret1 = factor(AA$pxret1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabsrsn = factor(AA$pxabsrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxabspdo = factor(AA$pxabspdo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxmjot = factor(AA$pxmjot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxmjnum = factor(AA$pxmjnum, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl1 = factor(AA$pxhrusl1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrusl2 = factor(AA$pxhrusl2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrftpt = factor(AA$pxhrftpt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhruslt = factor(AA$pxhruslt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrwant = factor(AA$pxhrwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn1 = factor(AA$pxhrrsn1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn2 = factor(AA$pxhrrsn2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhract1 = factor(AA$pxhract1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxhract2 = factor(AA$pxhract2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhractt = factor(AA$pxhractt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhrrsn3 = factor(AA$pxhrrsn3, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhravl = factor(AA$pxhravl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayavl = factor(AA$pxlayavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlaylk = factor(AA$pxlaylk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlaydur = factor(AA$pxlaydur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlayfto = factor(AA$pxrrp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkm1 = factor(AA$pxlkm1, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkavl = factor(AA$pxlkavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxlkll1o = factor(AA$pxlkll1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkll2o = factor(AA$pxlkll2o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlklwo = factor(AA$pxlklwo, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkdur = factor(AA$pxlkdur, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxlkfto = factor(AA$pxlkfto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwwnto = factor(AA$pxdwwnto, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwrsn = factor(AA$pxdwrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwlko = factor(AA$pxdwlko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwwk = factor(AA$pxdwwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdw4wk = factor(AA$pxdw4wk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxdwlkwk = factor(AA$pxdwlkwk, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavl = factor(AA$pxdwavl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxdwavr = factor(AA$pxdwavr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhwko = factor(AA$pxjhwko, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxjhrsn = factor(AA$pxjhrsn, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxjhwant = factor(AA$pxjhwant, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1cow = factor(AA$pxio1cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1icd = factor(AA$pxio1icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio1ocd = factor(AA$pxio1ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2cow = factor(AA$pxio2cow, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxio2icd = factor(AA$pxio2icd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxio2ocd = factor(AA$pxio2ocd, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernuot = factor(AA$pxernuot, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernper = factor(AA$pxernper, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernh1o = factor(AA$pxernh1o, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernhro = factor(AA$pxernhro, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxern = factor(AA$pxern, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernwkp = factor(AA$pxernwkp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernrt = factor(AA$pxernrt, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernhry = factor(AA$pxernhry, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxernh2 = factor(AA$pxernh2, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxernlab = factor(AA$pxernlab, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxerncov = factor(AA$pxerncov, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfjh = factor(AA$pxnlfjh, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxnlfret = factor(AA$pxnlfret, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  AA$pxnlfact = factor(AA$pxnlfact, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschenr = factor(AA$pxschenr, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschft = factor(AA$pxschft, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxschlvl = factor(AA$pxschlvl, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  
  
  
  AA$qstnum[AA$qstnum <= -1] = NA
  AA$occurnum[AA$occurnum <= -1] = NA
  AA$pedipged = factor(AA$pedipged, levels = c(-3:-1, 1, 2), labels = c("Refused", "Don't know", "Not in Universe", "Graduation from high school", "GED or other equivalent"))
  AA$pehgcomp = factor(AA$pehgcomp, levels = c(-3:-1, 1:8), labels = c("Refused", "Don't know", "Not in Universe", "Less than 1st grade", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade", "7th or 8th grade", "9th grade", "10th grade", "11th grade", "12th grade, NO DIPLOMA"))
  AA$pecyc = factor(AA$pecyc, levels = c(-3:-1, 1:5), labels = c("Refused", "Don't know", "Not in Universe", "Less than 1 year (includes 0 years completed)", "The first, or Freshman year", "The second, or Sophmore year", "The third, or Junior year", "Four or more years"))
  
  
  # These variables remain uncertain as to their options
  AA$pxdipged = factor(AA$pxdipged, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxhgcomp = factor(AA$pxhgcomp, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pxcyc = factor(AA$pxcyc, levels = -1:1, labels = c(NA, "No allocation", "One or more components of the recode are allocated"))
  AA$pwcmpwgt[AA$pwcmpwgt <= -1] = NA
  AA$peio1icd[AA$peio1icd == -1] = NA
  
  
  AA$peio1ocd[AA$peio1ocd == -1] = NA
  AA$peio2icd[AA$peio2icd == -1] = NA 
  AA$peio2ocd[AA$peio2ocd == -1] = NA
  AA$primind1 = factor(AA$primind1, levels = c(-3:-1, 1:22), labels = c("Refused", "Don't Know", NA, "Agriculture, forestry, fishing, and hunting", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable goods",
                                                                        "Wholesale trade", "Retail trade", "Transportation and warehousing", "Utilities", "Information", "Finance and Insurance",  
                                                                        "Real estate and rental and leasing", "Professional and technical services", "Management, administrative, and waste manufacturing services", "Educational services", "Health care and social services", "Arts, entertainment, and recreation",
                                                                        "Accomodation and food services", "Private households", "Other services, except private households", "Public administration", "Armed Forces"))
  AA$primind2 = factor(AA$primind2, levels = c(-3:-1, 1:22), labels = c("Refused", "Don't Know", NA, "Agriculture, forestry, fishing, and hunting", "Mining", "Construction", "Manufacturing - Durable goods", "Manufacturing - Non-durable goods",
                                                                        "Wholesale trade", "Retail trade", "Transportation and warehousing", "Utilities", "Information", "Finance and Insurance",  
                                                                        "Real estate and rental and leasing", "Professional and technical services", "Management, administrative, and waste manufacturing services", "Educational services", "Health care and social services", "Arts, entertainment, and recreation",
                                                                        "Accomodation and food services", "Private households", "Other services, except private households", "Public administration", "Armed Forces"))
  
  
  return(AA)
}