ParserJanuary1994 = function(AA, DictionaryIn) {
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
  AA$hryear = 1900 + AA$hryear
  AA$hrlonglk = factor(AA$hrlonglk, levels = c(-1, 0, 2, 3), labels = c(NA, "MIS 1 or replaement HH (No link)", "MIS 2-4 or MIS 6-8", "MIS 5"))
  AA$hrsample[AA$hrsample == "-1"] = NA
  AA$hrsample = str_trunc(AA$hrsample, width = 2, side = "left", ellipsis = "")
  AA$hrsersuf[is.na(AA$hrsersuf)] = -1
  AA$hrsersuf = str_trunc(str_c("0", match(as.character(AA$hrsersuf), c("-1", LETTERS[1:26]))-1), width = 2, side = "left", ellipsis = "")
  AA$hrhhid2 = str_c(AA$hrsample, AA$hrsersuf, AA$huhhnum)
  AA = AA %>% select(1:23, hrhhid2,everything())
}


# X = sort(unique(AA$hufinal))
# XX = sort(c(X, 0, 24, 200, 210))
# View(XX)
