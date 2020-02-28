ParserJanuary1994 = function(AA, DictionaryIn) {
  AA$huinttyp = factor(AA$huinttyp, levels = -1:2, labels = c(NA, "Noninterview/Indeterminate", "Personal", "Telephone"))
  AA$huprscnt[AA$huprscnt == -1] = NA
  AA$hurespli[AA$hurespli == -1] = NA
  AA$hufinal = factor(AA$hufinal, 
                      levels = c(0, 1, 2, 5, 24, 115, 200, 201, 202, 203, 204, 205, 210, 216, 217, 218, 219, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 240, 241, 242, 243, 244, 245, 246, 247, 248),
                      labels = c("New interview - Not contacted", "Fully complete CATI interview", "Partially completed CATI interview", "Labor force complete, Supplement incomplete - CATI", "HH occupied entirely by Armed Forces members",
                                 "Partial interview with callback panned - CATI", "New interview - Contacted", "CAPI Complete", "Callback needed", "Sufficient partial - Precloseout",
                                 "Sufficient partial - At closeout", "Labor force complete, - Suppl. incomplete - CAPI", "CAPI complete reinterview", "No one home", "Temporarily absent",
                                 "Refused", "Other occupied - Specify", "Armed Forces occupied or under age 14", "Temp. occupied w/persons with URE", "Vacant regular",
                                 "Vacant - Storage of HHLD furniture", "Unfit, to be demolished", "Under construction, not ready", "Converted to temp business or storage", "Unoccupied tent or trailer site",
                                 "Permit granted - Construction not started", "Other - Specify", "Demolished", "House or trailer moved", "Outside segment",
                                 "Converted to perm. business or storage", "Merged", "Condemned", "Built after April 1, 1980", "Unused serial no./listing sheet line",
                                 "Other - Specify"))
}


X = sort(unique(AA$hufinal))
XX = sort(c(X, 0, 24, 200, 210))
View(XX)
