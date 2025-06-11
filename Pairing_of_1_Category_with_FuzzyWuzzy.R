detach("package:fuzzywuzzyR", unload = TRUE)
reticulate::py_config()
library("fuzzywuzzyR")
check_availability()
init_scor = FuzzMatcher$new()


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Token Set Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Token_Set_Ratio_90.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Token_set_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    
    if (Employee_Name_Diff >= 90 & Mother_Name_Diff >= 90){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 90 & Mother_Name_Diff >= 90){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Partial Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Partial_Ratio_90.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Partial_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    if (is.na(database_Subset[k, "VNEVEM"]) == TRUE | is.na(database_Subset[k, "UNEVEM"]) == TRUE | is.na(database_Subset[k, "SZVNEVE"]) == TRUE | is.na(database_Subset[k, "SZUNEVE"]) == TRUE | is.na(database_Subset[k, "AVNEVE"]) == TRUE | is.na(database_Subset[k, "AUNEVE"]) == TRUE)
      next
    
    if (database_Subset[k, "VNEVEM"] == "" | database_Subset[k, "UNEVEM"] == "" | database_Subset[k, "SZVNEVE"] == "" | database_Subset[k, "SZUNEVE"] == "" | database_Subset[k, "AVNEVE"] == "" | database_Subset[k, "AUNEVE"] == "")
      next
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "))
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "))
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "))
    
    if (Employee_Name_Diff >= 90 & Mother_Name_Diff >= 90){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 90 & Mother_Name_Diff >= 90){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Token Set Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Token_Set_Ratio_85.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Token_set_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    
    if (Employee_Name_Diff >= 85 & Mother_Name_Diff >= 85){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 85 & Mother_Name_Diff >= 85){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Partial Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Partial_Ratio_85.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Partial_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    if (is.na(database_Subset[k, "VNEVEM"]) == TRUE | is.na(database_Subset[k, "UNEVEM"]) == TRUE | is.na(database_Subset[k, "SZVNEVE"]) == TRUE | is.na(database_Subset[k, "SZUNEVE"]) == TRUE | is.na(database_Subset[k, "AVNEVE"]) == TRUE | is.na(database_Subset[k, "AUNEVE"]) == TRUE)
      next
    
    if (database_Subset[k, "VNEVEM"] == "" | database_Subset[k, "UNEVEM"] == "" | database_Subset[k, "SZVNEVE"] == "" | database_Subset[k, "SZUNEVE"] == "" | database_Subset[k, "AVNEVE"] == "" | database_Subset[k, "AUNEVE"] == "")
      next
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "))
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "))
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "))
    
    if (Employee_Name_Diff >= 85 & Mother_Name_Diff >= 85){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 85 & Mother_Name_Diff >= 85){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Token Set Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Token_Set_Ratio_80.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Token_set_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), force_ascii = TRUE, full_process = TRUE)
    
    if (Employee_Name_Diff >= 80 & Mother_Name_Diff >= 80){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 80 & Mother_Name_Diff >= 80){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}
SILC[SILC$FIXSZ == "1808010001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2886610001", "Talalt"] <- 0


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Partial Ratio. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat_Partial_Ratio_80.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

SCOR = init_scor$Partial_ratio

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    if (is.na(database_Subset[k, "VNEVEM"]) == TRUE | is.na(database_Subset[k, "UNEVEM"]) == TRUE | is.na(database_Subset[k, "SZVNEVE"]) == TRUE | is.na(database_Subset[k, "SZUNEVE"]) == TRUE | is.na(database_Subset[k, "AVNEVE"]) == TRUE | is.na(database_Subset[k, "AUNEVE"]) == TRUE)
      next
    
    if (database_Subset[k, "VNEVEM"] == "" | database_Subset[k, "UNEVEM"] == "" | database_Subset[k, "SZVNEVE"] == "" | database_Subset[k, "SZUNEVE"] == "" | database_Subset[k, "AVNEVE"] == "" | database_Subset[k, "AUNEVE"] == "")
      next
    
    Employee_Name_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "))
    Employee_BirthName_Diff <- SCOR(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "))
    Mother_Name_Diff <- SCOR(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "))
    
    if (Employee_Name_Diff >= 80 & Mother_Name_Diff >= 80){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff >= 80 & Mother_Name_Diff >= 80){
      
      cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
                paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
                paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
                database_Subset[k, "AAJE"], 
                SILC[i, "ADOSZAM"], 
                database_Subset[k, "SZUL_DAT"], 
                SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
                SILC[i, "FEOR08"],
                SILC[i, "Kategoria"],
                sep = ";"), sep = "\n", file = filename, append = TRUE)
      SILC[i, "Talalt"] <- 1
      
    }
  }
}