library("stringdist")
library("stringr")
library("data.table")

#Adózó és anyja neve is megegyezik, továbbá a születési dátum is
filename <- "Talalt_Parok_2._kat.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if (SILC[i, "Kategoria"] != 2 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if (nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "dl")
    
    if (Mother_Name_Diff == 0){
      
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


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Csak első két név vizsgálata.
filename <- "Talalt_Parok_2._kat_b.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if (SILC[i, "Kategoria"] != 2 | SILC[i, "Talalt"] != 0)
    next
  
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- SILC[i, "ANYNEV_VIZSGALT"]
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if (nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
    
    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(SILC_Mother_Fullname, MotherFirstName[1], method = "dl")
    
    if (Mother_Name_Diff == 0){
      
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


View(SILC[SILC$Kategoria == 2 & SILC$Talalt == 0, ])
