library("stringdist")
library("stringr")
library("data.table")

#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Full Damerau-Levenshtein distance. Teljes nevek vizsgálata.
filename <- "Talalt_Parok_1._kat.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "dl")
    
    if (Employee_Name_Diff == 0 & Mother_Name_Diff == 0){
      
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
      break
      
    }
    
    if (Employee_BirthName_Diff == 0 & Mother_Name_Diff == 0){
      
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
      break
      
    }
  }
}


#Adózó és anyja neve is megegyezik, továbbá a születési dátum is. Full Damerau-Levenshtein distance. Csak első két név vizsgálata.
filename <- "Talalt_Parok_1._kat_b.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
    
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
    
  if(nrow(database_Subset) == 0)
    next
      
  for(k in 1:nrow(database_Subset)){
        
    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
        
    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
    
    if (Employee_Name_Diff == 0 & Mother_Name_Diff == 0){
      
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
      break
      
    }
    
    if (Employee_BirthName_Diff == 0 & Mother_Name_Diff == 0){
            
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
      break
            
    }
  }
}


#Nevekben legfeljebb két eltérés lehet. Teljes nevek
filename <- "Talalt_Parok_1._kat_c.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "dl")
    
    if (Employee_Name_Diff <= 2 & Mother_Name_Diff <= 2){
      
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
    
    if (Employee_BirthName_Diff <= 2 & Mother_Name_Diff <= 2){
      
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


#Nevekben legfeljebb két eltérés lehet. Csak első két név.
filename <- "Talalt_Parok_1._kat_d.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")

    if (Employee_Name_Diff <= 2 & Mother_Name_Diff <= 2){

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

    if (Employee_BirthName_Diff <= 2 & Mother_Name_Diff <= 2){

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


# #Distance based on soundex encoding
# filename <- "Talalt_Parok_1._kat_Soundex.txt"
# 
# if (file.exists(filename)){
#   
#   file.remove(filename)
#   
# }
# 
# for(i in 1:nrow(SILC)){
#   
#   if (SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
#     next
#   
#   database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
#   
#   if (nrow(database_Subset) == 0)
#     next
#   
#   for(k in 1:nrow(database_Subset)){
#     
#     Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "soundex")
#     Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "soundex")
#     Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "soundex")
#     
#     if (Employee_Name_Diff == 0 & Mother_Name_Diff == 0){
#       
#       cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                 paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                 paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                 database_Subset[k, "AAJE"], 
#                 SILC[i, "ADOSZAM"], 
#                 database_Subset[k, "SZUL_DAT"], 
#                 SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                 SILC[i, "FEOR08"],
#                 SILC[i, "Kategoria"],
#                 sep = ";"), sep = "\n", file = filename, append = TRUE)
#       #SILC[i, "Talalt"] <- 1
#       next
#       
#     }
#     
#     if (Employee_BirthName_Diff == 0 & Mother_Name_Diff == 0){
#       
#       cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                 paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                 paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                 database_Subset[k, "AAJE"], 
#                 SILC[i, "ADOSZAM"], 
#                 database_Subset[k, "SZUL_DAT"], 
#                 SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                 SILC[i, "FEOR08"],
#                 SILC[i, "Kategoria"],
#                 sep = ";"), sep = "\n", file = filename, append = TRUE)
#       #SILC[i, "Talalt"] <- 1
#       
#     }
#   }
# }


#Születési dátum nem egyezik
filename <- "Talalt_Parok_1._kat_SZULDAT_NEM0.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  print(i)
  if (SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  if (is.na(SILC[i, "SZNAP"]) == TRUE)
    next
  
  database_Subset <- database[(NAV_Adozo_Kezdo_Betu == substr(SILC[i, "SZNEV_VIZSGALT"], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC[i, "ANYNEV_VIZSGALT"], start = 1, stop = 1)), ]
  
  if (nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    if (is.na(database_Subset[k, "SZUL_DAT"]) == TRUE)
      next

    if (str_sub(database_Subset[k, "SZUL_DAT"], 1, 4) != SILC[i, "SZEV"] & str_sub(database_Subset[k, "SZUL_DAT"], 6, 7) != SILC[i, "SZHO"] & str_sub(database_Subset[k, "SZUL_DAT"], 9, 10) !=  SILC[i, "SZNAP"])
      next
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "dl")
    
    if (Employee_Name_Diff == 0 & Mother_Name_Diff == 0){
      
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
      #SILC[i, "Talalt"] <- 1
      next
      
    }
    
    if (Employee_BirthName_Diff == 0 & Mother_Name_Diff == 0){
      
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
      #SILC[i, "Talalt"] <- 1
      
    }
  }
}

SILC[SILC$FIXSZ == "991510001", "Talalt"] <- 0



#Születési dátum nem egyezik
filename <- "Talalt_Parok_1._kat_SZULDAT_NEM0_2.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}


for(i in 1:nrow(SILC)){

  if (SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  if (is.na(SILC[i, "SZNAP"]) == TRUE)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- database[(NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[1], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[2], start = 1, stop = 1)) & (NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[1], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[2], start = 1, stop = 1)), ]

  if (nrow(database_Subset) == 0)
      next
    
  for(k in 1:nrow(database_Subset)){
    
    if (is.na(database_Subset[k, "SZUL_DAT"]) == TRUE)
      next
    
    if (str_sub(database_Subset[k, "SZUL_DAT"], 1, 4) != SILC[i, "SZEV"] & str_sub(database_Subset[k, "SZUL_DAT"], 6, 7) != SILC[i, "SZHO"] & str_sub(database_Subset[k, "SZUL_DAT"], 9, 10) !=  SILC[i, "SZNAP"])
      next

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")

    if (Employee_Name_Diff == 0 & Mother_Name_Diff == 0){
          
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
          #SILC[i, "Talalt"] <- 1
          next
          
        }
        
    if (Employee_BirthName_Diff == 0 & Mother_Name_Diff == 0){
          
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
          #SILC[i, "Talalt"] <- 1
          
    }
  }
}

SILC[SILC$FIXSZ == "991510001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "1015910001", "Talalt"] <- 0
