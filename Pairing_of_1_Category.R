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
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-")
                            # & (
                            #     (str_sub(database$VNEVEM, 1, 1) == str_sub(SILC_Employee_Fullname[1], 1, 1) & str_sub(database$UNEVEM, 1, 1) == str_sub(SILC_Employee_Fullname[2], 1, 1)) |
                            #     (str_sub(database$SZVNEVE, 1, 1) == str_sub(SILC_Employee_Fullname[1], 1, 1) & str_sub(database$SZUNEVE, 1, 1) == str_sub(SILC_Employee_Fullname[2], 1, 1))
                            #   )
                            )
    
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


#Adózónak több neve van, mint kettő és anyja neve ugyanaz
filename <- "Talalt_Parok_1._kat_e.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  if(length(SILC_Employee_Fullname) <= 2)
    next

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
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
      SILC[i, "Talalt"] <- 1

    }
  }
}


#Adózó neve ugyanaz és az anyja neve több, mint kettő
filename <- "Talalt_Parok_1._kat_f.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  if(length(SILC_Mother_Fullname) <= 2)
    next

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")

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
      SILC[i, "Talalt"] <- 1

    }
  }
}


#Adózó és anyja neve is több, mint kettő
filename <- "Talalt_Parok_1._kat_g.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  if(length(SILC_Mother_Fullname) <= 2 | length(SILC_Employee_Fullname) <= 2)
    next

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")

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
      SILC[i, "Talalt"] <- 1

    }
  }
}


#Adózónak több neve van, mint három és anyja neve ugyanaz
filename <- "Talalt_Parok_1._kat_h.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  if(length(SILC_Employee_Fullname) <= 3)
    next

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
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
      SILC[i, "Talalt"] <- 1

    }
  }
}


#Adózó neve ugyanaz és anyja neve több, mint három
filename <- "Talalt_Parok_1._kat_i.txt"

if (file.exists(filename)){

  file.remove(filename)

}

for(i in 1:nrow(SILC)){

  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next

  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]

  if(length(SILC_Mother_Fullname) <= 3)
    next

  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))

  if(nrow(database_Subset) == 0)
    next

  for(k in 1:nrow(database_Subset)){

    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]

    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[3], SILC_Mother_Fullname[4], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")

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
      SILC[i, "Talalt"] <- 1

    }
  }
}


#Distance based on soundex encoding
filename <- "Talalt_Parok_1._kat_Soundex.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if (SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if (nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "soundex")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "soundex")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "soundex")
    
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
      SILC[i, "Talalt"] <- 1
      
    }
  }
}

SILC[SILC$FIXSZ == "1340110003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2315310003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2587510001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2886610001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6445310001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6445310003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6874010002", "Talalt"] <- 0

#Distance based on soundex encoding
filename <- "Talalt_Parok_1._kat_Soundex2.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if (SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0)
    next
  
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- subset(database, database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"))
  
  if (nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
    
    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "soundex")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "soundex")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "soundex")
    
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
      SILC[i, "Talalt"] <- 1
      
    }
  }
}


SILC[SILC$FIXSZ == "1340110003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2315310003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2587510001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "2886610001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6445310001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6445310003", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6874010002", "Talalt"] <- 0


# Születési nap hiányzik
SILC[is.na(SILC$SZNAP) == TRUE, ] # 15 alkalmazott

filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
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


filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik2.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
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


# Születési nap hiányzik
filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik3.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
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
      break
      
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
      break
      
    }
  }
}


filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik4.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
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
      break
      
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
      break
      
    }
  }
}


# Születési nap hiányzik
filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik5.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    Employee_Name_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), method = "soundex")
    Employee_BirthName_Diff <- stringdist(SILC[i, "SZNEV_VIZSGALT"], paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), method = "soundex")
    Mother_Name_Diff <- stringdist(SILC[i, "ANYNEV_VIZSGALT"], paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), method = "soundex")
    
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

SILC[SILC$FIXSZ == "2907410001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6435110001", "Talalt"] <- 0
# 29074100;2907410001;BERECZKI ZOLTAN;BERECZKI SANDORNE MARIA;BERCES MATE;BARCZA JULIANNA;8474981085;;1997-01-17;1997;01;NA;5112;1
# 64351100;6435110001;MAGYAROSSY SZILVIA;MAGYAROSSY ISTVANNE MARIA;MAJOROS AGNES;MAGYAR AGNES;8424200527;;1983-02-22;1983;02;NA;5113;1


filename <- "Talalt_Parok_1._kat_Szuletesi_nap_hianyzik6.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

for(i in 1:nrow(SILC)){
  
  if(SILC[i, "Kategoria"] != 1 | SILC[i, "Talalt"] != 0 | is.na(SILC[i, "SZNAP"]) == FALSE)
    next
  
  SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
  SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
  
  database_Subset <- subset(database, str_sub(database$SZUL_DAT, 1, 7) == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], sep = "-"))
  
  if(nrow(database_Subset) == 0)
    next
  
  for(k in 1:nrow(database_Subset)){
    
    FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
    BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
    MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
    
    Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "soundex")
    Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "soundex")
    Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "soundex")
    
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

SILC[SILC$FIXSZ == "2907410001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "6435110001", "Talalt"] <- 0



#Születési dátum nem egyezik
filename <- "Talalt_Parok_1._kat_SZULDAT_NEM0.txt"

if (file.exists(filename)){
  
  file.remove(filename)
  
}

# SILC[SILC$FIXSZ == "1145110001", ]
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
      SILC[i, "Talalt"] <- 1
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
      SILC[i, "Talalt"] <- 1
      
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
          SILC[i, "Talalt"] <- 1
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
          SILC[i, "Talalt"] <- 1
          
    }
  }
}

SILC[SILC$FIXSZ == "991510001", "Talalt"] <- 0
SILC[SILC$FIXSZ == "1015910001", "Talalt"] <- 0


# 
# #G kategóriánál hagytam abba
# #Adózó anyja neve és születési dátum megegyezik
# # filename <- "Talalt_Parok_1._kat_h.txt"
# # 
# # if (file.exists(filename)){
# #   
# #   file.remove(filename)
# #   
# # }
# # 
# # for(i in 1:nrow(SILC)){
# #   
# #   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
# #     
# #     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
# #     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
# #     
# #     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
# #     
# #     if(nrow(database_Subset) != 0){
# #       for(k in 1:nrow(database_Subset)){
# #         
# #         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
# #         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
# #         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
# #         
# #         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
# #         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
# #         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
# #         
# #         if (Mother_Name_Diff <= 0){
# #           
# #           if (Employee_Name_Diff <= Employee_BirthName_Diff){
# #             
# #             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
# #                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
# #                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
# #                       database_Subset[k, "AAJE"], 
# #                       SILC[i, "ADOSZAM"], 
# #                       database_Subset[k, "SZUL_DAT"], 
# #                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
# #                       SILC[i, "FEOR08"],
# #                       SILC[i, "Kategoria"],
# #                       sep = ";"), sep = "\n", file = filename, append = TRUE)
# #             
# #           }else{
# #             
# #             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
# #                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
# #                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
# #                       database_Subset[k, "AAJE"], 
# #                       SILC[i, "ADOSZAM"], 
# #                       database_Subset[k, "SZUL_DAT"], 
# #                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
# #                       SILC[i, "FEOR08"],
# #                       SILC[i, "Kategoria"],
# #                       sep = ";"), sep = "\n", file = filename, append = TRUE)
# #             
# #           }
# #           
# #           
# #        #   SILC[i, "Talalt"] <- 1
# #         }
# #       }
# #       
# #     }
# #   }
# # }
# 
# 
# 
# 
# 
# #Adózó anyja neve és születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_g2.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         #(Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & 
#         if (Mother_Name_Diff <= 2){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#       #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# #Adózó és anyja vezetékneve, továbbá születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_h.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         Employee_Name_Diff <- stringdist(SILC_Employee_Fullname[1], database_Subset[k, "VNEVEM"], method = "dl")
#         Employee_BirthName_Diff <- stringdist(SILC_Employee_Fullname[1], database_Subset[k, "SZVNEVE"], method = "dl")
#         Mother_Name_Diff <- stringdist(SILC_Mother_Fullname[1], database_Subset[k, "AVNEVE"], method = "dl")
#         # 
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & Mother_Name_Diff <= 0){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#         #  SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Adózó és anyja vezetékneve, továbbá születési dátum megegyezik, valamint adózónak több neve van
# filename <- "Talalt_Parok_1._kat_h2.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
#
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 ){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#     
#       database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           Employee_Name_Diff <- stringdist(SILC_Employee_Fullname[2], database_Subset[k, "VNEVEM"], method = "dl")
#           Employee_BirthName_Diff <- stringdist(SILC_Employee_Fullname[2], database_Subset[k, "SZVNEVE"], method = "dl")
#           Mother_Name_Diff <- stringdist(SILC_Mother_Fullname[1], database_Subset[k, "AVNEVE"], method = "dl")
#           # 
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & Mother_Name_Diff <= 0){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #  SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Csak a keresztnevek vizsgálata
# filename <- "Talalt_Parok_1._kat_i.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# #1465-nél és 1466-nál nincs születési dátum
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         if(length(FirstName) != 0 & length(BirthFirstName) != 0 & length(MotherFirstName) != 0){
#           
#           Employee_Name_Diff <- stringdist(SILC_Employee_Fullname[2], FirstName[1], method = "dl")
#           Employee_BirthName_Diff <- stringdist(SILC_Employee_Fullname[2], BirthFirstName[1], method = "dl")
#           Mother_Name_Diff <- stringdist(SILC_Mother_Fullname[2], MotherFirstName[1], method = "dl")
#           
#         }
#         
#         
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & Mother_Name_Diff <= 0){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Születési dátum nem egyezik
# filename <- "Talalt_Parok_1._kat_j.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     database_Subset <- database[(NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[1], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[2], start = 1, stop = 1)) & (NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[1], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[2], start = 1, stop = 1)), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & Mother_Name_Diff <= 0){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#       #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# #Születési dátum nem egyezik és a nevekben legfeljebb 2-2 eltérés van
# filename <- "Talalt_Parok_1._kat_j2.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     database_Subset <- database[(NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[1], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[2], start = 1, stop = 1)) & (NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[1], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[2], start = 1, stop = 1)), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) & Mother_Name_Diff <= 2){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# #Adózó neve és születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_k.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) ){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#        #   SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Adózó neve és születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_k2.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#       
#       database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) ){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #   SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# #Adózó neve és születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_k3.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) ){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #   SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# #Adózó neve és születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_k4.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & is.na(SILC[i, "SZNAP"]) == FALSE){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 3){
#       
#       database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
# 
#           if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) ){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #   SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] & database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] & database$SZUNEVE == SILC_Employee_Fullname[2]), ]
# 
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#        
#         if (Mother_Name_Diff <= 2){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l2.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
# 
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Mother_Fullname) > 2){
#       
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] & database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] & database$SZUNEVE == SILC_Employee_Fullname[2]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#   
#           if (Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l3.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Mother_Fullname) > 3){
#       
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] & database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] & database$SZUNEVE == SILC_Employee_Fullname[2]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[3], SILC_Mother_Fullname[4], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if (Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l4.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 1){
#       #
#       #(database$VNEVEM == SILC_Employee_Fullname[1] | database$SZVNEVE == SILC_Employee_Fullname[1]) & (database$UNEVEM == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[2])
#       database_Subset <- database[NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[1], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[2], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[1], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[2], start = 1, stop = 1), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if (Employee_Name_Diff <= 2 & Employee_BirthName_Diff <= 2 & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l5.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 1 & length(SILC_Mother_Fullname) > 2){
#       #
#       #(database$VNEVEM == SILC_Employee_Fullname[1] | database$SZVNEVE == SILC_Employee_Fullname[1]) & (database$UNEVEM == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[2])
#       database_Subset <- database[NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[1], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[2], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[2], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[3], start = 1, stop = 1), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if (Employee_Name_Diff <= 2 & Employee_BirthName_Diff <= 2 & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l6.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 3 & length(SILC_Mother_Fullname) > 1){
#       #
#       #(database$VNEVEM == SILC_Employee_Fullname[1] | database$SZVNEVE == SILC_Employee_Fullname[1]) & (database$UNEVEM == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[2])
#       database_Subset <- database[NAV_Adozo_Kezdo_Betu == substr(SILC_Employee_Fullname[3], start = 1, stop = 1) & NAV_Adozo_Kezdo_Betu_Kereszt == substr(SILC_Employee_Fullname[4], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu == substr(SILC_Mother_Fullname[1], start = 1, stop = 1) & NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt == substr(SILC_Mother_Fullname[2], start = 1, stop = 1), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if (Employee_Name_Diff <= 2 & Employee_BirthName_Diff <= 2 & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l7.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# #1465-nél és 1466-nál nincs születési dátum
# #SILC[c(1465, 1466), ]
# for(i in 317:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] | database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] | database$SZUNEVE == SILC_Employee_Fullname[2]), ]
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         
#         if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) & Mother_Name_Diff <= 2){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l8.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# #1465-nél és 1466-nál nincs születési dátum
# #SILC[c(1465, 1466), ]
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#     
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[2] | database$UNEVEM == SILC_Employee_Fullname[3]) | (database$SZVNEVE == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[3]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           
#           if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l9.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# #1465-nél és 1466-nál nincs születési dátum
# #SILC[c(1465, 1466), ]
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2 & length(SILC_Mother_Fullname) > 2){
#       
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[2] | database$UNEVEM == SILC_Employee_Fullname[3]) | (database$SZVNEVE == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[3]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           
#           if ((Employee_Name_Diff <= 2 | Employee_BirthName_Diff <= 2) & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l10.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# #SILC[c(1465, 1466), ]
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] | database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] | database$SZUNEVE == SILC_Employee_Fullname[2]), ]
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         
#         #& Mother_Name_Diff <= 2
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) ){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #    SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l11.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# #1465-nél és 1466-nál nincs születési dátum
# #SILC[c(1465, 1466), ]
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#     
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[2] | database$UNEVEM == SILC_Employee_Fullname[3]) | (database$SZVNEVE == SILC_Employee_Fullname[2] | database$SZUNEVE == SILC_Employee_Fullname[3]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           #& Mother_Name_Diff <= 2
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) ){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# #Csak az anyja nevének vizsgálata
# filename <- "Talalt_Parok_1._kat_l12.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# #1465-nél és 1466-nál nincs születési dátum
# #SILC[c(1465, 1466), ]
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 3){
#       
#       database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[3] | database$UNEVEM == SILC_Employee_Fullname[4]) | (database$SZVNEVE == SILC_Employee_Fullname[3] | database$SZUNEVE == SILC_Employee_Fullname[4]), ]
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[3], SILC_Employee_Fullname[4], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           #& Mother_Name_Diff <= 2
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) ){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#     }
#   }
# }
# 
# 
# 
# 
# 
# View(SILC[SILC$Kategoria == 1 & SILC$Talalt == 0, ])
# 
# 
# SILC[SILC$FIXSZ == "7449210003" | SILC$FIXSZ == "7449210003" | SILC$FIXSZ == "8476710002" | SILC$FIXSZ == "7547310001" | 
# SILC$FIXSZ == "9789410003" | SILC$FIXSZ == "9121210001" | SILC$FIXSZ == "0396510003" | SILC$FIXSZ == "9380210003" | 
# SILC$FIXSZ == "0504010002" | SILC$FIXSZ == "9044310001" | SILC$FIXSZ == "0066610002" | SILC$FIXSZ == "9090410001" | 
# SILC$FIXSZ == "9653310002" | SILC$FIXSZ == "0464010001" | SILC$FIXSZ == "9654910001" | SILC$FIXSZ == "9726810001" | 
# SILC$FIXSZ == "9737910001" | SILC$FIXSZ == "7540710017" | SILC$FIXSZ == "9770910003" | SILC$FIXSZ == "7131910001" | 
# SILC$FIXSZ == "8589510001" | SILC$FIXSZ == "7164710001" | SILC$FIXSZ == "0440610002" | SILC$FIXSZ == "6804910002" | 
# SILC$FIXSZ == "8486710002" | SILC$FIXSZ == "0465110002" | SILC$FIXSZ == "0536710001" | SILC$FIXSZ == "8187710001" | 
# SILC$FIXSZ == "7482710002" | SILC$FIXSZ == "9558810002" | SILC$FIXSZ == "9682210001" | SILC$FIXSZ == "9893510001" | 
# SILC$FIXSZ == "7362210003" | SILC$FIXSZ == "9631510001" | SILC$FIXSZ == "0079210002" | SILC$FIXSZ == "0602910002" | 
# SILC$FIXSZ == "9221510011" | SILC$FIXSZ == "7367810001" | SILC$FIXSZ == "9833310002" | SILC$FIXSZ == "7358210003" | 
# SILC$FIXSZ == "7560110002" | SILC$FIXSZ == "0034110001" | SILC$FIXSZ == "9318810002" | SILC$FIXSZ == "7342010002" | 
# SILC$FIXSZ == "7505110002" | SILC$FIXSZ == "6881410002" | SILC$FIXSZ == "8326110006" | SILC$FIXSZ == "7076210003" | 
# SILC$FIXSZ == "0604010003" | SILC$FIXSZ == "7092710003", ]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #adózó vezeték vagy keresztneve ugyanaz és az anyja neve több, mint kettő
# filename <- "Talalt_Parok_1._kat_k.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# 
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     
#     
#     if(length(SILC_Mother_Fullname) > 2){
#       
#       database_Subset <- database[database$VNEVEM == SILC_Employee_Fullname[1] | database$UNEVEM == SILC_Employee_Fullname[2], ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[2], SILC_Mother_Fullname[3], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if (Mother_Name_Diff <= 0){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Adózónak több neve van, mint kettő és a születési dátum nem egyezik
# filename <- "Talalt_Parok_1._kat_m.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# 
# for(i in 1:nrow(SILC)){
#   # & i != 1465 & i != 1466
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & i != 1465 & i != 1466){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     #database$VNEVEM == SILC_Employee_Fullname[2] & database$UNEVEM == SILC_Employee_Fullname[3]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#       
#       database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#           Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[2], SILC_Employee_Fullname[3], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           # & Mother_Name_Diff <= 5
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0)){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #    SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# #Adózó és anyja neve is megegyezik, továbbá a születési dátum is
# filename <- "Talalt_Parok_1._kat_m.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# #1465-nél és 1466-nál nincs születési dátum
# for(i in 3400:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & i != 1465 & i != 1466){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     database_Subset <- database[(database$VNEVEM == SILC_Employee_Fullname[1] | database$UNEVEM == SILC_Employee_Fullname[2]) | (database$SZVNEVE == SILC_Employee_Fullname[1] | database$SZUNEVE == SILC_Employee_Fullname[2]), ]
#     
#     
#     if(nrow(database_Subset) != 0){
#       for(k in 1:nrow(database_Subset)){
#         
#         FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#         BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#         MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#         
#         Employee_Name_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "VNEVEM"], FirstName[1], sep = " "), method = "dl")
#         Employee_BirthName_Diff <- stringdist(paste(SILC_Employee_Fullname[1], SILC_Employee_Fullname[2], sep = " "), paste(database_Subset[k, "SZVNEVE"], BirthFirstName[1], sep = " "), method = "dl")
#         Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#         #& Mother_Name_Diff <= 0
#         if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0)  & stringdist(database_Subset[k, "SZUL_DAT"], paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), method = "dl") <= 2){
#           
#           if (Employee_Name_Diff <= Employee_BirthName_Diff){
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }else{
#             
#             cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                       paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                       paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                       database_Subset[k, "AAJE"], 
#                       SILC[i, "ADOSZAM"], 
#                       database_Subset[k, "SZUL_DAT"], 
#                       SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                       SILC[i, "FEOR08"],
#                       SILC[i, "Kategoria"],
#                       sep = ";"), sep = "\n", file = filename, append = TRUE)
#             
#           }
#           
#           
#           #     SILC[i, "Talalt"] <- 1
#         }
#       }
#       
#     }
#   }
# }
# 
# 
# 
# 
# 
# #születési dátum megegyezik
# filename <- "Talalt_Parok_1._kat_n.txt"
# 
# if (file.exists(filename)){
#   file.remove(filename)
# }
# 
# 
# library("stringdist")
# for(i in 1:nrow(SILC)){
#   
#   if(SILC[i, "Kategoria"] == 1 & SILC[i, "Talalt"] == 0 & i != 841 & i != 5362 & i != 5363 & i != 6260){
#     
#     SILC_Employee_Fullname <- strsplit(SILC[i, "SZNEV_VIZSGALT"], "\\s")[[1]]
#     SILC_Mother_Fullname <- strsplit(SILC[i, "ANYNEV_VIZSGALT"], "\\s")[[1]]
#     
#     if(length(SILC_Employee_Fullname) > 2){
#       
#       database_Subset <- database[database$SZUL_DAT == paste(SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], sep = "-"), ]
#       
#       
#       if(nrow(database_Subset) != 0){
#         for(k in 1:nrow(database_Subset)){
#           
#           FirstName <- strsplit(database_Subset[k, "UNEVEM"], "\\s")[[1]]
#           BirthFirstName <- strsplit(database_Subset[k, "SZUNEVE"], "\\s")[[1]]
#           MotherFirstName <- strsplit(database_Subset[k, "AUNEVE"], "\\s")[[1]]
#           
#           Employee_Name_Diff <- stringdist(SILC_Employee_Fullname[3], FirstName[1], method = "dl")
#           Employee_BirthName_Diff <- stringdist(SILC_Employee_Fullname[3], BirthFirstName[1], method = "dl")
#           Mother_Name_Diff <- stringdist(paste(SILC_Mother_Fullname[1], SILC_Mother_Fullname[2], sep = " "), paste(database_Subset[k, "AVNEVE"], MotherFirstName[1], sep = " "), method = "dl")
#           
#           if ((Employee_Name_Diff <= 0 | Employee_BirthName_Diff <= 0) & Mother_Name_Diff <= 2){
#             
#             if (Employee_Name_Diff <= Employee_BirthName_Diff){
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "VNEVEM"], database_Subset[k, "UNEVEM"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }else{
#               
#               cat(paste(SILC[i, "HAZTART"], SILC[i, "FIXSZ"], SILC[i, "SZNEV_VIZSGALT"], SILC[i, "ANYNEV_VIZSGALT"], 
#                         paste(database_Subset[k, "SZVNEVE"], database_Subset[k, "SZUNEVE"], sep = " "), 
#                         paste(database_Subset[k, "AVNEVE"], database_Subset[k, "AUNEVE"], sep = " "), 
#                         database_Subset[k, "AAJE"], 
#                         SILC[i, "ADOSZAM"], 
#                         database_Subset[k, "SZUL_DAT"], 
#                         SILC[i, "SZEV"], SILC[i, "SZHO"], SILC[i, "SZNAP"], 
#                         SILC[i, "FEOR08"],
#                         SILC[i, "Kategoria"],
#                         sep = ";"), sep = "\n", file = filename, append = TRUE)
#               
#             }
#             
#             
#             #   SILC[i, "Talalt"] <- 1
#           }
#         }
#       }
#       }
#   }
# }
# 
# 
# 
# View(SILC[SILC$Kategoria == 1 & SILC$Talalt == 0, ])
