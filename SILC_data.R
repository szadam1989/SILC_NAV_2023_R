library("haven")
library("stringr")
library("dplyr")

SILC <- read_sav("SPSS/alkalmazottak_2024.sav")

SILC <- data.frame(lapply(SILC, as.character))
dim(SILC) # 7630 sor és 9 oszlop
SILC$SZHO <- str_pad(SILC$SZHO, pad = "0", width = 2)
SILC$SZNAP <- str_pad(SILC$SZNAP, pad = "0", width = 2)
SILC$FEOR08 <- str_pad(SILC$FEOR08, pad = "0", width = 4)

#Személy nevének tisztítása
SILC$SZNEV_VIZSGALT <- toupper(SILC$SZNEV)

SILC$SZNEV_VIZSGALT <- gsub("0|1|2|3|4|5|6|7|8|9", "", SILC$SZNEV_VIZSGALT)

SILC$SZNEV_VIZSGALT <- gsub("Á", "A", SILC$SZNEV_VIZSGALT)   
SILC$SZNEV_VIZSGALT <- gsub("Ä", "A", SILC$SZNEV_VIZSGALT) 
SILC$SZNEV_VIZSGALT <- gsub("É", "E", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("É", "Ë", SILC$SZNEV_VIZSGALT) 
SILC$SZNEV_VIZSGALT <- gsub("Í", "I", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("Ó", "O", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("Ö", "O", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("Ő", "O", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("Ú", "U", SILC$SZNEV_VIZSGALT)  
SILC$SZNEV_VIZSGALT <- gsub("Ü", "U", SILC$SZNEV_VIZSGALT) 
SILC$SZNEV_VIZSGALT <- gsub("Ű", "U", SILC$SZNEV_VIZSGALT)

SILC$SZNEV_VIZSGALT <- gsub("DR\\.\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("DR\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IFJ\\.\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IFJ\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IF\\.\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IF\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("ID\\.\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("ID\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("OZV\\.\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("OZV\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("\\.", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("DR\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IFJ\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IF\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("IFJABB\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^ID\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("OZV\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("\\sIFJ", "", SILC$SZNEV_VIZSGALT)

SILC$SZNEV_VIZSGALT <- gsub("\\s-", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("-\\s", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("-", "", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("[()]", " ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("\\s+", " ", SILC$SZNEV_VIZSGALT)

SILC$SZNEV_VIZSGALT <- trimws(SILC$SZNEV_VIZSGALT, which = "both")

SILC$SZNEV_VIZSGALT <- gsub("^A\\s", "A\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^B\\s", "B\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^C\\s", "C\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^CS\\s", "CS\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^CZ\\s", "CZ\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^D\\s", "D\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^DZ\\s", "DZ\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^DZS\\s", "DZS\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^E\\s", "E\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^F\\s", "F\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^G\\s", "G\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^GY\\s", "GY\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^H\\s", "H\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^I\\s", "I\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^J\\s", "J\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^K\\s", "K\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^L\\s", "L\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^M\\s", "M\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^N\\s", "N\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^NY\\s", "NY\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^O\\s", "O\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^P\\s", "P\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^Q\\s", "Q\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^R\\s", "R\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^S\\s", "S\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^SZ\\s", "SZ\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^T\\s", "T\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^TY\\s", "TY\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^U\\s", "U\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^V\\s", "V\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^W\\s", "W\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^X\\s", "X\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^Y\\s", "Y\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^Z\\s", "Z\\. ", SILC$SZNEV_VIZSGALT)
SILC$SZNEV_VIZSGALT <- gsub("^ZS\\s", "ZS\\. ", SILC$SZNEV_VIZSGALT)

#Anyja nevének tisztítása
SILC$ANYNEV_VIZSGALT <- toupper(SILC$ANYNEV)

SILC$ANYNEV_VIZSGALT <- gsub("0|1|2|3|4|5|6|7|8|9", "", SILC$ANYNEV_VIZSGALT)

SILC$ANYNEV_VIZSGALT <- gsub("Á", "A", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ä", "A", SILC$ANYNEV_VIZSGALT) 
SILC$ANYNEV_VIZSGALT <- gsub("É", "E", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("É", "Ë", SILC$ANYNEV_VIZSGALT) 
SILC$ANYNEV_VIZSGALT <- gsub("Í", "I", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ó", "O", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ö", "O", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ő", "O", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ú", "U", SILC$ANYNEV_VIZSGALT)  
SILC$ANYNEV_VIZSGALT <- gsub("Ü", "U", SILC$ANYNEV_VIZSGALT) 
SILC$ANYNEV_VIZSGALT <- gsub("Ű", "U", SILC$ANYNEV_VIZSGALT)

SILC$ANYNEV_VIZSGALT <- gsub("DR\\.\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("DR\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IFJ\\.\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IFJ\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IF\\.\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IF\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("ID\\.\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("ID\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("OZV\\.\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("OZV\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("\\.", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("DR\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IFJ\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("IF\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^ID\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("OZV\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("\\sIFJ", "", SILC$ANYNEV_VIZSGALT)


SILC$ANYNEV_VIZSGALT <- gsub("\\s-", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("-\\s", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("-", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("-", "", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("[()]", " ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("\\s+", " ", SILC$ANYNEV_VIZSGALT)

SILC$ANYNEV_VIZSGALT <- trimws(SILC$ANYNEV_VIZSGALT, which = "both")

SILC$ANYNEV_VIZSGALT <- gsub("^A\\s", "A\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^B\\s", "B\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^C\\s", "C\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^CS\\s", "CS\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^CZ\\s", "CZ\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^D\\s", "D\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^DZ\\s", "DZ\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^DZS\\s", "DZS\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^E\\s", "E\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^F\\s", "F\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^G\\s", "G\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^GY\\s", "GY\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^H\\s", "H\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^I\\s", "I\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^J\\s", "J\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^K\\s", "K\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^L\\s", "L\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^M\\s", "M\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^N\\s", "N\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^NY\\s", "NY\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^O\\s", "O\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^P\\s", "P\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^Q\\s", "Q\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^R\\s", "R\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^S\\s", "S\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^SZ\\s", "SZ\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^T\\s", "T\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^TY\\s", "TY\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^U\\s", "U\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^V\\s", "V\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^W\\s", "W\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^X\\s", "X\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^Y\\s", "Y\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^Z\\s", "Z\\. ", SILC$ANYNEV_VIZSGALT)
SILC$ANYNEV_VIZSGALT <- gsub("^ZS\\s", "ZS\\. ", SILC$ANYNEV_VIZSGALT)

#Kategórizálás: 
SILC$Kategoria <- 1
SILC[grepl("\\.", SILC$SZNEV_VIZSGALT), "Kategoria"] <- 2 # Adózó vezetékneve csak monogram
SILC[grepl("\\.", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 3 # Adózó anyja születési vezetékneve csak monogram
SILC[grepl("\\.", SILC$SZNEV_VIZSGALT) & grepl("\\.", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 4 # Adózó és anyja születési vezetékneve csak monogram
SILC[!grepl("\\s", SILC$SZNEV_VIZSGALT), "Kategoria"] <- 5 # Adózónak csak keresztneve van
SILC[!grepl("\\s", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 6 # Adózó anyja születési neve csak keresztnév
SILC[!grepl("\\s", SILC$SZNEV_VIZSGALT) & !grepl("\\s", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 7 # Adózó és az anyja születési nevének is csak a keresztneve van meg
SILC[grepl("\\.", SILC$SZNEV_VIZSGALT) & !grepl("\\s", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 8 # Adózónak a vezetékneve csak monogram és Adózó anyja születési neve csak keresztnév
SILC[!grepl("\\s", SILC$SZNEV_VIZSGALT) & grepl("\\.", SILC$ANYNEV_VIZSGALT), "Kategoria"] <- 9 # Adózónak csak keresztneve van és Adózó anyja születési vezetékneve csak monogram

# Nagyon szépen tiszták a nevek:
nrow(SILC %>% filter(Kategoria == 1)) # 7335, 7336
nrow(SILC %>% filter(Kategoria == 2)) # 1, 0
nrow(SILC %>% filter(Kategoria == 3)) # 3
nrow(SILC %>% filter(Kategoria == 4)) # 1
nrow(SILC %>% filter(Kategoria == 5)) # 11
nrow(SILC %>% filter(Kategoria == 6)) # 266
nrow(SILC %>% filter(Kategoria == 7)) # 12
nrow(SILC %>% filter(Kategoria == 8)) # 1
nrow(SILC %>% filter(Kategoria == 9)) # 0

SILC$Talalt <- 0

SILC[SILC$Kategoria == 2, "FIXSZ"]
SILC[SILC$Kategoria == 2, "SZNEV_VIZSGALT"] # T. KERTESZ TUNDE
SILC[SILC$Kategoria == 2, "ANYNEV_VIZSGALT"] # TKERTESZ JANOSNE
SILC[SILC$FIXSZ == "6160210002", "SZNEV_VIZSGALT"] <- "KERTESZ TUNDE"
SILC[SILC$FIXSZ == "6160210002", "ANYNEV_VIZSGALT"] <- "KERTESZ JANOSNE"
SILC[SILC$FIXSZ == "6160210002", "Kategoria"] <- 1
SILC[SILC$FIXSZ == "6160210002", ]

# SILC[c(7260, 7261, 7281), ]
# SILC[SILC$FIXSZ == "1773810002", ]
# SILC[SILC$FIXSZ == "1650610002", ]
# SILC[SILC$FIXSZ == "1614410001", ]
# SILC[SILC$FIXSZ == "7248910002", ]
# SILC[SILC$FIXSZ == "771210002", ]
# SILC[SILC$FIXSZ == "2429110004", ]

# Még keresendő személyek száma kategóriánként:
nrow(SILC %>% filter(Kategoria == 1 & Talalt == 0)) # 3723, 3182, 2473, 2331, 2142, 2138, 2097, 2091, 2083, 2064, 2060, 2042, 2019, 2011  
nrow(SILC %>% filter(Kategoria == 2 & Talalt == 0)) # 0
nrow(SILC %>% filter(Kategoria == 3 & Talalt == 0)) # 1, 0
nrow(SILC %>% filter(Kategoria == 4 & Talalt == 0)) # 1
nrow(SILC %>% filter(Kategoria == 5 & Talalt == 0)) # 11
nrow(SILC %>% filter(Kategoria == 6 & Talalt == 0)) # 121, 95, 79, 75, 74
nrow(SILC %>% filter(Kategoria == 7 & Talalt == 0)) # 12
nrow(SILC %>% filter(Kategoria == 8 & Talalt == 0)) # 1
nrow(SILC %>% filter(Kategoria == 9 & Talalt == 0)) # 0

nrow(SILC %>% filter(Kategoria == 1 & Talalt == 1)) # 3613, 4154, 4863, 5005, 5194, 5198, 5239, 5245, 5253, 5272, 5276, 5294, 5317, 5325, 
nrow(SILC %>% filter(Kategoria == 2 & Talalt == 1)) # 0
nrow(SILC %>% filter(Kategoria == 3 & Talalt == 1)) # 2, 3
nrow(SILC %>% filter(Kategoria == 4 & Talalt == 1)) # 0
nrow(SILC %>% filter(Kategoria == 5 & Talalt == 1)) # 0
nrow(SILC %>% filter(Kategoria == 6 & Talalt == 1)) # 145, 171, 187, 191, 192
nrow(SILC %>% filter(Kategoria == 7 & Talalt == 1)) # 0
nrow(SILC %>% filter(Kategoria == 8 & Talalt == 1)) # 0
nrow(SILC %>% filter(Kategoria == 9 & Talalt == 1)) # 0

#SILC[SILC$Kategoria == 6, "Talalt"] <- 0
nrow(SILC %>% filter(Talalt == 1)) # 5445, 5448, 5467, 5471, 5489, 5512, 5520,     # 5207, 5234, 5254, 5256, 5397, 5399, 5404, 5407, 5618, 



SILC[str_sub(SILC$SZNEV_VIZSGALT, 1, 2) == "ID", ]
SILC[str_sub(SILC$ANYNEV_VIZSGALT, 1, 2) == "ID", ]

SILC[SILC$Kategoria == 3, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 3, "ANYNEV_VIZSGALT"]

SILC[SILC$Kategoria == 4, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 4, "ANYNEV_VIZSGALT"]

SILC[SILC$Kategoria == 5, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 5, "ANYNEV_VIZSGALT"]

SILC[SILC$Kategoria == 6, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 6, "ANYNEV_VIZSGALT"]

SILC[SILC$Kategoria == 7, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 7, "ANYNEV_VIZSGALT"]

SILC[SILC$Kategoria == 8, "SZNEV_VIZSGALT"]
SILC[SILC$Kategoria == 8, "ANYNEV_VIZSGALT"]

SILC$Talalt <- 0
firstData <- file(description = "Talalt_SILC_azonositok.txt", open = "r")
line <- readLines(con = firstData)

for (NotebooK_i in 1:length(line)){
  value <- line[NotebooK_i]
  SILC[SILC$FIXSZ == value, "Talalt"] <- 1
}

close(firstData)

# write.table(SILC[SILC$Talalt == 0, ], "Nem_találtam_meg_őket.txt", sep = ";", row.names=FALSE)

# SILC[SILC$FIXSZ == "6112310002", ]
# SILC[SILC$FIXSZ == "6185910001", ]
# SILC[SILC$FIXSZ == "7633710002", ]
# SILC[SILC$FIXSZ == "7753110002", ]

View(SILC[SILC$FIXSZ == "6441510001", c("SZNEV_VIZSGALT", "ANYNEV_VIZSGALT")])
View(database[database$AAJE == "8424093798", c("VNEVEM", "UNEVEM", "SZVNEVE", "SZUNEVE", "AVNEVE", "AUNEVE")])
# 
# database[database$AAJE == "8423370658", "SZVNEVE"]
# database[database$AAJE == "8343532147", c("VNEVEM", "UNEVEM", "SZVNEVE", "SZUNEVE", "AVNEVE", "AUNEVE")]
# database[database$AAJE == "8374942371", c("VNEVEM", "UNEVEM", "SZVNEVE", "SZUNEVE", "AVNEVE", "AUNEVE")]
# database[database$AAJE == "8389302578", c("VNEVEM", "UNEVEM", "SZVNEVE", "SZUNEVE", "AVNEVE", "AUNEVE")]


SILC[SILC$FIXSZ == "2769210002", c("SZNEV_VIZSGALT", "ANYNEV_VIZSGALT")]
database[database$AAJE == "8411371662", c("VNEVEM", "UNEVEM", "SZVNEVE", "SZUNEVE", "AVNEVE", "AUNEVE")]
# R-ben nem találom a fenti FIXSZ-t: 2769210002. Végül a Partial 85-ben meg lett.

# Python-ban nem találtam meg csak Token Sort ratio 85-ben: 23958100;2395810001;TOTH LASZLO;TELEKI KATALIN;TOTH LASZLO;TELKES KATALIN;8376690884;;1970-02-19;1970;02;19;2431;1

SILC[SILC$FIXSZ == "6201910003", ]
SILC[SILC$FIXSZ == "1123710002", ]


SILC[is.na(SILC$SZNAP) == TRUE, "SZNAP"] <- "00"
SILC[SILC$SZNAP == "00", ]
