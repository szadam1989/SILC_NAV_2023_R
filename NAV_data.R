library("ROracle")
library("stringr")

drv <- Oracle()
con <- dbConnect(drv, username = Sys.getenv("userid"), password = Sys.getenv("pwd"), dbname = "emerald.ksh.hu")

# A-Á
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where (upper(VNEVEM) like 'A%' or upper(VNEVEM) like 'Á%') order by upper(VNEVEM), upper(UNEVEM)")
database_A <- fetch(res)
dbClearResult(res)
dim(database_A) # 96352

# B
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'B%' order by upper(VNEVEM), upper(UNEVEM)")
database_B <- fetch(res)
dbClearResult(res)
dim(database_B) # 532284

# C
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'C%' order by upper(VNEVEM), upper(UNEVEM)")
database_C <- fetch(res)
dbClearResult(res)
dim(database_C) # 166400

# D
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'D%' order by upper(VNEVEM), upper(UNEVEM)")
database_D <- fetch(res)
dbClearResult(res)
dim(database_D) # 159265

# E-É
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where (upper(VNEVEM) like 'E%' or upper(VNEVEM) like 'É%') order by upper(VNEVEM), upper(UNEVEM)")
database_E <- fetch(res)
dbClearResult(res)
dim(database_E) # 55682

# F
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'F%' order by upper(VNEVEM), upper(UNEVEM)")
database_F <- fetch(res)
dbClearResult(res)
dim(database_F) # 213893

# G
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'G%' order by upper(VNEVEM), upper(UNEVEM)")
database_G <- fetch(res)
dbClearResult(res)
dim(database_G) # 209511

# H
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'H%' order by upper(VNEVEM), upper(UNEVEM)")
database_H <- fetch(res)
dbClearResult(res)
dim(database_H) # 294636

# I-Í
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where (upper(VNEVEM) like 'I%' or upper(VNEVEM) like 'Í%') order by upper(VNEVEM), upper(UNEVEM)")
database_I <- fetch(res)
dbClearResult(res)
dim(database_I) # 40961

# J
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'J%' order by upper(VNEVEM), upper(UNEVEM)")
database_J <- fetch(res)
dbClearResult(res)
dim(database_J) # 98547

# K
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'K%' order by upper(VNEVEM), upper(UNEVEM)")
database_K <- fetch(res)
dbClearResult(res)
dim(database_K) # 605066

# L
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'L%' order by upper(VNEVEM), upper(UNEVEM)")
database_L <- fetch(res)
dbClearResult(res)
dim(database_L) # 166070

# M
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'M%' order by upper(VNEVEM), upper(UNEVEM)")
database_M <- fetch(res)
dbClearResult(res)
dim(database_M) # 326759

# N
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'N%' order by upper(VNEVEM), upper(UNEVEM)")
database_N <- fetch(res)
dbClearResult(res)
dim(database_N) # 224915

# O-Ó-Ö-Ő
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where (upper(VNEVEM) like 'O%' or upper(VNEVEM) like 'Ó%' or upper(VNEVEM) like 'Ö%' or upper(VNEVEM) like 'Ő%') order by upper(VNEVEM), upper(UNEVEM)")
database_O <- fetch(res)
dbClearResult(res)
dim(database_O) # 83131

# P
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'P%' order by upper(VNEVEM), upper(UNEVEM)")
database_P <- fetch(res)
dbClearResult(res)
dim(database_P) # 275606 

# Q
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'Q%' order by upper(VNEVEM), upper(UNEVEM)")
database_Q <- fetch(res)
dbClearResult(res)
dim(database_Q) # 628

# R
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'R%' order by upper(VNEVEM), upper(UNEVEM)")
database_R <- fetch(res)
dbClearResult(res)
dim(database_R) # 137449

# S
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'S%' order by upper(VNEVEM), upper(UNEVEM)")
database_S <- fetch(res)
dbClearResult(res)
dim(database_S) # 600892

# T
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'T%' order by upper(VNEVEM), upper(UNEVEM)")
database_T <- fetch(res)
dbClearResult(res)
dim(database_T) # 282563

# U-Ú-Ü-Ű
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where (upper(VNEVEM) like 'U%' or upper(VNEVEM) like 'Ú%' or upper(VNEVEM) like 'Ü%' or upper(VNEVEM) like 'Ű%') order by upper(VNEVEM), upper(UNEVEM)")
database_U <- fetch(res)
dbClearResult(res)
dim(database_U) # 26587

# V
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'V%' order by upper(VNEVEM), upper(UNEVEM)")
database_V <- fetch(res)
dbClearResult(res)
dim(database_V) # 240079

# W
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'W%' order by upper(VNEVEM), upper(UNEVEM)")
database_W <- fetch(res)
dbClearResult(res)
dim(database_W) # 19002

# X
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'X%' order by upper(VNEVEM), upper(UNEVEM)")
database_X <- fetch(res)
dbClearResult(res)
dim(database_X) # 568

# Y
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'Y%' order by upper(VNEVEM), upper(UNEVEM)")
database_Y <- fetch(res)
dbClearResult(res)
dim(database_Y) # 2657

# Z
res <- dbSendQuery(con, "select distinct AAJE, upper(VNEVEM) VNEVEM, upper(UNEVEM) UNEVEM, upper(SZVNEVE) SZVNEVE, upper(SZUNEVE) SZUNEVE, upper(AVENVE) AVNEVE, upper(AUNEVE) AUNEVE, to_char(lg.lg_naptar_uj.szulido_adoazbol(AAJE), 'YYYY-MM-DD') SZUL_DAT from LG23.VNAA0_2326_230112_V00 where upper(VNEVEM) like 'Z%' order by upper(VNEVEM), upper(UNEVEM)")
database_Z <- fetch(res)
dbClearResult(res)
dim(database_Z) # 62055


dbDisconnect(con)

database <- rbind(database_A, database_B, database_C, database_D, database_E, database_F, database_G, database_H, database_I, database_J, 
                  database_K, database_L, database_M, database_N, database_O, database_P, database_Q,  database_R, database_S, database_T,
                  database_U, database_V, database_W, database_X, database_Y, database_Z)
dim(database)
# 4.921.576 sor és 8 oszlop

# Aktuális vezetéknév tisztítása
database$VNEVEM <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$VNEVEM)

database$VNEVEM <- gsub("Á", "A", database$VNEVEM)   
database$VNEVEM <- gsub("Ä", "A", database$VNEVEM) 
database$VNEVEM <- gsub("É", "E", database$VNEVEM)  
database$VNEVEM <- gsub("Ë", "E", database$VNEVEM) 
database$VNEVEM <- gsub("Í", "I", database$VNEVEM)  
database$VNEVEM <- gsub("Ó", "O", database$VNEVEM)  
database$VNEVEM <- gsub("Ö", "O", database$VNEVEM)  
database$VNEVEM <- gsub("Ő", "O", database$VNEVEM)  
database$VNEVEM <- gsub("Ú", "U", database$VNEVEM)  
database$VNEVEM <- gsub("Ü", "U", database$VNEVEM) 
database$VNEVEM <- gsub("Ű", "U", database$VNEVEM)

database$VNEVEM <- gsub("DR\\.\\s", "", database$VNEVEM)
database$VNEVEM <- gsub("DR\\.", "", database$VNEVEM)

database$VNEVEM <- gsub("\\.", "", database$VNEVEM)
database$VNEVEM <- gsub("-", "", database$VNEVEM)

database$VNEVEM <- gsub("DR\\s", "", database$VNEVEM)
database$VNEVEM <- gsub("^DR$", "", database$VNEVEM)
database$VNEVEM <- gsub("IFJ\\s", "", database$VNEVEM)

database$VNEVEM <- gsub("\\s+", " ", database$VNEVEM)
database$VNEVEM <- trimws(database$VNEVEM, which = "both")

gc()

# Aktuális keresztnév tisztítása
database$UNEVEM <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$UNEVEM)

database$UNEVEM <- gsub("Á", "A", database$UNEVEM)   
database$UNEVEM <- gsub("Ä", "A", database$UNEVEM) 
database$UNEVEM <- gsub("É", "E", database$UNEVEM)  
database$UNEVEM <- gsub("Ë", "E", database$UNEVEM) 
database$UNEVEM <- gsub("Í", "I", database$UNEVEM)  
database$UNEVEM <- gsub("Ó", "O", database$UNEVEM)  
database$UNEVEM <- gsub("Ö", "O", database$UNEVEM)  
database$UNEVEM <- gsub("Ő", "O", database$UNEVEM)  
database$UNEVEM <- gsub("Ú", "U", database$UNEVEM)  
database$UNEVEM <- gsub("Ü", "U", database$UNEVEM) 
database$UNEVEM <- gsub("Ű", "U", database$UNEVEM)

database$UNEVEM <- gsub("DR\\.\\s", "", database$UNEVEM)
database$UNEVEM <- gsub("DR\\.", "", database$UNEVEM)

database$UNEVEM <- gsub("\\.", "", database$UNEVEM)
database$UNEVEM <- gsub("-", "", database$UNEVEM)

database$UNEVEM <- gsub("DR\\s", "", database$UNEVEM)
database$UNEVEM <- gsub("IFJ\\s", "", database$UNEVEM)

database$UNEVEM <- gsub("\\s+", " ", database$UNEVEM)
database$UNEVEM <- trimws(database$UNEVEM, which = "both")

gc()

# Aktuális születési vezetéknév tisztítása
database$SZVNEVE <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$SZVNEVE)

database$SZVNEVE <- gsub("Á", "A", database$SZVNEVE)   
database$SZVNEVE <- gsub("Ä", "A", database$SZVNEVE) 
database$SZVNEVE <- gsub("É", "E", database$SZVNEVE) 
database$SZVNEVE <- gsub("Ë", "E", database$SZVNEVE) 
database$SZVNEVE <- gsub("Í", "I", database$SZVNEVE)  
database$SZVNEVE <- gsub("Ó", "O", database$SZVNEVE)  
database$SZVNEVE <- gsub("Ö", "O", database$SZVNEVE)  
database$SZVNEVE <- gsub("Ő", "O", database$SZVNEVE)  
database$SZVNEVE <- gsub("Ú", "U", database$SZVNEVE)  
database$SZVNEVE <- gsub("Ü", "U", database$SZVNEVE) 
database$SZVNEVE <- gsub("Ű", "U", database$SZVNEVE)

database$SZVNEVE <- gsub("DR\\.\\s", "", database$SZVNEVE)
database$SZVNEVE <- gsub("DR\\.", "", database$SZVNEVE)

database$SZVNEVE <- gsub("\\.", "", database$SZVNEVE)
database$SZVNEVE <- gsub("-", "", database$SZVNEVE)

database$SZVNEVE <- gsub("DR\\s", "", database$SZVNEVE)
database$SZVNEVE <- gsub("IFJ\\s", "", database$SZVNEVE)

database$SZVNEVE <- gsub("\\s+", " ", database$SZVNEVE)
database$SZVNEVE <- trimws(database$SZVNEVE, which = "both")

gc()

# Aktuális születési keresztnév tisztítása
database$SZUNEVE <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$SZUNEVE)

database$SZUNEVE <- gsub("Á", "A", database$SZUNEVE)   
database$SZUNEVE <- gsub("Ä", "A", database$SZUNEVE) 
database$SZUNEVE <- gsub("É", "E", database$SZUNEVE) 
database$SZUNEVE <- gsub("Ë", "E", database$SZUNEVE) 
database$SZUNEVE <- gsub("Í", "I", database$SZUNEVE)  
database$SZUNEVE <- gsub("Ó", "O", database$SZUNEVE)  
database$SZUNEVE <- gsub("Ö", "O", database$SZUNEVE)  
database$SZUNEVE <- gsub("Ő", "O", database$SZUNEVE)  
database$SZUNEVE <- gsub("Ú", "U", database$SZUNEVE)  
database$SZUNEVE <- gsub("Ü", "U", database$SZUNEVE) 
database$SZUNEVE <- gsub("Ű", "U", database$SZUNEVE)

database$SZUNEVE <- gsub("DR\\.\\s", "", database$SZUNEVE)
database$SZUNEVE <- gsub("DR\\.", "", database$SZUNEVE)

database$SZUNEVE <- gsub("\\.", "", database$SZUNEVE)
database$SZUNEVE <- gsub("-", "", database$SZUNEVE)

database$SZUNEVE <- gsub("DR\\s", "", database$SZUNEVE)
database$SZUNEVE <- gsub("IFJ\\s", "", database$SZUNEVE)

database$SZUNEVE <- gsub("\\s+", " ", database$SZUNEVE)
database$SZUNEVE <- trimws(database$SZUNEVE, which = "both")

gc()

# Anyja születési vezetéknevének tisztítása
database$AVNEVE <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$AVNEVE)

database$AVNEVE <- gsub("Á", "A", database$AVNEVE)   
database$AVNEVE <- gsub("Ä", "A", database$AVNEVE) 
database$AVNEVE <- gsub("É", "E", database$AVNEVE) 
database$AVNEVE <- gsub("Ë", "E", database$AVNEVE) 
database$AVNEVE <- gsub("Í", "I", database$AVNEVE)  
database$AVNEVE <- gsub("Ó", "O", database$AVNEVE)  
database$AVNEVE <- gsub("Ö", "O", database$AVNEVE)  
database$AVNEVE <- gsub("Ő", "O", database$AVNEVE)  
database$AVNEVE <- gsub("Ú", "U", database$AVNEVE)  
database$AVNEVE <- gsub("Ü", "U", database$AVNEVE) 
database$AVNEVE <- gsub("Ű", "U", database$AVNEVE)

database$AVNEVE <- gsub("DR\\.\\s", "", database$AVNEVE)
database$AVNEVE <- gsub("DR\\.", "", database$AVNEVE)

database$AVNEVE <- gsub("\\.", "", database$AVNEVE)
database$AVNEVE <- gsub("-", "", database$AVNEVE)

database$AVNEVE <- gsub("DR\\s", "", database$AVNEVE)
database$AVNEVE <- gsub("^DR$", "", database$AVNEVE)
database$AVNEVE <- gsub("IFJ\\s", "", database$AVNEVE)

database$AVNEVE <- gsub("\\s+", " ", database$AVNEVE)
database$AVNEVE <- trimws(database$AVNEVE, which = "both")

gc()

# Anyja születési keresztnevének tisztítása
database$AUNEVE <- gsub("0|1|2|3|4|5|6|7|8|9", "", database$AUNEVE)

database$AUNEVE <- gsub("Á", "A", database$AUNEVE)   
database$AUNEVE <- gsub("Ä", "A", database$AUNEVE) 
database$AUNEVE <- gsub("É", "E", database$AUNEVE)  
database$AUNEVE <- gsub("Ë", "E", database$AUNEVE) 
database$AUNEVE <- gsub("Í", "I", database$AUNEVE)  
database$AUNEVE <- gsub("Ó", "O", database$AUNEVE)  
database$AUNEVE <- gsub("Ö", "O", database$AUNEVE)  
database$AUNEVE <- gsub("Ő", "O", database$AUNEVE)  
database$AUNEVE <- gsub("Ú", "U", database$AUNEVE)  
database$AUNEVE <- gsub("Ü", "U", database$AUNEVE) 
database$AUNEVE <- gsub("Ű", "U", database$AUNEVE)

database$AUNEVE <- gsub("DR\\.\\s", "", database$AUNEVE)
database$AUNEVE <- gsub("DR\\.", "", database$AUNEVE)

database$AUNEVE <- gsub("\\.", "", database$AUNEVE)
database$AUNEVE <- gsub("-", "", database$AUNEVE)

database$AUNEVE <- gsub("DR\\s", "", database$AUNEVE)
database$AUNEVE <- gsub("IFJ\\s", "", database$AUNEVE)

database$AUNEVE <- gsub("\\s+", " ", database$AUNEVE)
database$AUNEVE <- trimws(database$AUNEVE, which = "both")

gc()

# Párok kereséséhez kezdőbetűk
NAV_Adozo_Kezdo_Betu <- str_sub(database$VNEVEM, 1, 1)
NAV_Adozo_Kezdo_Betu_Kereszt <- str_sub(database$UNEVEM, 1, 1)
NAV_Anyja_Szul_Neve_Kezdo_Betu <- str_sub(database$AVNEVE, 1, 1)
NAV_Anyja_Szul_Neve_Kezdo_Betu_Kereszt <- str_sub(database$AUNEVE, 1, 1)