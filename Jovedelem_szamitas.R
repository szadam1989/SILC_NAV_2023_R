library("data.table")
library("RODBC")
library("stringr")

channelOracle <- odbcDriverConnect(paste("DRIVER={Oracle in OraClient18Home1};DBQ=EMERALD.KSH.HU;UID=", Sys.getenv("userid"), ";PWD=", Sys.getenv("pwd")))

SILC_FEOR2Jegy <- SILC[order(SILC$FEOR08), ][SILC$FEOR08 != "" | is.na(SILC$FEOR08) == FALSE, ]
dim(SILC_FEOR2Jegy) # 7630 sor és 13 oszlop
unique(substr(SILC_FEOR2Jegy$FEOR08, 1, 2)) # 42 lekérdezés csak, mert "99" az ismeretlen

Income <- data.frame()
elozoFEORKetjegy <- 0

for(i in 1:nrow(SILC_FEOR2Jegy)){
  
  if (substr(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2) == "99")
    break
  
  if(elozoFEORKetjegy != substr(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2)){
      
    Income_SzulEvHo <- sqlQuery(channelOracle, paste0("select lg.lg_munkaero.dekodol(AAJE) AAJE, LGAA029, to_char(lg.lg_naptar_uj.szulido_adoazbol(lg.lg_munkaero.dekodol(AAJE)), 'YYYY-MM') SZUL_IDO, ML05, nvl(LGAA517, 0) LGAA517, nvl(LGAA520, 0) LGAA520 from LG23.LGAA0_230112_P_V01 where ML62 like '1%' and substr(LGAA029, 1, 2) = '", substr(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2), "'"), as.is = c(AAJE = TRUE, LGAA029 = TRUE, SZUL_IDO = TRUE, ML05 = TRUE, LGAA517 = FALSE, LGAA520 = FALSE))
      
    elozoFEORKetjegy <- substr(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2)
    print(elozoFEORKetjegy)
    
    gc()
      
  }

  database_SzulEvHo_FILTER <- as.data.table(subset(Income_SzulEvHo, Income_SzulEvHo$SZUL_IDO == paste(SILC_FEOR2Jegy[i, "SZEV"], SILC_FEOR2Jegy[i, "SZHO"], sep = "-") & Income_SzulEvHo$ML05 == SILC_FEOR2Jegy[i, "NEME"] & Income_SzulEvHo$LGAA517 != 0 & Income_SzulEvHo$LGAA520 != 0))
  database_SzulEvHo_FILTER <- database_SzulEvHo_FILTER[, .(SUMLGAA517 = sum(LGAA517), SUMLGAA520 = sum(LGAA520)), by = "AAJE,LGAA029,SZUL_IDO,ML05"]
    
  if(nrow(DT) != 0){
      
    database_WITH_SILC_AZON_by_FEOR <- as.data.frame(cbind(SILC_FEOR2Jegy[i, "FIXSZ"], SILC_FEOR2Jegy[i, "SZNEV_VIZSGALT"], SILC_FEOR2Jegy[i, "ANYNEV_VIZSGALT"], SILC_FEOR2Jegy[i, "FEOR08"], SILC_FEOR2Jegy[i, "NEME"], database_SzulEvHo_FILTER))
    Income <- rbind(Income, database_WITH_SILC_AZON_by_FEOR)
      
  }
    
}

odbcClose(channelOracle)
dim(Income) #1386765, 13 sor és 11 oszlop
Income_biztonsagi_masolat <- Income

#Azonosito2_as_data_frame
Income <- Income[is.na(Income$AAJE) == FALSE, ]

Incomes_All <- Income[, 6:11]
Incomes_All <- unique(Incomes_All)
dim(Incomes_All) # 1073676, 13 sor és 6 oszlop
Incomes_All <- Incomes_All[Incomes_All$SUMLGAA517 > 200000, ]
Incomes_All$LGAA029_2jegy <- str_sub(Incomes_All$LGAA029, start = 1, end = 2)

mean1 <- as.data.table(Incomes_All)
mean1 <- as.data.frame(mean1[, .(LGAA517 = mean(SUMLGAA517), LGAA520 = mean(SUMLGAA520)), by = "SZUL_IDO,ML05,LGAA029_2jegy"])
dim(mean1)#5274
for(i in 1:nrow(mean1)){
  
  cat(paste(mean1[i, 1], mean1[i, 2], mean1[i, 3], mean1[i, 4], mean1[i, 5], mean1[i, 6], sep = ";"), sep = "\n", file = "Számtani_közép_első_verzió.txt", append = TRUE)
  
}

mean2 <- as.data.table(Incomes_All)
View(mean2 %>% count(AAJE, LGAA029))


database_Howmany_COPY <- database_Howmany
dim(database_Howmany) #1037337
colnames(database_Howmany) <- c("AAJE", "LGAA029", "COUNTAAJE")
database_Howmany_szukebb <- database_Howmany[database_Howmany$COUNTAAJE >= 12, ]
dim(database_Howmany_szukebb) #660022
colnames(Azonosito2_Shorter)
dim(Azonosito2_Shorter)#991396
Azonosito2_Shorter <- Azonosito2_Shorter[is.na(Azonosito2_Shorter$AAJE) == FALSE, ]
Azonosito2_Shorter$Torolheto <- 0
#5231

for(i in 114611:nrow(database_Howmany_szukebb)){
  Azonosito2_Shorter[Azonosito2_Shorter$AAJE == database_Howmany_szukebb[i, 1] & Azonosito2_Shorter$LGAA029 == database_Howmany_szukebb[i, 2], "Torolheto"] <- 1
}

sum(Azonosito2_Shorter$Torolheto)#659946

Azonosito2_Shorter_Longer <- Azonosito2_Shorter[Azonosito2_Shorter$Torolheto == 1, ]
nrow(Azonosito2_Shorter_Longer)#659946

head(Azonosito2_Shorter_Longer, 6)
DT_Azon2_mean <- data.table(Azonosito2_Shorter_Longer)

DT_Azon2_mean <- DT_Azon2_mean[, .(LGAA517=mean(SUMLGAA517), LGAA520=mean(SUMLGAA520)), by = "ML05,SZUL_IDO,LGAA029_2jegy"]

mean2_as_data_frame <- as.data.frame(DT_Azon2_mean)

for(i in 1:nrow(mean2_as_data_frame)){
  cat(paste(mean2_as_data_frame[i, 1], mean2_as_data_frame[i, 2], mean2_as_data_frame[i, 3], mean2_as_data_frame[i, 4], mean2_as_data_frame[i, 5], sep = ";"), sep = "\n", file = "Számtani_közép_JOBB.txt", append = TRUE)
  
}