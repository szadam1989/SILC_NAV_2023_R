library("ROracle")
library("stringr")
library("data.table")
library("dplyr")

drv <- Oracle()
con <- dbConnect(drv, username = Sys.getenv("userid"), password = Sys.getenv("pwd"), dbname = "emerald.ksh.hu")

SILC_FEOR2Jegy <- SILC[order(SILC$FEOR08), ][SILC$FEOR08 != "" | is.na(SILC$FEOR08) == FALSE, ]
dim(SILC_FEOR2Jegy) # 7630 sor és 13 oszlop
unique(str_sub(SILC_FEOR2Jegy$FEOR08, 1, 2)) # 42 lekérdezés csak, mert "99" az ismeretlen

Income <- data.frame()
At_least_12_months <- data.frame()
elozoFEORKetjegy <- "0"
for(i in 1:nrow(SILC_FEOR2Jegy)){
  
  if (str_sub(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2) == "99")
    break
  
  if(elozoFEORKetjegy != str_sub(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2)){
    
    res <- dbSendQuery(con, paste0("select lg.lg_munkaero.dekodol(AAJE) AAJE, LGAA029, to_char(lg.lg_naptar_uj.szulido_adoazbol(lg.lg_munkaero.dekodol(AAJE)), 'YYYY-MM') SZUL_IDO, ML05, nvl(LGAA517, 0) LGAA517, nvl(LGAA520, 0) LGAA520 from LG23.LGAA0_230112_P_V01 where ML62 like '1%' and substr(LGAA029, 1, 2) = '", str_sub(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2), "'"))
    Income_SzulEvHo <- fetch(res)
    dbClearResult(res)
    
    elozoFEORKetjegy <- str_sub(SILC_FEOR2Jegy[i, "FEOR08"], 1, 2)
    print(elozoFEORKetjegy)
    At_least_12_months <- rbind(At_least_12_months, Income_SzulEvHo %>% count(AAJE, LGAA029) %>% filter(n >= 12))
    
    gc()
      
  }

  database_SzulEvHo_FILTER <- as.data.table(subset(Income_SzulEvHo, Income_SzulEvHo$SZUL_IDO == paste(SILC_FEOR2Jegy[i, "SZEV"], SILC_FEOR2Jegy[i, "SZHO"], sep = "-") & Income_SzulEvHo$ML05 == SILC_FEOR2Jegy[i, "NEME"] & Income_SzulEvHo$LGAA517 != 0 & Income_SzulEvHo$LGAA520 != 0))
  database_SzulEvHo_FILTER <- database_SzulEvHo_FILTER[, .(SUMLGAA517 = sum(LGAA517), SUMLGAA520 = sum(LGAA520)), by = "AAJE,LGAA029,SZUL_IDO,ML05"]

  if(nrow(database_SzulEvHo_FILTER) != 0){

    database_WITH_SILC_AZON_by_FEOR <- as.data.frame(cbind(SILC_FEOR2Jegy[i, "FIXSZ"], SILC_FEOR2Jegy[i, "SZNEV_VIZSGALT"], SILC_FEOR2Jegy[i, "ANYNEV_VIZSGALT"], SILC_FEOR2Jegy[i, "FEOR08"], SILC_FEOR2Jegy[i, "NEME"], database_SzulEvHo_FILTER))
    Income <- rbind(Income, database_WITH_SILC_AZON_by_FEOR)

  }
    
}

dbDisconnect(con)
dim(Income) #1.666.074 sor és 11 oszlop
Income_biztonsagi_masolat <- Income

Income <- Income[is.na(Income$AAJE) == FALSE, ]
dim(Income) #1.666.056 sor és 11 oszlop

Incomes_All <- Income[, 6:11]
Incomes_All <- unique(Incomes_All)
dim(Incomes_All) # 1.234.710, 13 sor és 6 oszlop
Incomes_All <- Incomes_All[Incomes_All$SUMLGAA517 > 300000, ]
dim(Incomes_All) # 1.181.198
Incomes_All$LGAA029_2jegy <- str_sub(Incomes_All$LGAA029, start = 1, end = 2)

mean1 <- as.data.table(Incomes_All)
mean1 <- as.data.frame(mean1[, .(LGAA517 = mean(SUMLGAA517), LGAA520 = mean(SUMLGAA520)), by = "SZUL_IDO,ML05,LGAA029_2jegy"])
dim(mean1) # 6189 sor és 5 oszlop

write.table(mean1 ,"Számtani_közép_első_verzió2.txt", quote = FALSE, sep = ";", row.names = FALSE, append = FALSE)

mean2 <- as.data.table(Incomes_All)
dim(mean2) # 1.181.198 sor és 7 oszlop
mean2 <- subset(mean2, mean2$AAJE %in% At_least_12_months$AAJE)
dim(mean2) # 797.898

mean2 <- as.data.table(mean2)
mean2 <- as.data.frame(mean2[, .(LGAA517=mean(SUMLGAA517), LGAA520=mean(SUMLGAA520)), by = "ML05,SZUL_IDO,LGAA029_2jegy"])

write.table(mean2 ,"Számtani_közép_JOBB2.txt", quote = FALSE, sep = ";", row.names = FALSE, append = FALSE)