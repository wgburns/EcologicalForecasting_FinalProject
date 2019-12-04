#Old script for calculating and plotting daily means and maxes, and adding (future) timelags.
SA18_daily <- SA18 %>%
  select(c(timestamp, TEMP_0.5m, SPCOND_0.5m, PH_0.5m, ODO_CONC_0.5m, 
           CHL_RFU_0.5m, TURB_0.5m, deltaDO, deltaTemp, PC_RFU_0.5m)) %>%
  rename(Timestamp = timestamp,
         Temp = TEMP_0.5m, 
         SpCond = SPCOND_0.5m, 
         pH = PH_0.5m,
         ODO = ODO_CONC_0.5m,
         Chl = CHL_RFU_0.5m,
         Turb = TURB_0.5m,
         DeltaDO = deltaDO,
         DeltaTemp = deltaTemp,
         PC = PC_RFU_0.5m) %>%
  mutate(timestamp = as.POSIXct(strptime(Timestamp, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT-4")),
         date = as.Date(timestamp, tz = "Etc/GMT-4"), 
         PC = replace(PC, which(PC<0), 0), 
         Chl = replace(Chl, which(Chl<0), 0)) %>% 
  group_by(date) %>%
  summarize(TempMean = mean(Temp, na.rm = F), 
            TempMax = max(Temp, na.rm = F),
            SpCondMean = mean(SpCond, na.rm = F), 
            SpCondMax = max(SpCond, na.rm = F),
            pHMean = mean(pH, na.rm = F),
            pHMax = max(pH, na.rm = F),
            ODOMean = mean(ODO, na.rm = F),
            ODOMax = max(ODO, na.rm = F),
            ChlMean = mean(Chl, na.rm = F), 
            ChlMax = max(Chl, na.rm = F),
            TurbMean = mean(Turb, na.rm = F),
            TurbMax = max(Turb, na.rm = F),
            DeltaDOMean = mean(DeltaDO, na.rm = F), 
            DeltaDOMax = max(DeltaDO, na.rm = F),
            DeltaTempMean = mean(DeltaTemp, na.rm = F), 
            DeltaTempMax = max(DeltaTemp, na.rm = F),
            PCMean1 = mean(PC, na.rm = F), 
            PCMax = max(PC, na.rm = F)) 
SA18_daily$PCMean2 <- lead(SA18_daily$PCMean1)
SA18_daily$PCMean3 <- lead(SA18_daily$PCMean1, 2)
SA18_daily$PCMean4 <- lead(SA18_daily$PCMean1, 3)
SA18_daily$PCMean5 <- lead(SA18_daily$PCMean1, 4)
SA18_daily$PCMean6 <- lead(SA18_daily$PCMean1, 5)
SA18_daily$PCMean7 <- lead(SA18_daily$PCMean1, 6)
SA18_daily$PCMax1 <- SA18_daily$PCMax
SA18_daily$PCMax2 <- lead(SA18_daily$PCMax)
SA18_daily$PCMax3 <- lead(SA18_daily$PCMax, 2)
SA18_daily$PCMax4 <- lead(SA18_daily$PCMax, 3)
SA18_daily$PCMax5 <- lead(SA18_daily$PCMax, 4)
SA18_daily$PCMax6 <- lead(SA18_daily$PCMax, 5)
SA18_daily$PCMax7 <- lead(SA18_daily$PCMax, 6)
SA18_daily$PCMax <- NULL
write.csv(SA18_daily, "SA18_daily.csv")

#Plot maxes:
timePlot(SA18_daily, pollutant = c("PCMax", 
                                   "TempMax",
                                   "SpCondMax",
                                   "ChlMax",
                                   "pHMax",
                                   "ODOMax",
                                   "TurbMax",
                                   "DeltaDOMax",
                                   "DeltaTempMax"),
         y.relation = "free")
