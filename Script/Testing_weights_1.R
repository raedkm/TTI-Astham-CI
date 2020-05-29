
# Loading data set

load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets



year <- 10
  
# Assigning the ACBS and BRFSS file paths
  path_ACBS <- paste0("Input/ACBS_20", year, ".SAV")
  path_BRFSS <- paste0("Input/CDBRFS", year, ".XPT")
  
  
  # Reading the ACBS and BRFSS files
  ACBS <- read.spss(path_ACBS, use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T)
  BRFSS <- read.xport(path_BRFSS)
  
  
  # Excluding states that are not included in the study 
  exlude_state <- c(2,15,66,72,78)

  
  # ACBS Analysis
  
  ACBS_sample <- ACBS %>% 
    select("X._STATE", "INCIDNT", "X._EVER_ASTH_C" , "CHILDWT_F", "X._CHILDWT") %>% 
    rename(FIPS =  X._STATE ) %>% 
    group_by(FIPS, INCIDNT)

 
  
  BRFSS_weighted <- BRFSS %>% 
    select("X_STATE", "X_CHILDWT") %>% 
    rename(FIPS =  X_STATE ) 
    
  
sum(ACBS_sample$CHILDWT_F)
sum(ACBS_sample$X._CHILDWT)
sum(BRFSS_weighted$X_CHILDWT, na.rm = T)  

  
  ACBS_sample <- ACBS %>% 
    select("X._STATE", "INCIDNT", "X._EVER_ASTH_C" , "CHILDWT_F", "X._CHILDWT") %>% 
    rename(FIPS =  X._STATE ) %>% 
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, INCIDNT) %>% 
    summarise(count = sum(CHILDWT_F, na.rm = T)) %>% 
    spread(INCIDNT, count) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS)
  
  sum(ACBS_sample$X._EVER_ASTH_C)
  sum(ACBS_sample$CHILDWT_F)
  head(ACBS_sample)
  
  
  ACBS_weighted <- ACBS %>% 
    select("X._STATE", "INCIDNT", "CHILDWT_F") %>% 
    rename(FIPS =  X._STATE ) %>% 
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, INCIDNT) %>% 
    summarise(count = sum(CHILDWT_F, na.rm = T)) %>% 
    spread(INCIDNT, count) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS)
  

  

  
  # Joining ACBS with BRFSS
  
  Asthma_sample <- ACBS_sample %>% 
    full_join(BRFSS_sample, by = "FIPS", suffix = c("_ACBS", "_BRFSS")) %>% 
    rename(State = State_BRFSS)

  
  
  
  # Incidence Rate Estimation
  
  Asthma_IR <- Asthma_weighted %>% 
    mutate(At_risk = `1_ACBS` + `2_BRFSS`) %>% 
    mutate(IR_per1000 = `1_ACBS` / At_risk*1000) %>% 
    select(FIPS,State, `1_ACBS`, At_risk, IR_per1000) %>% 
    arrange(FIPS) %>% 
    rename(`<12_month` =  `1_ACBS`) 
  
  
  # Prevalance Rate EstimationEstimat
  
  Asthma_PRV <- Asthma_sample %>% 
    mutate(SAMPLE = `1_BRFSS` + `2_BRFSS`) %>% 
    rename(EVER =  `1_BRFSS`) %>%
    mutate(PRV_per100 = EVER/ SAMPLE * 100) %>% 
    select(FIPS,State, EVER, SAMPLE, PRV_per100) %>% 
    arrange(FIPS)
 
   
  # Converting to data frame to print
  
  Asthma_IR <- as.data.frame(Asthma_IR)
  Asthma_PRV <- as.data.frame(Asthma_PRV)
  Asthma_sample <- as.data.frame(Asthma_sample)
  Asthma_weighted <- as.data.frame(Asthma_weighted)
  
  
  # Printing to Excel sheet
  
  write.xlsx(Asthma_sample, "Output/Tables/Asthma_result.xlsx", sheetName = paste0("20", year, "_count"), showNA=F, append = T, row.names = F)
  write.xlsx(Asthma_weighted, "Output/Tables/Asthma_result.xlsx", sheetName = paste0("20", year, "_weighted"), showNA=F, append = T,row.names = F)
  write.xlsx(Asthma_IR, "Output/Tables/Asthma_IR.xlsx", sheetName = paste0("20", year), showNA=F, append = T, row.names = F)
  write.xlsx(Asthma_PRV, "Output/Tables/Asthma_PRV.xlsx", sheetName = paste0("20", year), showNA=F, append = T, row.names = F)
  
  
  
}




