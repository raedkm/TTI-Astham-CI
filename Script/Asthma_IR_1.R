#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (01) Preparing ACBD and BRFS data sets for 2006-2010, printing out results in excel
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#

Sys.time()

# Houskeeping
ls()
rm(list=ls())
search()

# Check working directory
getwd()


# 
# Java issues
# Sys.getenv("JAVA_HOME")
# if (Sys.getenv("JAVA_HOME")!="")+Sys.setenv(JAVA_HOME="")



# # installing packages
# install.packages("Deducer")
# install.packages("rJava")
# install.packages("foreign")
# install.packages("dplyr")
# install.packages("xlsx")
# install.packages("tidyr")
# install.packages("readxl")
# install.packages("plyr")



# loading libraries
library(foreign)
library(plyr)
library(dplyr)
library(xlsx)
library(tidyr)
library(readxl)


# Loading data set

load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets


# Creating for loop to run the code for the years 2006-2010
for(i in 6:10){
  
  ifelse(i < 10, year <- paste0("0", i), year <- i)
  
  print(paste0(" ######## RUNNING ANALYSIS FOR THE YEAR 20", year ))
  
  
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
    select("X._STATE", "INCIDNT") %>% 
    rename(FIPS =  X._STATE ) %>% 
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, INCIDNT) %>% 
    count(INCIDNT) %>%
    spread(INCIDNT, n) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS)
  
  
  
  ACBS_weighted <- ACBS %>% 
    select("X._STATE", "INCIDNT", "CHILDWT_F") %>% 
    rename(FIPS =  X._STATE ) %>% 
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, INCIDNT) %>% 
    summarise(count = sum(CHILDWT_F, na.rm = T)) %>% 
    spread(INCIDNT, count) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS)
  
  
  
  # BRFSS Analysis
  
  BRFSS_sample <- BRFSS %>% 
    select("X_STATE", "CASTHDX2") %>% 
    rename(FIPS =  X_STATE ) %>% 
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, CASTHDX2) %>% 
    count(CASTHDX2) %>%
    spread(CASTHDX2, n) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS)
  
  BRFSS_weighted <- BRFSS %>% 
    select("X_STATE", "CASTHDX2", "X_CHILDWT") %>% 
    rename(FIPS =  X_STATE ) %>%
    filter(!FIPS  %in% exlude_state ) %>% 
    group_by(FIPS, CASTHDX2) %>% 
    summarise(count = sum(X_CHILDWT, na.rm = T)) %>% 
    spread(CASTHDX2, count) %>% 
    full_join(FIPS) %>% 
    arrange(FIPS) %>% 
    na_if(0) # The BRFSS had some values coded as zero when they should have been NA
  
  
  # Joining ACBS with BRFSS
  
  Asthma_sample <- ACBS_sample %>% 
    full_join(BRFSS_sample, by = "FIPS", suffix = c("_ACBS", "_BRFSS")) %>% 
    rename(State = State_BRFSS)
  
  
  Asthma_weighted <- ACBS_weighted %>% 
    full_join(BRFSS_weighted, by = "FIPS", suffix = c("_ACBS", "_BRFSS")) %>% 
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





