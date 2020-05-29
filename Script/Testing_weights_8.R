


#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Purpose: Conduct the burden modeling and estimate results by state
#Part : (T-01) Creating new weigths 
#Created by Raed Alotaibi
#Date Created: April-24-2020
#Last modified: April-24-2020
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
library(tidyverse)
library(foreign)
library(xlsx)
library(readxl)
library(data.table)
library(survey)


# creating variables to feed into functions
exlude_state <- c(2,15,66,72,78)



#composed functions

read_rename_ACBS <- compose(
  partial(rename, FIPS =  X._STATE, PSU = X._PSU, STSTR = X._STSTR),
  partial(read.spss, use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T)
)

read_rename_BRFSS <- compose(
  partial(rename, FIPS =  X_STATE, PSU = X_PSU, STSTR = X_STSTR),
  read.xport
)




# Loading FIPS and file paths -------------------------------------------------------


#Loading ACBS and BRFSS file paths

path <- "C:/Users/Raed Dell/Documents/R projects/TTI-AsthmaIR/Input"

files_ACBS <- list.files(path=path ,pattern = ".sav",full.names=TRUE)
files_BRFSS <- list.files(path=path ,pattern = ".XPT",full.names=TRUE)



# loading FIPS file
load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets




# Loading ACBS
ACBS <- map(files_ACBS, ~.x %>% 
              read_rename_ACBS %>% 
              select("FIPS", "INCIDNT", "CHILDWT_F","PSU", "STSTR", "X._CHILDWT") %>%
              filter(!FIPS  %in% exlude_state)) %>% 
  set_names(.,c("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")) %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") %>% 
  mutate(type = "ACBS") %>% 
  as_tibble()



# Loading BRFSS
BRFSS <- map(files_BRFSS, ~.x %>% 
               read_rename_BRFSS %>% 
               select("FIPS", "CASTHDX2", "X_CHILDWT", "PSU", "STSTR") %>% 
               filter(!FIPS  %in% exlude_state,
                      !is.na(X_CHILDWT))) %>%
  set_names(.,c("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")) %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") %>% 
  mutate(type = "BRFSS") %>% 
  as_tibble()



# Assigning new weights ---------------------------------------------------

# ACBS
ACBS_sample_k <- ACBS %>% 
  filter(!is.na(CHILDWT_F)) %>% 
  group_by(FIPS, year) %>% 
  count() %>% 
  mutate(k = 1)  %>%
  group_by(FIPS) %>% 
  summarise(k = sum(k)) 


BRFSS_sample_k <- BRFSS  %>%
  filter(!is.na(X_CHILDWT), !is.na(CASTHDX2)) %>% 
  group_by(FIPS, year) %>% 
  count() %>% 
  mutate(k = 1)  %>%
  group_by(FIPS) %>% 
  summarise(k = sum(k))


ACBS_final <- ACBS %>% 
  left_join(ACBS_sample_k, by = c("FIPS")) %>% 
  mutate(new_weight = CHILDWT_F / k,
         new_case = factor(if_else(INCIDNT == 1, 1, 0))) 


BRFSS_final <- BRFSS %>% 
 inner_join(BRFSS_sample_k, by = c("FIPS")) %>% 
  mutate(new_weight =  X_CHILDWT / k) %>% 
  filter(!is.na(new_weight)) %>% 
  as_tibble()


# Merging -----------------------------------------------------------------


join_asthm_new <- full_join(ACBS_final, BRFSS_final, by = c("year", "FIPS", "STSTR", "PSU"), suffix = c("_ACBS", "_BRFSS")) %>% 
  mutate(type = ifelse(is.na(type_ACBS), "BRFSS",  "ACBS"),
         k = if_else(type == "ACBS", k_ACBS, k_BRFSS),
         weight = if_else(type == "ACBS", CHILDWT_F, X_CHILDWT),
         new_weight = weight / k,
         CASTHDX2 = if_else(is.na(CASTHDX2), 0, CASTHDX2), 
         INCIDNT = if_else(is.na(INCIDNT), 0, INCIDNT), 
         new_case = if_else(INCIDNT == 1 , 1, 0),
         at_risk = if_else(( INCIDNT == 1 | CASTHDX2 == 2 ) , 1, 0),
         cases_w = new_case * new_weight,
         at_risk_w =  at_risk * new_weight) %>% 
  select(-X._CHILDWT,  -new_weight_BRFSS, -new_weight_ACBS, -type_ACBS, -type_BRFSS, -k_ACBS, -k_BRFSS) %>% 
  as_tibble() 


# Other tables and results ------------------------------------------------

join_asthm_new %>% 
  filter(type == "ACBS") %>% 
  group_by(year, type) %>% 
  mutate(s=1) %>% 
  summarise(`ACBS sample` = sum(s),
            `ACBS weight` = sum(weight),
            `Incident cases sample` = sum(new_case),
            `Incident cases weight` = sum(cases_w)) %>% 
  t()



join_asthm_new %>% 
  filter(type == "BRFSS") %>% 
  mutate(Ever_asthma_i = if_else(CASTHDX2 ==1 , 1, 0),
         Ever_asthma = Ever_asthma_i * weight) %>%  
  group_by(year, type) %>% 
  mutate(s  =1) %>% 
  summarise(`BRFSS sample` = sum(s),
            `BRFSS weight` = sum(weight),
            `Ever asthma sample` = sum(Ever_asthma_i),
            `Ever asthma weight` = sum(Ever_asthma)) %>% 
  t()
