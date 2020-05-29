#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Purpose: Conduct the burden modeling and estimate results by state
#Part : (T-01) Creating new weigths 
#Created by Raed Alotaibi
#Date Created: April-8-2020
#Last modified: April-13-2020
#---------------------------------------------#






# loading libraries
library(foreign)
library(plyr)
library(dplyr)
library(xlsx)
library(tidyr)
library(readxl)
library(purrr)
library(data.table)
library(survey)


# Loading data set


# laoding FIPS file
load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets


# Loading file paths for ACBS and BRFSS
path <- "C:/Users/Raed Dell/Documents/R projects/TTI-AsthmaIR/Input"

files_ACBS <- list.files(path=path ,pattern = ".sav",full.names=TRUE)
files_BRFSS <- list.files(path=path ,pattern = ".XPT",full.names=TRUE)


# Excluding states that are not included in the study 
exlude_state <- c(2,15,66,72,78)




# ACBS  --------------------------------------------------------------------



ACBS <- map(files_ACBS, ~.x %>% 
              read.spss(use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T) %>% 
              select("X._STATE", "INCIDNT", "CHILDWT_F", "X._PSU", "X._STSTR") %>% 
              rename(FIPS =  X._STATE,
                     PSU = X._PSU,
                     STSTR = X._STSTR) %>%  
              filter(!FIPS  %in% exlude_state,
                     !is.na(INCIDNT)) %>% 
              mutate(id = 1,
                     new_case = if_else(INCIDNT == 1, 1, 0))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist( use.names=TRUE, fill=TRUE, idcol="ID") 





# Sample estimates ACBS ---------------------------------------------------


# estimate incidence sample per year

ACBS_year_samples <- ACBS %>%
  group_by(ID) %>% 
  summarise(samples = sum(id)) %>%
  spread(ID, samples)


# estimate incidence sample per year

ACBS_year_incident <- ACBS %>%
  group_by(ID) %>% 
  summarise(new_case = sum(new_case)) %>%
  spread(ID, new_case)  


# estimate sample size for each state per year

ACBS_State_year_samples <- ACBS %>%
  group_by(ID,FIPS) %>% 
  summarise(samples = sum(id)) %>% 
  spread(ID, samples) %>% 
  mutate(Total = rowSums(select(., -"FIPS"), na.rm = T))



# estimate incidence sample for each state per year

ACBS_State_year_incident <- ACBS %>%
  group_by(ID,FIPS) %>% 
  summarise(new_case = sum(new_case)) %>% 
  spread(ID, new_case) %>% 
  mutate(Total = rowSums(select(., -"FIPS"), na.rm = T))




# Weighted estimates ACBS -------------------------------------------------

ACBS_weighted <- map(files_ACBS, ~.x %>% 
                       read.spss(use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T) %>% 
                       select("X._STATE", "INCIDNT", "CHILDWT_F", "X._PSU", "X._STSTR") %>% 
                       rename(FIPS =  X._STATE,
                              PSU = X._PSU,
                              STSTR = X._STSTR) %>%  
                       filter(!FIPS  %in% exlude_state,
                              !is.na(INCIDNT)) %>% 
                       mutate(id = 1,
                              new_case = if_else(INCIDNT == 1, 1, 0))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") 





ACBS_mean<- list()
for(i in 1:length(ACBS_design)) {
  ACBS_mean[i] <-  ACBS_weighted[i] %>% 
    map(~svyby(~ new_case, ~ FIPS, ACBS_design[[i]], na.rm = TRUE, svytotal, level = 0.95))
  print(ACBS_mean)
}  

ACBS_mean <- ACBS_mean %>%  
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")



ACBS_mean %>% 
  map_df(~data.frame(
    FIPS = .x[["FIPS"]],
    new_case = .x[["new_case"]])) %>% 
  print("hello")


map2(ACBS_design,
     ACBS_weighted,
     ~.y , ~.y)

# estimate weighted incidence for each state per year

ACBS_year_incident_weighted <- ACBS %>%
  mutate(new_case_w = new_case*CHILDWT_F) %>% 
  group_by(ID) %>% 
  summarise(samples = sum(id)) %>%
  spread(ID, samples)



# estimate weighted incidence for each state per year

ACBS_State_year_incident_weighted <- ACBS %>%
  mutate(new_case_w = new_case*CHILDWT_F) %>% 
  group_by(ID,FIPS) %>% 
  summarise(new_case_w = sum(new_case_w)) %>% 
  spread(ID, new_case_w) %>% 
  mutate(Total = rowSums(select(., -"FIPS"), na.rm = T))









# calculate the Standard Error (SE) for incidence cases by state 

svymean(~new_case, ACBS_dsgn, na.rm = TRUE, level = 0.95)


ACBS_se <- svyby(~ new_case, ~ FIPS, ACBS_dsgn, na.rm = TRUE, svytotal, level = 0.95) %>% 
  select(FIPS, se)


