
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




# Creating partial and composed functions ----------------------------------


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
               filter(!FIPS  %in% exlude_state)) %>% 
  set_names(.,c("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")) %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") %>% 
  mutate(type = "BRFSS") %>% 
  as_tibble()



# Assigning new weights ---------------------------------------------------

# ACBS
ACBS_sample_k <- ACBS %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  rename(sample_k = n) 

ACBS_sample_t <- ACBS_sample_k %>% 
  group_by(FIPS) %>% 
  summarise(sample_t = sum(sample_k)) 

ACBS_sample <- ACBS_sample_k %>% 
  semi_join(BRFSS_sample_k, by = c("FIPS", "year")) %>% 
  left_join(ACBS_sample_t)


ACBS_final <- ACBS %>% 
  inner_join(ACBS_sample, by = c("FIPS", "year")) %>% 
  mutate(weight_prop = sample_k/sample_t,
         new_weight = weight_prop * CHILDWT_F,
         new_case = if_else(INCIDNT == 1, 1, 0)) %>% 
  filter(!is.na(new_weight)) %>% 
  as_tibble()




# BRFSS
BRFSS_sample_k <- BRFSS %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  rename(sample_k = n) 

BRFSS_sample_t <- BRFSS_sample_k %>% 
  group_by(FIPS) %>% 
  summarise(sample_t = sum(sample_k)) 

BRFSS_sample <- BRFSS_sample_k %>% 
  semi_join(ACBS_sample_k, by = c("FIPS", "year")) %>% 
  left_join(BRFSS_sample_t)


BRFSS_final <- BRFSS %>% 
  inner_join(BRFSS_sample, by = c("FIPS", "year")) %>% 
  mutate(weight_prop = sample_k/sample_t,
         new_weight = weight_prop * X_CHILDWT) %>% 
  filter(!is.na(new_weight)) %>% 
  as_tibble()






# Merging data sets

join_asthm <- full_join(ACBS_final, BRFSS_final, by = c("year", "FIPS", "STSTR", "PSU"), suffix = c("_ACBS", "_BRFSS")) %>% 
  mutate(new_weight = if_else(!is.na(new_weight_ACBS),new_weight_ACBS ,new_weight_BRFSS),
         old_weight = if_else(!is.na(CHILDWT_F), CHILDWT_F, X_CHILDWT),
         new_case = if_else(INCIDNT == 1 , 1, 0),
         at_risk = if_else(new_case == 1 | CASTHDX2 != 1 , 1, 0),
         cases_w = new_case * new_weight,
         at_risk_w =  at_risk * new_weight) %>% 
  as_tibble() %>% 
  filter(!is.na(new_weight)) %>% 
  mutate(at_risk = if_else( is.na(at_risk), 1, at_risk)) %>% 
  select(-X._CHILDWT, -sample_k_ACBS, -sample_t_ACBS, -weight_prop_ACBS, -sample_k_BRFSS ,-sample_t_BRFSS ,-weight_prop_BRFSS) 
  




# Cross checking data sets with merged ------------------------------------

ACBS  
BRFSS
ACBS_final
BRFSS_final
join_asthm 


BRFSS_final %>% 
  filter(!is.na(new_weight) & is.na(CASTHDX2)) %>% 
  summarise(sum = sum(new_weight))


join_asthm %>% 
  filter(year %in% c("Y2006")) %>% 
  summarise(at_risk = sum(at_risk, na.rm = T), 
            new_case = sum(new_case, na.rm = T),
            at_risk_w = sum(at_risk_w, na.rm = T),
            cases_w = sum(cases_w, na.rm = T)) %>% 
  mutate(IR = cases_w/at_risk_w*1000)



join_asthm %>% 
  filter(is.na(at_risk))

join_asthm %>% 
  filter(is.na(new_case))

join_asthm %>% 
  filter(is.na(new_weight))

# design ------------------------------------------------------------------

asthma_2006 <- join_asthm %>% 
  filter(year == "Y2007")  


options(survey.lonely.psu = "adjust")


ashtm_design <- svydesign(id = ~ 1, strata = ~ STSTR, weights = ~ new_weight,   nest = TRUE, data = asthma_2006)
summary(ashtm_design)  

svyratio(~ new_case , denominator = ~ at_risk, na.rm = TRUE,  ashtm_design, vartype = "ci") 




asthma_year <- svyby(~ as.factor(new_case) ,  denominator = ~at_risk, ashtm_design, na.rm = TRUE, svyratio, vartype = "ci")








