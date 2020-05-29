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
library(magrittr)




# Creating partial and composed functions ----------------------------------


# creating variables to feed into functions
exlude_state <- c(2,15,66,72,78)


# Partial functions
group_FIPS <- partial(group_by, by = FIPS)

svydsgn_ACBS <- partial(svydesign, id = ~ 1, strata = ~ year+STSTR, weights = ~ new_weight,  nest = TRUE, data = .x)


#composed functions

read_rename_ACBS <- compose(
  partial(rename, FIPS =  X._STATE, PSU = X._PSU, STSTR = X._STSTR),
  partial(read.spss, use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T)
)

read_rename_BRFSS <- compose(
  partial(rename, FIPS =  X_STATE, PSU = X_PSU, STSTR = X_STSTR),
  read.xport
)


svydesign_fun <- function(df){
  svydesign(id = ~ 1, strata = ~ year+STSTR, weights = ~ weigth_new,  nest = TRUE, data = df)
}


incident_fun <- function(df ){
  svytotal(~ new_case, design = df ,level = 0.95)
}






# Loading FIPS and file paths -------------------------------------------------------


#Loading ACBS and BRFSS file paths

path <- "C:/Users/Raed Dell/Documents/R projects/TTI-AsthmaIR/Input"

files_ACBS <- list.files(path=path ,pattern = ".sav",full.names=TRUE)
files_BRFSS <- list.files(path=path ,pattern = ".XPT",full.names=TRUE)



# loading FIPS file
load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets





# Creating indicator variable of available k data for ACBS set BRFSS --------



# Loading ACBS
ACBS <- map(files_ACBS, ~.x %>% 
              read_rename_ACBS %>% 
              select("FIPS", "INCIDNT", "CHILDWT_F","PSU", "STSTR", "X._CHILDWT") %>%
              filter(!FIPS  %in% exlude_state)) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year")



# Loading BRFSS
BRFSS <- map(files_BRFSS, ~.x %>% 
               read_rename_BRFSS %>% 
               select("FIPS", "CASTHDX2", "X_CHILDWT", "PSU", "STSTR") %>% 
               filter(!FIPS  %in% exlude_state,
                      !is.na(CASTHDX2),
                      !is.na(X_CHILDWT))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") 





# Creating filtered data sets (set join) ----------------------------------

# To estimate the state IR for a givin year k, both the ACBS and BRFSS must be available for year k.

ACBS_filter <- ACBS %>% 
  semi_join(BRFSS, by = c("FIPS", "year")) %>% 
  as_tibble()


BRFSS_filter <- BRFSS %>% 
  semi_join(ACBS, by = c("FIPS", "year")) %>% 
  as_tibble()




# Assigning new weights ---------------------------------------------------

# ACBS
ACBS_sample_k <- ACBS_filter %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  rename(sample_k = n) 
  
ACBS_sample_t <- ACBS_sample_k %>% 
  group_by(FIPS) %>% 
  summarise(sample_t = sum(sample_k)) 


ACBS_final <- ACBS_filter %>% 
  left_join(ACBS_sample_k, by = c("year", "FIPS")) %>% 
  left_join(ACBS_sample_t, by =  "FIPS") %>% 
  mutate(weight_prop = sample_k/sample_t,
         new_weight = weight_prop * CHILDWT_F,
         new_case = if_else(INCIDNT == 1, 1, 0))




# BRFSS
BRFSS_sample_k <- BRFSS_filter %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  rename(sample_k = n) 

BRFSS_sample_t <- BRFSS_sample_k %>% 
  group_by(FIPS) %>% 
  summarise(sample_t = sum(sample_k)) 


BRFSS_final <- BRFSS_filter %>% 
  left_join(BRFSS_sample_k) %>% 
  left_join(BRFSS_sample_t) %>% 
  mutate(weight_prop = sample_k/sample_t,
         new_weight = weight_prop * X_CHILDWT)



# Creating survey design objects ------------------------------------------



options(survey.lonely.psu = "adjust")



# design
ACBS_design <- svydesign(id = ~ 1, strata = ~ year + STSTR, weights = ~ new_weight,  nest = TRUE, data = ACBS_final)
BRFSS_design <- svydesign(id = ~ 1, strata = ~ year + STSTR, weights = ~ new_weight,  nest = TRUE, data = BRFSS_final)


summary(ACBS_design)  
summary(BRFSS_design)
  


# Creating survey tables --------------------------------------------------

state_9 <- ACBS_final %>% 
  filter(year == "Y2009",
         FIPS == 17)

state_9_design <- svydesign(id = ~ 1, strata = ~ STSTR  , weights = ~ CHILDWT_F,  nest = TRUE, data = state_9, na)
(svyby(~ new_case , ~ FIPS, state_9_design, na.rm = TRUE, svytotal, vartype = "ci")) 


ACBS_design <- svydesign(id = ~ PSU, strata = ~ year+STSTR, weights = ~ new_weight,  nest = TRUE, data = ACBS_final)
ACBS_se <- svyby(~ new_case , ~ FIPS, ACBS_design, na.rm = TRUE, svytotal, vartype = "ci") 

ts <- svyciprop(new_case  , ACBS_design) 
summary(ts)


ACBS_se %>% 
  mutate(new_case1000 = new_case*1000)

