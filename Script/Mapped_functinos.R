#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Purpose: Conduct the burden modeling and estimate results by state
#Part : (T-01) Creating new weigths 
#Created by Raed Alotaibi
#Date Created: April-19-2020
#Last modified: April-19-2020
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
library(purrr)
library(data.table)
library(survey)







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

path_ACBS <- "C:/Users/Raed Dell/Documents/R projects/TTI-AsthmaIR/Input"
files_ACBS <- list.files(path=path_ACBS ,pattern = ".sav",full.names=TRUE)
files_BRFSS <- list.files(path=path ,pattern = ".XPT",full.names=TRUE)



# loading FIPS file
load("Input/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets





# Creating indicator variable of available k data for ACBS set BRFSS --------



# Loading ACBS
ACBS <- map(files_ACBS, ~.x %>% 
              read_rename_ACBS %>% 
              select("FIPS", "INCIDNT", "CHILDWT_F","PSU", "STSTR") %>%
              filter(!FIPS  %in% exlude_state)) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year")
  

# ACBS indicator
ACBS_i <- ACBS %>%   
  group_by(year, FIPS) %>% 
  summarise() %>% 
  mutate(i = "ACBS")





# Loading BRFSS
BRFSS <- map(files_BRFSS, ~.x %>% 
              read_rename_BRFSS %>% 
              select("FIPS", "CASTHDX2", "X_CHILDWT", "PSU", "STSTR") %>% 
              filter(!FIPS  %in% exlude_state,
                     !is.na(CASTHDX2))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") 


#BRFSS indicator
BRFSS_i <- BRFSS %>% 
  group_by(year, FIPS) %>% 
  summarise() %>% 
  mutate(i = "BRFSS")




# ACBS set BRFSS indicator
asthma_i <- full_join(ACBS_i, BRFSS_i, by = c("year", "FIPS")) 

mutate(i = case_when(
    (!is.na(i.x) & !is.na(i.y)) ~ paste(i.x, i.y),
    (!is.na(i.x) &  is.na(i.y)) ~ i.x,
    ( is.na(i.x) & !is.na(i.y)) ~ i.y)) %>% 
  spread(year, i)




# Analysis ACBS ----------------------------------------------------------------



# nested dataframe
n_ACBS <- ACBS %>% 
  group_by(FIPS) %>% 
  nest() %>% 
  rename(ACBS_data = data) 


# Summarise sample size functin

n_ACBS <- ACBS %>%
  mutate(new_case = ifelse(INCIDNT == 1, 1, 0)) %>% 
  group_by(year,FIPS) %>% 
  nest() %>% 
  mutate(sample_k = map_int(data, nrow)) %>% 
  group_by(FIPS) %>% 
  mutate(sample_t = sum(sample_k),
         weight_prop = sample_k / sample_t) %>% 
  unnest() %>% 
  group_by(year, FIPS) %>% 
  mutate(weigth_new = weight_prop * CHILDWT_F)%>% 
  group_by(FIPS) %>% 
  nest() %>% 
  rename(ACBS_data = data) 




n_ACBS[[2]]




# Analysis BRFSS ----------------------------------------------------------


# nested dataframe
n_BRFSS <- BRFSS %>%
  group_by(FIPS) %>% 
  nest() %>% 
  rename(BRFSS_data = data) 


  
