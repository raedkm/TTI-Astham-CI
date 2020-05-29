#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Purpose: Conduct the burden modeling and estimate results by state
#Part : (T-01) Creating new weigths 
#Created by Raed Alotaibi
#Date Created: April-8-2020
#Last modified: April-8-2020
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


# estimate sample size for each state per year
ACBS_samples <- map(files_ACBS, ~.x %>% 
                  read.spss(use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T) %>% 
                  select("X._STATE", "INCIDNT", "CHILDWT_F") %>% 
                 rename(FIPS =  X._STATE ) %>% 
                 filter(!FIPS  %in% exlude_state,
                        !is.na(INCIDNT)) %>% 
                 mutate(id = 1,
                        new_case = if_else(INCIDNT == 1, 1, 0)) %>% 
                 group_by(FIPS) %>% 
                 summarize(samples = sum(id))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")



# estimating total sample size for each state across 2006:2010

ACBS_df <-  rbindlist(ACBS_samples, use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  group_by(FIPS) %>%
  mutate(k = 1) %>% 
  summarise(Total_samples = sum(samples))



# estimating the proportion of new weightes for each state per year

ACBS_weigth_prop <-  map(ACBS_samples, ~.x %>% left_join(ACBS_df, by = "FIPS") %>% 
                           mutate(new_weight_prop = samples/Total_samples) %>% 
                           select(FIPS, new_weight_prop)) %>% 
  rbindlist( use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  mutate(k = 1)



# Creating  an ACBS data frame with new weigths (new_weight)
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
                      rbindlist( use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  left_join(ACBS_weigth_prop, by = c("ID", "FIPS")) %>% 
  mutate(new_weight = CHILDWT_F*new_weight_prop) %>% 
  select(-CHILDWT_F , -new_weight_prop  )







# BRFSS -------------------------------------------------------------------


# estimate sample size for each state per year
BRFSS_samples <- map(files_BRFSS, ~.x %>% 
                      read.xport() %>% 
                      select("X_STATE", "CASTHDX2", "X_CHILDWT") %>% 
                      rename(FIPS =  X_STATE ) %>% 
                      filter(!FIPS  %in% exlude_state,
                             !is.na(CASTHDX2)) %>% 
                      mutate(id = 1,
                             prevalent = if_else(CASTHDX2 == 1, 1, 0)) %>% 
                      group_by(FIPS) %>%  
                      summarize(samples = sum(id))) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010")



# estimating total sample size for each state across 2006:2010

BRFSS_df <-  rbindlist(BRFSS_samples, use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  group_by(FIPS) %>%
  mutate(k = 1) %>% 
  summarise(Total_samples = sum(samples))



# estimating the proportion of new weightes for each state per year

BRFSS_weigth_prop <-  map(BRFSS_samples, ~.x %>% left_join(BRFSS_df, by = "FIPS") %>% 
                           mutate(new_weight_prop = samples/Total_samples) %>% 
                           select(FIPS, new_weight_prop)) %>% 
  rbindlist( use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  mutate(k = 1)



# Creating  an BRFSS data frame with new weigths (new_weight) using all the previous estimates

BRFSS <- map(files_BRFSS, ~.x %>% 
               read.xport() %>% 
               select("X_STATE", "CASTHDX2", "X_CHILDWT", "X_PSU", "X_STSTR") %>% 
               rename(FIPS =  X_STATE ,
                      PSU = X_PSU,
                      STSTR = X_STSTR) %>% 
               filter(!FIPS  %in% exlude_state,
                      !is.na(CASTHDX2),
                      !is.na(X_CHILDWT)) %>% 
               mutate(id = 1,
                      prevalent = if_else(CASTHDX2 == 1, 1, 0))) %>%  
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist( use.names=TRUE, fill=TRUE, idcol="ID") %>% 
  left_join(BRFSS_weigth_prop, by = c("ID", "FIPS")) %>% 
  mutate(new_weight = X_CHILDWT*new_weight_prop) %>% 
  select(-X_CHILDWT , -new_weight_prop  )






# Estimating number of years states contributed to data sets (k) --------------------------------------------------


# creating data frame for k (number of years each state contributed to the ACBS data)

ACBS_k <- ACBS_weigth_prop %>% 
  group_by(FIPS, ID) %>% 
  summarise(k = sum(k))


# creating data frame for k (number of years each state contributed to the BRFSS data)

BRFSS_k <- BRFSS_weigth_prop %>% 
  group_by(FIPS, ID) %>% 
  summarise(k = sum(k))


k <-   inner_join(ACBS_k, BRFSS_k, by = c("FIPS", "ID"), suffix = c("_ACBS", "_BRFSS")) %>% 
  mutate(k = 1) %>% 
  group_by(FIPS) %>% 
  summarise(k_years = sum(k))
  






# Estimating the SE for IR by state using the survey package  --------------------------------------


# Set options for allowing a single observation per stratum

options(survey.lonely.psu = "adjust")


# Create survey design variable (seeThe Behavioral Risk Factor Surveillance System Complex Sampling Weights and Preparing 2017 BRFSS Module Data for Analysis July 2018)

ACBS_dsgn <- svydesign(id = ~ 1, strata = ~ ID+STSTR, weights = ~ new_weight,  nest = TRUE, data = ACBS)

summary(ACBS_dsgn)

# calculate the Standard Error (SE) for incidence cases by state 
 
svymean(~new_case, ACBS_dsgn, na.rm = TRUE, level = 0.95)


ACBS_se <- svyby(~ new_case, ~ FIPS, ACBS_dsgn, na.rm = TRUE, svytotal, level = 0.95) %>% 
  select(FIPS, se)




# Estimating the SE for pR by state using the survey package  --------------------------------------


# Set options for allowing a single observation per stratum

options(survey.lonely.psu = "adjust")


# Create survey design variable (seeThe Behavioral Risk Factor Surveillance System Complex Sampling Weights and Preparing 2017 BRFSS Module Data for Analysis July 2018)

BRFSS_dsgn <- svydesign(id = ~ 1, strata = ~ ID+STSTR, weights = ~ new_weight,  nest = TRUE, data = BRFSS)

summary(BRFSS_dsgn)

# calculate the Standard Error (SE) for incidence cases by state 

svymean(~prevalent , BRFSS_dsgn, na.rm = TRUE, level = 0.95)


BRFSS_se <- svyby(~ prevalent , ~ FIPS, BRFSS_dsgn, na.rm = TRUE, svytotal, level = 0.95) %>% 
  select(FIPS, se) 



# Estimating IR and PR by state --------------------------------------------------


incident_cases <- ACBS %>% 
  group_by(FIPS) %>% 
  mutate(case_w = new_case*new_weight) %>% 
  summarize(incident_cases = sum(case_w)) %>% 
            #prevalent_cases = sum(new_weight)) %>% 
  left_join(ACBS_se, by = "FIPS") 


total_children <- BRFSS %>% 
  group_by(FIPS) %>% 
  mutate(prevalent_w = prevalent*new_weight) %>% 
  summarize(total_children = sum(new_weight, na.rm = T),
            prevalent_cases = sum(prevalent_w)) %>% 
  left_join(BRFSS_se, by = "FIPS")


IR <- FIPS %>% 
  left_join(total_children, by = "FIPS") %>%
  left_join(incident_cases, by = "FIPS", suffix = c( "_BRFSS", "_ACBS")) %>%
  left_join(k, by = "FIPS") %>% 
  mutate(IR = incident_cases/total_children*1000,
         IR_se = se_ACBS/total_children*1000,
         IR_lower = IR - IR_se,
         IR_upper = IR + IR_se,
         PR = prevalent_cases/total_children*1000,
         PR_se = se_BRFSS/total_children*1000,
         PR_lower = PR - PR_se,
         PR_upper = PR + PR_se) %>% 
  select(State, total_children, incident_cases, prevalent_cases, PR, PR_lower, PR_upper, IR, IR_lower, IR_upper, k_years, IR_se)





# Impute IR for stateswith missing value --------

# To impute IR for states not contributing to the ACBS we will use a random effect model of the pooled IR across contributing states

library(meta)

IR_filter <- IR %>% 
  filter(!is.na(IR),
         State != "Massachusetts") 

IR_meta <- metagen( TE = IR_filter$IR, 
                    lower = IR_filter$IR_lower, 
                    upper = IR_filter$IR_upper , studlab = IR_filter$State, overall = T,
                    comb.fixed = F) 

# forest plot
IR_meta %>% 
  forest(col.random = "red", 
         bysort = T, 
         xlim =c(0,20), 
         prediction = T,
         smlab = expression("Incidence Rate"),
         leftcols = c("studlab","effect.ci",  "w.random"),
         leftlabs = c("Study","IR95%CI","Weight\n(fixed)", "Weight \n(Random)"),
         rightcols = F,
         lwd = 2)

# To impute PR for states not contributing to the ACBS we will use a random effect model of the pooled IR across contributing states

library(meta)

PR_filter <- IR %>% 
  filter(!is.na(PR),
         State != "Massachusetts") 

PR_meta <- metagen( TE = PR_filter$PR, 
                    lower = PR_filter$PR_lower, 
                    upper = PR_filter$PR_upper , studlab = PR_filter$State, overall = T,
                    comb.fixed = F) 


# forest plot
PR_meta %>% 
  forest(col.random = "red", 
         bysort = T, 
         xlim =c(60,180), 
         prediction = T,
         smlab = expression("Incidence Rate"),
         leftcols = c("studlab","effect.ci",  "w.random"),
         leftlabs = c("Study","IR95%CI","Weight\n(fixed)", "Weight \n(Random)"),
         rightcols = F,
         lwd = 2)




#Extracting the pooled effect and prediction intervals


IR_impute <- data.frame(IR = IR_meta$TE.random, IR_low = IR_meta$lower.predict, IR_high = IR_meta$upper.predict)  
PR_impute <- data.frame(PR = PR_meta$TE.random, PR_low = PR_meta$lower.predict, PR_high = PR_meta$upper.predict)  



#Inserting random effect value and prediction interval and for missing counties

Asthma_rate <- IR %>% 
  mutate(IR = if_else(is.na(IR)|IR==Inf, IR_impute$IR, IR),
         IR_lower = if_else(is.na(IR_lower)|IR_lower==Inf, IR_impute$IR_low, IR_lower),
         IR_upper = if_else(is.na(IR_upper)|IR_upper==Inf, IR_impute$IR_high, IR_upper),
         PR = if_else(is.na(PR)|PR==Inf, PR_impute$PR, PR),
         PR_lower = if_else(is.na(PR_lower)|PR_lower==Inf, PR_impute$PR_low, PR_lower),
         PR_upper = if_else(is.na(PR_upper)|PR_upper==Inf, PR_impute$PR_high, PR_upper),) 





saveRDS(Asthma_rate, file = "Input/Asthma_rate.RDS" )











