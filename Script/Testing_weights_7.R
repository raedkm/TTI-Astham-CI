
#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Purpose: Conduct the burden modeling and estimate results by state
#Part : (T-01) Creating new weigths 
#Created by Raed Alotaibi
#Date Created: April-24-2020
#Last modified: April-24-2020
#---------------------------------------------#

Sys.time()

# Houskeepingcitation
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
  mutate(k = 1) %>% 
  as_tibble()


BRFSS_sample_k <- BRFSS %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  mutate(k = 1) %>% 
  as_tibble()


ACBS_BRFSS_sample_filter <- inner_join(ACBS_sample_k, BRFSS_sample_k,by = c("FIPS", "year"), suffix = c("_ACBS", "_BRFSS")) %>% 
  group_by(year, FIPS) %>% 
  count()


ACBS_BRFSS_sample_k <- ACBS_BRFSS_sample_filter %>% 
  group_by(FIPS) %>% 
  summarise(k = sum(n)) %>% 
  as_tibble() 


ACBS_final <- ACBS %>% 
  inner_join(ACBS_BRFSS_sample_filter, by = c("FIPS", "year")) %>%
  left_join(ACBS_BRFSS_sample_k, by = "FIPS") %>% 
  mutate(new_weight = CHILDWT_F / k,
         new_case = factor(if_else(INCIDNT == 1, 1, 0))) 


BRFSS_final <- BRFSS %>% 
  inner_join(ACBS_BRFSS_sample_filter, by = c("FIPS", "year")) %>%
  left_join(ACBS_BRFSS_sample_k, by = "FIPS") %>%
  mutate(new_weight =  X_CHILDWT / k) %>% 
  filter(!is.na(new_weight)) %>% 
  as_tibble()




# Merging -----------------------------------------------------------------



# Merging data sets

join_asthm <- full_join(ACBS_final, BRFSS_final, by = c("year", "FIPS", "STSTR", "PSU"), suffix = c("_ACBS", "_BRFSS")) %>% 
  mutate(type = ifelse(is.na(type_ACBS), "BRFSS",  "ACBS"),
         k = if_else(type == "ACBS", k_ACBS, k_BRFSS),
         weight = if_else(type == "ACBS", CHILDWT_F, X_CHILDWT),
         new_weight = weight / k,
         CASTHDX2 = if_else(is.na(CASTHDX2), 0, CASTHDX2), 
         INCIDNT = if_else(is.na(INCIDNT), 0, INCIDNT), 
         new_case = if_else(INCIDNT == 1 , 1, 0),
         at_risk = if_else(( INCIDNT == 1 | CASTHDX2 == 2 ) , 1, 0),
         cases_w = new_case * weight,
         at_risk_w =  at_risk * weight) %>% 
  select(-n_BRFSS, -n_ACBS, -X._CHILDWT,  -new_weight_BRFSS, -new_weight_ACBS, -type_ACBS, -type_BRFSS, -k_ACBS, -k_BRFSS) %>% 
  as_tibble() 



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
         at_risk_w =  at_risk * new_weight,
         ever_asthma = if_else((INCIDNT > 0 | CASTHDX2 == 1 | CASTHDX2 > 2) ,1,0) ,
         n = 1) %>% 
  select(-n_BRFSS, -n_ACBS, -X._CHILDWT,  -new_weight_BRFSS, -new_weight_ACBS, -type_ACBS, -type_BRFSS, -k_ACBS, -k_BRFSS) %>% 
  as_tibble() 



# Cross checking merged with data sets  ------------------------------------

IR <- join_asthm_new %>% 
  group_by(FIPS) %>% 
  summarise(cases = sum(cases_w, na.rm = T),
            at_risk = sum(at_risk_w, na.rm = T)) %>% 
  mutate(IR = cases/at_risk *1000) 



IR_national_new <-  join_asthm_new %>% 
  summarise(cases = sum(cases_w, na.rm = T),
            at_risk = sum(at_risk_w, na.rm = T)) %>% 
  mutate(IR = cases/(at_risk) *1000) 


join_asthm_new %>% 
  group_by(year) %>% 
  summarise(eve = sum(ever_asthma),
            all = sum(n),
            w = sum(weight), 
            nw = sum(new_weight)) %>% 
  t()



# design ------------------------------------------------------------------

options(survey.lonely.psu = "adjust")


ashtm_design <- svydesign(id = ~ PSU, strata = ~ STSTR, weights = ~ weight,   nest = TRUE, data = join_asthm) # Original weights
ashtm_design_new <- svydesign(id = ~ PSU, strata = ~ STSTR, weights = ~ new_weight,   nest = TRUE, data = join_asthm) # New weights


# Running survey function -------------------------------------------------

#By state
#IR_state_old <- svyby(~ new_case, ~FIPS ,  denominator = ~at_risk, ashtm_design, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 


# IR_state_old_print <- IR_state_old %>% 
#   mutate(IR = round(`new_case/at_risk`*1000, 1),
#          SE = round(`se.new_case/at_risk`*1000, 1),
#          Relative_SE = paste0(round(`se.new_case/at_risk`/`new_case/at_risk`*100, 0), "%"),
#          Relative_SE_i = if_else(SE/IR*100 >30, "High", "Low"),
#          CI_lower = round(ci_l, 4)*1000,
#          CI_upper = round(ci_u, 4)*1000,
#          copy = paste0(IR, "(", CI_lower, "-", CI_upper, ")")) %>%
#   left_join(FIPS, by = "FIPS")  %>% 
#   select(FIPS, State, IR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper, copy)  


IR_state_new <- svyby(~ new_case, ~FIPS ,  denominator = ~at_risk, ashtm_design_new, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 


IR_state_new_print <- IR_state_new %>% 
  mutate(IR = round(`new_case/at_risk`*1000, 1),
         SE = round(`se.new_case/at_risk`*1000, 1),
         Relative_SE = paste0(round(`se.new_case/at_risk`/`new_case/at_risk`*100, 0), "%"),
         Relative_SE_i = if_else(SE/IR*100 >30, "High", "Low"),
         CI_lower = round(ci_l, 4)*1000,
         CI_upper = round(ci_u, 4)*1000, 
         copy = paste0(IR, "(", CI_lower, "-", CI_upper, ")")) %>%
  left_join(FIPS, by = "FIPS")  %>% 
  select(FIPS, State, IR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper, copy)  


#National average By year
#IR_year_old <- svyby(~ new_case, ~year ,  denominator = ~at_risk, ashtm_design, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 


# IR_year_old_print <- IR_year_old %>% 
#   mutate(IR = round(`new_case/at_risk`*1000, 1),
#          SE = round(`se.new_case/at_risk`*1000, 1),
#          Relative_SE = paste0(round(`se.new_case/at_risk`/`new_case/at_risk`*100, 0), "%"),
#          Relative_SE_i = if_else(SE/IR*100 >30, "High", "Low"),
#          CI_lower = round(ci_l, 4)*1000,
#          CI_upper = round(ci_u, 4)*1000,
#          copy = paste0(IR, "(", CI_lower, "-", CI_upper, ")")) %>%
#   select(year, IR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper, copy)  


IR_year_new <- svyby(~ new_case, ~year ,  denominator = ~at_risk, ashtm_design_new, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 

IR_year_new_print <- IR_year_new %>% 
  mutate(IR = round(`new_case/at_risk`*1000, 1),
         SE = round(`se.new_case/at_risk`*1000, 1),
         Relative_SE = paste0(round(`se.new_case/at_risk`/`new_case/at_risk`*100, 0), "%"),
         Relative_SE_i = if_else(SE/IR*100 >30, "High", "Low"),
         CI_lower = round(ci_l, 4)*1000,
         CI_upper = round(ci_u, 4)*1000,
         copy = paste0(IR, "(", CI_lower, "-", CI_upper, ")")) %>%
  select(year, IR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper)  


#Natioanl average (by states using weighted IR (i.e. weighted_state_IR = IR / k years of available data))
#IR_old <- svyratio(~ new_case ,  denominator = ~at_risk, ashtm_design, na.rm = TRUE, vartype = c("se", "ci")) 
IR_new <- svyratio(~ new_case ,  denominator = ~at_risk, ashtm_design_new, na.rm = TRUE, vartype = c("se", "ci")) 

low <- round((IR_new$ratio - IR_new$var*1.96)*1000, 2)
up <- round((IR_new$ratio + IR_new$var*1.96)*1000, 2)
paste0(round(IR_new$ratio*1000, 1), "(",low, "-", up, ")" )


#National average By year and state
IR_state_year_new <- svyby(~ new_case, ~FIPS+year ,  denominator = ~at_risk, ashtm_design_new, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 


IR_state_year_new_print <- IR_state_year_new %>% 
  #group_by(FIPS) %>% 
  select(year, FIPS,`new_case/at_risk`) %>% 
  mutate(IR = round(`new_case/at_risk`*1000, 1)) 
  spread(year, IR)  





# Prevelance --------------------------------------------------------------


t<- BRFSS %>% 
  mutate(asthma = if_else(CASTHDX2 ==1, 1, 0),
         sample = if_else(CASTHDX2 ==1 | CASTHDX2 ==2, 1, 0))

BRFSS_design <- svydesign(id = ~ PSU, strata = ~ STSTR, weights = ~ X_CHILDWT,   nest = TRUE, data = t) # Original weights


PR_state_new_r <- svyby(~ asthma , ~FIPS , denominator = ~sample,  BRFSS_design, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 

PR_state_new_r_print <- PR_state_new_r %>% 
  mutate(PR = round(`asthma/sample`*100, 1),
         SE = round(`se.asthma/sample`*100, 1),
         Relative_SE = paste0(round(SE/PR*100, 1), "%"),
         Relative_SE_i = if_else(SE/PR*100 >30, "High", "Low"),
         CI_lower = round(ci_l, 3)*100,
         CI_upper = round(ci_u, 3)*100,
         copy = paste0(PR, "(", CI_lower, "-", CI_upper, ")")) %>%
  left_join(FIPS, by = "FIPS")  %>% 
  select(FIPS, State, PR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper, copy)



PR_year_new_r <- svyby(~ asthma , ~year , denominator = ~sample,  BRFSS_design, na.rm = TRUE, svyratio, vartype = c("se", "ci")) 


PR_year_new_r_print <-  PR_year_new_r %>% 
mutate(PR = round(`asthma/sample`*100, 1),
       SE = round(`se.asthma/sample`*100, 1),
       Relative_SE = paste0(round(SE/PR*100, 1), "%"),
       Relative_SE_i = if_else(SE/PR*100 >30, "High", "Low"),
       CI_lower = round(ci_l, 3)*100,
       CI_upper = round(ci_u, 3)*100,
       copy = paste0(PR, "(", CI_lower, "-", CI_upper, ")")) %>%
  select(year, PR , SE , Relative_SE, Relative_SE_i, CI_lower, CI_upper, copy)  



PR_new <- svyratio(~ asthma ,  denominator = ~sample, BRFSS_design, na.rm = TRUE, vartype = c("se", "ci")) 

low <- round((PR_new$ratio - PR_new$var*1.96)*100, 1)
up <- round((PR_new$ratio + PR_new$var*1.96)*100, 1)
paste0(round(PR_new$ratio*100, 1), "(",low, "-", up, ")" )

# Writing results to excel ------------------------------------------------


IR_new
IR_year_new_print
IR_state_new_print
IR_state_year_new_print
PR_state_new_r_print
PR_year_new_r_print

write.xlsx(IR_state_old_print, "Output/Tables/IR_state_se.xlsx", showNA=F, append = T, row.names = F)
write.xlsx(IR_year_old_print, "Output/Tables/IR_year_se.xlsx",  showNA=F, append = T, row.names = F)
write.xlsx(PR_state_new_r_print, "Output/Tables/PR_state_se.xlsx",  showNA=F, append = T, row.names = F)
write.xlsx(PR_year_new_r_print, "Output/Tables/PR_year_se.xlsx",  showNA=F, append = T, row.names = F)





