


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
  left_join(ACBS_sample_t, BY = FIPS) %>% 
  mutate(weight_prop = sample_k / sample_t) %>% 
  select(FIPS, year, weight_prop) 


ACBS_final <- ACBS %>% 
  left_join(ACBS_sample, by = c("year", "FIPS")) %>% 
  mutate(new_weight = weight_prop * CHILDWT_F)



# BRFSS 
BRFSS <- map(files_BRFSS, ~.x %>% 
               read_rename_BRFSS %>% 
               select("FIPS", "CASTHDX2", "X_CHILDWT", "PSU", "STSTR") %>% 
               filter(!FIPS  %in% exlude_state)) %>% 
  set_names("Y2006", "Y2007", "Y2008", "Y2009", "Y2010") %>% 
  rbindlist(use.names=TRUE, fill=TRUE, idcol="year") 



#sample
BRFSS_sample_k <- BRFSS %>% 
  group_by(year, FIPS) %>% 
  count() %>% 
  rename(sample_k = n) 

BRFSS_sample_t <- BRFSS_sample_k %>% 
  group_by(FIPS) %>% 
  summarise(sample_t = sum(sample_k)) 

BRFSS_sample <- BRFSS_sample_k %>% 
  left_join(ACBS_sample_t, BY = FIPS) %>% 
  mutate(weight_prop = sample_k / sample_t) %>% 
  select(FIPS, year, weight_prop) 


ACBS_final <- ACBS %>% 
  left_join(ACBS_sample, by = c("year", "FIPS")) %>% 
  mutate(new_weight = weight_prop * CHILDWT_F,
         new_case = INCIDNT==1, 
         case_asthma = new_case * new_weight)




# Survey ------------------------------------------------------------------


options(survey.lonely.psu = "adjust")

# design

ACBS_design <- svydesign(id = ~ 1, strata = ~ year+STSTR, weights = ~ CHILDWT_F,  nest = F, data = ACBS_final)

ACBS_se <- svyby(~ new_case , ~ FIPS, ACBS_design, na.rm = TRUE, svytotal, vartype = "ci") 

summary(ACBS_design)







# Texas -------------------------------------------------------------------

library(haven)


path_tx <- "C:/Users/Raed Dell/Documents/R projects/TTI-AsthmaIR/Input/Texas/tx_multiyrs_weighted.sas7bdat"



texas <- read_sas(path_tx) %>% 
  select(FIPS = `_STATE`, PSU = `_PSU`, new_weight = `_CHILDWT_M_YRS`, INCIDNT, year = survey_year, STSTR = `_STSTR`)
 
sum(texas$new_weight)

ACBS_tx<- ACBS_final %>% 
  filter(FIPS == 48) %>% 
  select(FIPS, PSU , new_weight , INCIDNT, year, STSTR) %>% 
  as_tibble()

sum(ACBS_tx$new_weight)



# design

options(survey.lonely.psu = "adjust")

texas_design <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ new_weight,  nest = F, data = texas)
ACBS_tx_design <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ new_weight,  nest = F, data = ACBS_tx)

summary(texas_design)
summary(ACBS_tx_design)

svyby(~ as.factor(INCIDNT) , ~ FIPS, texas_design, na.rm = TRUE, svymean, vartype = "ci") %>% t()
svyby(~ as.factor(INCIDNT) , ~ FIPS, ACBS_tx_design, na.rm = TRUE, svymean, vartype = "ci") %>% t()

