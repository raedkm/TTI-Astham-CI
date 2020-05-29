ACBS6 <-ACBS %>% 
  filter(year == "Y2006")

ACBS7 <-ACBS %>% 
  filter(year == "Y2007")

ACBS8 <-ACBS %>% 
  filter(year == "Y2008")

ACBS9 <-ACBS %>% 
  filter(year == "Y2009")

ACBS10 <-ACBS %>% 
  filter(year == "Y2010")

ds6 <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS6)
ds7 <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS7)
ds8 <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS8)
ds9 <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS9)
ds10 <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS10)

Y2006 <- svyby(~ INCIDNT , ~ FIPS, ds6, na.rm = TRUE, svytotal, vartype = "se") %>% mutate(year = "Y2006")
Y2007 <- svyby(~ INCIDNT , ~ FIPS, ds7, na.rm = TRUE, svytotal, vartype = "se")%>% mutate(year = "Y2007")
Y2008 <- svyby(~ INCIDNT , ~ FIPS, ds8, na.rm = TRUE, svytotal, vartype = "se") %>% mutate(year = "Y2008")
Y2009 <- svyby(~ INCIDNT , ~ FIPS, ds9, na.rm = TRUE, svytotal, vartype = "se") %>% mutate(year = "Y2009")
Y2010 <- svyby(~ INCIDNT , ~ FIPS, ds10, na.rm = TRUE, svytotal, vartype = "se") %>% mutate(year = "Y2010")


asthma <- rbind(Y2006, Y2007, Y2008, Y2009, Y2010)

asthma_k <- asthma %>%
  mutate(n = 1) %>% 
  group_by(FIPS) %>% 
  summarize(k = sum(n), 
            cases = sum(INCIDNT)/k, 
            se = sum(se)/k)



# One file ----------------------------------------------------------------

ACBS

ds <- svydesign(id = ~ PSU, strata = ~ year + STSTR, weights = ~ CHILDWT_F,  nest = TRUE, data = ACBS)

summary(ds)

ds_v <- svyby(~ INCIDNT , ~ FIPS+year, ds, na.rm = TRUE, svytotal, vartype = "se") 

ds_v %>% 
  arrange(FIPS) %>% 
  group_by(FIPS) %>% 
  summarise(case = mean(INCIDNT), 
            se = mean(se))

