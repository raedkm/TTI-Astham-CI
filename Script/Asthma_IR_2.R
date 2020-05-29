#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (02) Estimating aggregate (2006-2010) astham IR and PR across states 
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: March-12-2019
#Date Updated: Aug-8-2019
#---------------------------------------------#


# Asthma IR Aggregate -----------------------------------------------------



for(i in 2006:2010) {
  
  i <- as.character(i)
  print(i)
  
  
  Asthma_result <- read_excel("Output/Tables/Asthma_IR.xlsx", sheet = i) %>% 
    select(FIPS, State,`<12_month`, `At_risk`) %>% 
    mutate(Year = i) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row


  ifelse(i == "2006",   Asthma_agg <- Asthma_result, Asthma_agg <- bind_rows(Asthma_agg, Asthma_result))
  
  
}


# Aggregating the IR by state for all years 
Asthma_agg_IR <- Asthma_agg %>%
  select(-Year) %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>% 
  as.data.frame()

range(Asthma_agg_IR$`IR per 1000`)



# Aggregating the National IR by year
 Asthma_agg_national_year_IR <- Asthma_agg %>%
  group_by(Year) %>%
  summarise_at(c("<12_month", "At_risk"), sum ) %>%
  mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>%
  as.data.frame()

 
 
 # Aggregating the National IR 
 Asthma_agg_national_IR <- Asthma_agg_national_year_IR %>%
   #group_by(Year) %>%
   summarize(`<12_month` = sum(`<12_month`)/5,
             At_risk = sum(At_risk)/5,
            `IR per 1000` = `<12_month`/ At_risk*1000) %>%
   mutate(Year = "'National") %>% 
   as.data.frame()
 
 
 # Printing Aggregated data to Excel
write.xlsx(Asthma_agg_IR, "Output/Tables/Asthma_IR.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)





# Asthma PR Aggregate -----------------------------------------------------


for(i in 2006:2010) {
  
  i <- as.character(i)
  j <- i
  i <- paste0(i, "_weighted")
  print(i)
  
  Asthma_result <- read_excel("Output/Tables/Asthma_result.xlsx", sheet = i) %>% 
    select(FIPS, State,`1_BRFSS`, `2_BRFSS`) %>% 
    mutate(Year = j) %>%
    mutate(SAMPLE = `1_BRFSS` + `2_BRFSS`) %>% 
    rename(EVER =  `1_BRFSS`) %>%
    select(FIPS, State, EVER, SAMPLE, Year) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row
  ifelse(i == "2006_weighted",   Asthma_agg <- Asthma_result, Asthma_agg <- bind_rows(Asthma_agg, Asthma_result))
  
}

# Aggregating the PR by state for all years 
Asthma_agg_PRV <- Asthma_agg %>%
  select(-Year) %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`PRV per 100` = EVER / SAMPLE*100) %>%
  as.data.frame()

range(Asthma_agg_PRV$`PRV per 100`)



# Aggregating the National PR by years
Asthma_agg_national_year_PRV <- Asthma_agg %>%
  group_by(Year) %>%
  summarise_at(c("EVER", "SAMPLE"), sum ) %>%
  mutate(`PRV per 100` = EVER / SAMPLE*100) %>%
  as.data.frame()



# Aggregating the National PR 
Asthma_agg_national_PRV <- Asthma_agg_national_year_PRV %>%
  #group_by(Year) %>%
  summarize(EVER = sum(EVER)/5,
            SAMPLE = sum(SAMPLE)/5,
            `PRV per 100` = EVER / SAMPLE*100) %>%
  mutate(Year = "'National") %>% 
  as.data.frame()



# Printing Aggregated data to Excel
write.xlsx(Asthma_agg_PRV, "Output/Tables/Asthma_PR.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)





# Creating tables ---------------------------------------------------------


# Printing Table_A1; Asthma survey summaries by year
table_IR <- Asthma_agg_national_IR %>% 
  rbind(Asthma_agg_national_year_IR) 

table_PRV <- Asthma_agg_national_PRV %>% 
  rbind(Asthma_agg_national_year_PRV) 

names <- table_IR$Year

Table_A1 <- table_IR %>% 
  left_join(table_PRV, by = "Year") %>% 
  select(SAMPLE, EVER, `<12_month`, At_risk, `IR per 1000`, `PRV per 100`) %>% 
  t()

colnames(Table_A1) <- names 

write.xlsx(Table_A1, "Output/Tables/Table_A1.xlsx",  append = F, row.names = T)



# Printing Table_A2; Asthma survey summeries by state

Table_A2 <- Asthma_agg_IR %>% 
  left_join(Asthma_agg_PRV, by = c("FIPS", "State")) %>% 
  select(State, SAMPLE, EVER, `<12_month`, At_risk, `IR per 1000`, `PRV per 100`)

write.xlsx(Table_A2, "Output/Tables/Table_A2.xlsx",  append = F, row.names = F)


