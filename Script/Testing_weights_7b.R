IR_save <- IR_state_new %>% 
  select(FIPS, ci_l, ci_u) %>% 
  mutate(ci_l = if_else(ci_l <0, 0, ci_l))

PR_save <- PR_state_new_r  %>%  
  select(FIPS, ci_l, ci_u) %>% 
  mutate(ci_l = if_else(ci_l <0, 0, ci_l))


save(IR_save,file="IR_save.Rda")
save(PR_save,file="PR_save.Rda")
