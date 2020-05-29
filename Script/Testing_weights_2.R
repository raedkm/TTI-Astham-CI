


library(survey)


#When using these software products, users must specify that the sample design is “
#With Replacement
#stratum variable (_STSTR), 
#the primary sampling unit (_PSU), 
#weight (FINALWT_F or CHILDWT_F) -- all of which are on the public use data file. 


# ACBS Analysis

ACBS_sample <- ACBS %>% 
  select("X._STATE", "INCIDNT", "CHILDWT_F", "X._CHILDWT", "X._STSTR", "X._PSU" )


mydesign <- 
  svydesign(
    id = ~0 ,
    data = ACBS_sample ,
    weight = ~X._CHILDWT ,
    strata = ~X._STSTR
  )


# CDC code ----------------------------------------------------------------


# Call the library for the current R session
library(survey)

# Read in BRFSS data
load("\\BRFSS\\BRFSS.rdata")

# Subset the data for Louisiana
BRFSS <- BRFSS[BRFSS$state == 22, ]

# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust")

# Create survey design
ACBSdsgn <- svydesign(
  id=~1,
  strata = ~X._STSTR,
  weights = ~CHILDWT_F,
  data = ACBS_sample)

# calculate average number of physical healthy days
svymean(~physhlth, # Variable to anlayze
        brfssdsgn,
        na.rm = TRUE)

# calculate percent in each arthritis category
svymean(~factor(havarth3),
        brfssdsgn,
        na.rm = TRUE)
