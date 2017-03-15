# loading initial data 

## setting working directory
setwd("C:/!zemskov/SVNCForecast/data_sources")

## loading useful libraries
require("Hmisc")
require("dplyr")

## loading data
init_data <- read.csv("./SVNC_FACT_DETAILED.csv", header = TRUE, sep=";", 
                      dec=".", stringsAsFactors = FALSE)

## preaparing data for analysis
init_data_filtered <- subset(x = init_data, select=c("TDATE", "PCODE", 
  "PNAME", "TCODE", "REGION_CODE", "P_VC_UNREG_AVG", "P_NC_UNREG_AVG"))
init_data_prepared <- mutate(.data=init_data_filtered, REGION_CODE = 
                      as.factor(REGION_CODE), PCODE = as.factor(PCODE), 
                      TCODE = as.factor(TCODE))
init_data_prepared$DATE <- as.Date(init_data_prepared$TDATE)
init_data_prepared <- subset(x = init_data_prepared, select = -TDATE)

## dealing with NA's
## function, that imputing NA's with median vaues in each group
imputeMean <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which(filter.var == v)] <- impute(impute.var[
      which(filter.var == v)
    ], fun = mean)
  }
  return (impute.var)
}

## imputing numeric variables
## 1. creating grouping variable
init_data_prepared$groupVar <- paste(init_data_prepared$PCODE,
          as.character(init_data_prepared$DATE), 
          as.character(init_data_prepared$REGION_CODE))

## 2. imputing NA's with means by groupVar

## replacing all P_VC_UNREG_AVG with strangely high values
init_data_prepared$P_VC_UNREG_AVG[
  which(init_data_prepared$P_VC_UNREG_AVG > 2500)] <- NA

## imputing ..
init_data_prepared$P_VC_AVG <- imputeMean(init_data_prepared$P_VC_UNREG_AVG, 
                     init_data_prepared$groupVar, init_data_prepared$groupVar)

## replacing all neagative P_NC_UNREG_AVG with NAs
init_data_prepared$P_NC_UNREG_AVG[
  which(init_data_prepared$P_NC_UNREG_AVG < 0)] <- NA

## imputing ..
init_data_prepared$P_NC_AVG <- imputeMean(init_data_prepared$P_NC_UNREG_AVG, 
                                          init_data_prepared$groupVar, init_data_prepared$groupVar)

## 3. some P_VC_UNREG_AVG values are still NA, so, first, need to 
## change then on NA. (Several P_NC_UNREG_AVG values are < 0)

## changing grouping variable
init_data_prepared$groupVar <- paste(init_data_prepared$PCODE,
                                     as.character(init_data_prepared$DATE))

## imputing ..
## VC
init_data_prepared$P_VC_AVG <- imputeMean(init_data_prepared$P_VC_AVG, 
                                          init_data_prepared$groupVar, init_data_prepared$groupVar)
## NC
init_data_prepared$P_NC_AVG <- imputeMean(init_data_prepared$P_NC_AVG, 
                                          init_data_prepared$groupVar, init_data_prepared$groupVar)
## Subsetting resulting dataset
init_data_final <- subset(x = init_data_prepared, select = 
                               -c(groupVar, P_VC_UNREG_AVG, P_NC_UNREG_AVG))

## saving dataset to file
save(init_data_final, file = "prepared.RData")

# cleaning memory
rm(init_data)
rm(init_data_filtered)