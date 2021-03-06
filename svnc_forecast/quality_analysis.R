# clearing workspace
rm(list = ls())
# loading fact data
load("SVNC_P_V_KOM_FACT.RData")
regions <- read.csv('C:/!zemskov/svnc_forecast/data_sources/REGION_NAME.csv'
                    ,sep = ';', stringsAsFactors = FALSE)
fact_data <- svnc_p_v_kom
#rm(svnc_p_v_kom)
comp_month <- "2017-01-01"
#fact_data$TDATE <- as.Date(fact_data$TDATE)
fact_data <- fact_data[fact_data$TDATE == as.Date(comp_month), ]
#fact_data_filtered <- fact_data[fact_data$TDATE == as.Date(comp_month), 
#      c("TDATE", "PCODE", "TCODE", "REGION_CODE", "P_VC_UNREG_AVG", "P_NC_UNREG_AVG")]
fact_data$N_COST <- fact_data$P_NC_UNREG_AVG * fact_data$PIKE_FACT
fact_data$EE_COST <- fact_data$P_VC_UNREG_AVG * fact_data$VOLUME
fact_data_filtered <- fact_data[, c("TDATE", "PCODE", "REGION_CODE", "N_COST", 
                                    "EE_COST", "PIKE_FACT", "VOLUME")]
library(dplyr)
fact_data_grouped <- summarize(group_by(fact_data_filtered, TDATE, PCODE,  REGION_CODE), 
                               SVNC_M_FACT = sum(N_COST)/sum(PIKE_FACT), 
                               SVNC_EE_FACT = sum(EE_COST)/sum(VOLUME))
fact_data_grouped <- fact_data_grouped[, c("PCODE", "REGION_CODE", "SVNC_M_FACT", "SVNC_EE_FACT")]
fact_data_grouped$REGION_CODE <- as.numeric(fact_data_grouped$REGION_CODE)
fact_data_grouped$PCODE <- as.character(trimws(fact_data_grouped$PCODE, which = "both"))
# loading forecasts
library(xlsx)
fcst_data <- read.xlsx(file = "C:/!zemskov/svnc_forecast/data_sources/january_2017_quality_report(weighted).xlsx", 
                       sheetName = "SVNC_M_ee", encoding = 'UTF-8')
# names(fcst_data) <- c('id', 'PCODE', 'REGION_NAME', 'PRICE_N', 'PRICE_EE')
# fcst_data$PCODE <- as.character(trimws(fcst_data$PCODE, which = "both"))
# fcst_data$REGION_NAME <- as.character(trimws(fcst_data$REGION_NAME, which = "both"))

# fcst_data <- merge(fcst_data, regions[, c('REG', 'REG_NAME')], 
#                    by.x = c('REGION_NAME'), by.y = c('REG_NAME'), all.x = TRUE)
# fcst_data <- select(fcst_data, PCODE, REG, REGION_NAME, PRICE_EE, PRICE_N)
# names(fcst_data) <- c('PCODE', 'REGION_CODE', 'REGION_NAME', 'PRICE_EE', 'PRICE_N')
fcst_data$REGION_CODE <- as.numeric(as.character(fcst_data$REGION_CODE))
#fcst_data <- fcst_data[, c("PCODE", "REGION_CODE", "PRICE")]
plan_fact <- merge(fcst_data, fact_data_grouped, 
                   by = c('PCODE', 'REGION_CODE'), all.x = TRUE, sort = TRUE)
quality <- na.omit(plan_fact)
quality$error_M <- quality$PRICE_N - quality$SVNC_M_FACT
quality$abs_error_M <- abs(quality$PRICE_N - quality$SVNC_M_FACT)
quality$pcnt_error_M <- quality$abs_error_M / quality$SVNC_M_FACT * 100
quality$MAE_M <- mean(quality$abs_error_M)
quality$MAE_pcnt_M <- mean(quality$pcnt_error_M)
quality$median_error_M <- median(quality$abs_error_M)
quality$pcnt_median_error_M <- median(quality$pcnt_error_M)

quality$error_EE <- quality$PRICE_EE - quality$SVNC_EE_FACT
quality$abs_error_EE <- abs(quality$PRICE_EE - quality$SVNC_EE_FACT)
quality$pcnt_error_EE <- quality$abs_error_EE / quality$SVNC_EE_FACT * 100
quality$MAE_EE <- mean(quality$abs_error_EE)
quality$MAE_pcnt_EE <- mean(quality$pcnt_error_EE)
quality$median_error_EE <- median(quality$abs_error_EE)
quality$pcnt_median_error_EE <- median(quality$pcnt_error_EE)
write.xlsx(quality, "january_2017_quality_report(weighted).xlsx")
