#### loading data ####
# SVNC
load("SVNC_P_V_KOM_FACT.RData")

#### transforming data from long to wide format ####
pabaksb1_data <- svnc_p_v_kom[svnc_p_v_kom$TCODE == 'PDAGENER', ]
pabaksb1_data <- pabaksb1_data[order(pabaksb1_data$TDATE), ]
# creating lag-variables
library(dplyr)
#pabaksb1_data$P_VC_UNREG_AVG_L1 <- lag(pabaksb1_data$P_VC_UNREG_AVG, k = 1)
pabaksb1_data$P_VC_UNREG_AVG_L2 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 2)
pabaksb1_data$P_VC_UNREG_AVG_L3 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 3)
pabaksb1_data$P_VC_UNREG_AVG_L4 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 4)
pabaksb1_data$P_VC_UNREG_AVG_L11 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 11)
pabaksb1_data$P_VC_UNREG_AVG_L12 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 12)
pabaksb1_data$P_VC_UNREG_AVG_L13 <- lag(pabaksb1_data$P_VC_UNREG_AVG, 13)
#pabaksb1_data$P_NC_UNREG_AVG_L1 <- lag(pabaksb1_data$P_NC_UNREG_AVG, k = 1)
pabaksb1_data$P_NC_UNREG_AVG_L2 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 2)
pabaksb1_data$P_NC_UNREG_AVG_L3 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 3)
pabaksb1_data$P_NC_UNREG_AVG_L4 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 4)
pabaksb1_data$P_NC_UNREG_AVG_L11 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 11)
pabaksb1_data$P_NC_UNREG_AVG_L12 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 12)
pabaksb1_data$P_NC_UNREG_AVG_L13 <- lag(pabaksb1_data$P_NC_UNREG_AVG, 13)

test_data <- pabaksb1_data[, -10]
test_data <- test_data[test_data$TDATE == as.Date('2016-08-01'), ]

# deleting august for testing quality of forecasting
pabaksb1_data <- pabaksb1_data[pabaksb1_data$TDATE < as.Date('2016-07-01'), ]

# extracting everything except numerics
pabaksb1_data_ids <- pabaksb1_data[, c(1:8, 11, 13)]
# leaving the top of the lagged table with NA
pabaksb1_data_na <- pabaksb1_data[which(is.na(rowSums(pabaksb1_data[, 
                                          10:ncol(pabaksb1_data)]))), ]
pabaksb1_data_na <- pabaksb1_data_na$P_NC_UNREG_AVG
# deleting rows with NAs
pabaksb1_data_nona <- na.omit(pabaksb1_data)

#### building iterated boost model ####
library(mboost)
pabaksb1_boost_model <- glmboost(P_NC_UNREG_AVG ~ P_NC_UNREG_AVG_L2 + 
                                   P_NC_UNREG_AVG_L3 + P_NC_UNREG_AVG_L4 + 
                                   P_NC_UNREG_AVG_L12 + PIKE_FACT + KOM_PRICE, 
                                 data = pabaksb1_data_nona, control = boost_control(mstop = 5000))
pabaksb_fitted <- pabaksb1_boost_model$fitted()

# main boosting cycle
for (i in 1:500) {
  new_values <- c(pabaksb1_data_na, pabaksb_fitted)
  new_train_df <- pabaksb1_data_ids
  new_train_df$P_NC_UNREG_AVG <- new_values
  new_train_df$P_NC_UNREG_AVG_L2 <- lag(new_train_df$P_NC_UNREG_AVG, 2)
  new_train_df$P_NC_UNREG_AVG_L3 <- lag(new_train_df$P_NC_UNREG_AVG, 3)
  new_train_df$P_NC_UNREG_AVG_L4 <- lag(new_train_df$P_NC_UNREG_AVG, 4)
  new_train_df$P_NC_UNREG_AVG_L11 <- lag(new_train_df$P_NC_UNREG_AVG, 11)
  new_train_df$P_NC_UNREG_AVG_L12 <- lag(new_train_df$P_NC_UNREG_AVG, 12)
  new_train_df$P_NC_UNREG_AVG_L13 <- lag(new_train_df$P_NC_UNREG_AVG, 13)
  new_train_df <- na.omit(new_train_df)
  new_boost <- glmboost(P_NC_UNREG_AVG ~ P_NC_UNREG_AVG_L2 + 
                          P_NC_UNREG_AVG_L3 + P_NC_UNREG_AVG_L4 + 
                          P_NC_UNREG_AVG_L12 + PIKE_FACT + KOM_PRICE, 
                        data = new_train_df, control = boost_control(mstop = 1800))
  pabaksb_fitted <- new_boost$fitted()
  print(i)
}

result_model <- new_boost

predictions <- predict(new_boost, newdata = test_data, type = "response")
