region_table <- read.csv("./REGION_NAME.csv", header = TRUE, sep=";", 
                         dec=".", stringsAsFactors = FALSE)
load("prepared.RData")
load("history_ranking.RData")
# separating time serieses by trend, seasonal and random components
require(tsauxfunc)
require(TTR)
require(reshape2)
require(dplyr)
require(ggplot2)

dpg_r_pz <- merge(init_data_final, region_table, 
                  by.x = 'REGION_CODE', by.y = 'REG', sort = FALSE)
dpg_r_pz <- dpg_r_pz[order(dpg_r_pz$TCODE, dpg_r_pz$DATE), ]

# selecting all DPGs from source table, which has history_size >= 24 (>= two years)
prop_history_size <- history_ranking[which(history_ranking$HistorySize >= 24), ]
prop_h_selected <- merge(prop_history_size, init_data_final, 
                        by.x = 'FactorLevel', by.y = 'TCODE', sort = TRUE)  %>% 
                    rename(TCODE = FactorLevel)

# function: devide random component for each group
## t - data.frame to be processed
## group_var - grouping variable of t (must be passed as string in quotes: 'group')
## date_var - date-variable of t (must be passed as string in quotes: 'date')
## ts_var - numeric variable (must be passed as string in quotes: 'value')
separateRandomTSComponent <- function(t, group_var, date_var, ts_var) {
  
  # loading required libraries
  require(xts)
  
  # safe copying
  dpgtable <- t
  dpgtable <- dpgtable[order(dpgtable[ , group_var], dpgtable[ , date_var]), ]
  # main cycle
  for (i in unique(dpgtable[, group_var])) {
    # creating TS-object
    one_piece  <- dpgtable[which(dpgtable[ , group_var] == i), ] 
    one_piece_s <- one_piece[ , c(group_var, date_var, ts_var)]
    print(nrow(one_piece_s))
    starting_point <- min(one_piece_s[ , date_var])
    one_piece_ts <- xts(one_piece_s[ , ts_var], order.by=as.Date(one_piece[ , date_var]))
    one_piece_ts_final <- ts(one_piece_ts, frequency=12, start=starting_point)
    
    # separating random component of the ts-variable
    randomComponent <- decompose(one_piece_ts_final)$random
    dpgtable[which(dpgtable[ , group_var] == i), 'RandomTSComponent'] <- randomComponent 
  }
  
  #rm(one_piece, one_piece_s, starting_point, one_piece_ts,one_piece_ts_final, randomComponent)
  return (dpgtable)
}

######################## ELECTRICITY ########################
# dataset for clustering
for_clustering <- separateRandomTSComponent(prop_h_selected, 'TCODE', 'DATE', 'P_VC_AVG')

# normalizing random component
test <- for_clustering
test$normalizedRandoms <- summarize(group_by(test, TCODE), 
                                    scale(test$RandomTSComponent))

# function: applies given function to grouped data
applyByGroups <- function(t, id_var, group_var, value_var, fun) {
  
  # safe copying
  table <- t
  table <- table[order(table[ , group_var], table[ , id_var]), ]
  # main cycle
  for (i in unique(table[, group_var])) {
    # partitioning by group_var values
    one_piece  <- table[which(table[ , group_var] == i), ] 
    result_var_name <- paste(deparse(substitute(fun)), "results", sep = "_")
    print(result_var_name)
    table[which(table[, group_var] == i), result_var_name] <- as.numeric(fun(one_piece[, value_var]))
  }
  
  #rm(one_piece, one_piece_s, starting_point, one_piece_ts,one_piece_ts_final, randomComponent)
  return (table)
}

for_clustering_normalized <- applyByGroups(for_clustering, 'DATE', 'TCODE', 
                                           'RandomTSComponent', scale)

# CLUSTERING by the distortion of the RANDOM COMPONENTS
require("TSclust")
for_clustering_na_omitted <- na.omit(for_clustering_normalized)
for_clustering_na_omitted <- rename(for_clustering_na_omitted, 
              random_scaled_ee = scale_results, random_EE = RandomTSComponent)

size_62 <- for_clustering_na_omitted[for_clustering_na_omitted$HistorySize == 62, ]
size_62 <- size_62[, c("DATE", "TCODE", "random_scaled")]

library(reshape2)
size_62_wide <- dcast(size_62, value.var = "random_scaled", DATE ~ TCODE)
# beginning from september 2014
size_62_wide_2014 <- size_62_wide[size_62_wide$DATE > as.Date("2014/08/01"), ]
# creating dissimilarity measure
require(xts)

# creating time-series object
toTimeSeries <- function(df, date_var, freq, start_date) {
  require(xts)
  as_ts <- xts(df[ , !colnames(df) %in% c(date_var)], order.by = df[ , date_var])
  result <- ts(as_ts, frequency = freq, start = as.Date(start_date))
  return (result)
}
size_62_wide_ts <- xts(size_62_wide[, -1], order.by=size_62_wide$DATE)
size_62_wide_ts <- ts(size_62_wide_ts, frequency=12, start=c(2011,7))
# beginning from 09-2014
size_62_wide_ts_2014 <- xts(size_62_wide_2014[, -1], order.by=size_62_wide_2014$DATE)
size_62_wide_ts_2014 <- ts(size_62_wide_ts_2014, frequency=12, start=c(2014,9))

# Defining distance - Descrete Wavelete Transformation Method
IP.dis <- diss(size_62_wide_ts, "DTw")
hc_62 <- hclust(d = IP.dis) 
hc_62_cluster <- cutree(hc_62, k = 2)
size_62_df <- data.frame(TCODE = names(hc_62_cluster), 
                           CLUSTER = hc_62_cluster)
size_62_df <- merge(size_62_df, unique(dpg_r_pz[, c("TCODE", "PZ")]), 
                      all.x = TRUE, by = 'TCODE', sort = FALSE)
#beginning from 09-2014
IP.dis_2014 <- diss(size_62_wide_ts_2014, "DTW")
hc_62_2014 <- hclust(d = IP.dis_2014) 
hc_62_cluster_2014 <- cutree(hc_62_2014, k = 2)
size_df_2014 <- data.frame(TCODE = names(hc_62_cluster_2014), 
                           CLUSTER = hc_62_cluster_2014)
size_df_2014 <- merge(size_df_2014, unique(dpg_r_pz[, c("TCODE", "PZ")]), 
                      all.x = TRUE, by = 'TCODE', sort = FALSE)

## Printing some data from size_62
test1 <- size_62[500:1000, ]
ggplot(test1, aes(x = DATE, y = random_scaled)) + 
  geom_line() + facet_wrap(~ TCODE, ncol = 1)


# DPG that are in cluster 1
dpg_cluster1 <- unique(dpg_r_pz[which(dpg_r_pz$TCODE %in% c(names(cluster1))), 
                                c("TCODE", "PZ", "OES")])
# DPG that are in second PZ
dpg_2pz <- unique(dpg_r_pz[which(dpg_r_pz$PZ == 2), c("TCODE", "PZ", "OES")])
dpg_2pz_s62 <- dpg_2pz[dpg_2pz$TCODE %in% unique(size_62$TCODE), ]

# Conclusion: Algorithm of clustering with DWT dissimilarity metric has devided
# data with dpg with history equal to 62 months in two clusters.
# 1 cluster contains 21 DPG 20 of which are in 2PZ.
# Among all DPGs with historySize = 62 there are 21 that are in 2PZ.
# From that we can conclude, that our algorith has clustered DPGs by PriceZone
# with the precision of 20/21 < alpha = 0.05.
# If we use data from 01-09-2014, then clusterization's precision of determination 
# PZ code reduces to 0. 
# Considering that from this date PZ1 and PZ2 has become physically one whole,
# (though we cannot take into consideration the reducing of the history size),
# this fact may serve as a proof, that from this date and further there are no
# sufficient differencies in the dynamic of normalized random component of the
# SVNC_EE.


######################## CAPACITY ##########################

# dataset for clustering
for_clustering_N <- separateRandomTSComponent(prop_h_selected, 'TCODE', 'DATE', 'P_NC_AVG')
for_clustering_normalized_N <- applyByGroups(for_clustering_N, 'DATE', 'TCODE', 
                                           'RandomTSComponent', scale)

# CLUSTERING by the distortion of the RANDOM COMPONENTS
require("TSclust")
for_clustering_na_omitted_N <- na.omit(for_clustering_normalized_N)
for_clustering_na_omitted_N <- rename(for_clustering_na_omitted_N, 
                  random_scaled_n = scale_results, random_N = RandomTSComponent)
prices_data <- cbind(for_clustering_na_omitted, for_clustering_na_omitted_N$random_N)
prices_data <- cbind(prices_data, for_clustering_na_omitted_N$random_scaled_n)
prices_data <- rename(prices_data, 
                  `random_N` = `for_clustering_na_omitted_N$random_N`, 
                  `random_scaled_n` = `for_clustering_na_omitted_N$random_scaled_n`)

# saving data with random_component variable
save(prices_data, file = "prices_data_history_ge_40.RData")

size_62_N <- for_clustering_na_omitted_N[for_clustering_na_omitted_N$HistorySize == 62, ]
size_62_N <- size_62_N[, c("DATE", "TCODE", "random_scaled")]
size_62_N_wide <- dcast(size_62_N, value.var = "random_scaled", DATE ~ TCODE)

size_62_N_wide_ts <- xts(size_62_N_wide[, -1], order.by=size_62_N_wide$DATE)
size_62_N_wide_ts <- ts(size_62_N_wide_ts, frequency=12, start=c(2011,7))
# Defining distance - Descrete Wavelete Transformation Method
IP.dis_N <- diss(size_62_N_wide_ts, "DTW")
hc_62_N <- hclust(d = IP.dis_N) 
hc_62_N_cluster <- cutree(hc_62_N, k = 2)
size_62_N_df <- data.frame(TCODE = names(hc_62_N_cluster), 
                         CLUSTER = hc_62_N_cluster)
size_62_N_df <- merge(size_62_N_df, unique(dpg_r_pz[, c("TCODE", "PZ")]), 
                    all.x = TRUE, by = 'TCODE', sort = FALSE)

test1 <- size_62_N[1:500, ]
par(mfrow = c(1, 1))
ggplot(test1, aes(x = DATE, y = random_scaled)) + 
  geom_line() + facet_wrap(~ TCODE, ncol = 1)

## need to check if there is any additional significant clusters







all_dpgs <- data.frame(DATE = seq(as.Date("2011/7/1"), 
                                  as.Date("2015/8/1"), by = "month"))

library(reshape2)

list_of_df <- list()
count = 0
# preparing list of time-series for futher merging
for (i in unique(for_clustering_na_omitted[, "HistorySize"])) {
  count = count + 1
  # selecting group with HistorySize equals to i
  size_grouped <- for_clustering_na_omitted[for_clustering_na_omitted$HistorySize == i, 
                                            c("DATE", "TCODE", "random_scaled")]
  # transforming dataset to wide format
  size_grouped_wide <- dcast(size_grouped, value.var = "random_scaled", DATE ~ TCODE)
  #all_dpgs <- merge(all_dpgs, size_grouped_wide, by = 'DATE', sort = TRUE)
  list_of_df[[count]] = size_grouped_wide
}






















# !!!!!!!!!!!!!!smth is steadely fucking up!!!!!!!!!!!!!! 

# making list of time-series data
list_of_ts <- list()
count = 0
for(df in list_of_df) {
  count = count + 1
  list_of_ts[[count]] <- toTimeSeries(df, "DATE", 12, "2011/7/1")
}
# clustering each dataset
more_the_10dpgs <- list_of_ts[which(sapply(list_of_ts, function(i) ncol(i) > 15))]
more_the_10dpgs_df <- list_of_df[which(sapply(list_of_df, function(i) ncol(i) > 15))]




IP.dis <- diss(more_the_10dpgs[[5]], "DWT")
oboron <- hclust(d = IP.dis) 
oboron_cluster <- cutree(oboron, k = 2)
oboron_cluster_df <- data.frame(TCODE = names(oboron_cluster), CLUSTER = oboron_cluster)

oboron_cluster_df <- merge(oboron_cluster_df, unique(dpg_r_pz[, c("TCODE", "PZ")]), all.x = TRUE, by = 'TCODE', sort = FALSE)
test_oboron <- dpg_r_pz[dpg_r_pz$TCODE %in% c("POBORO91", "POBORO93"), ]
ggplot(test_oboron, aes(x = DATE, y = P_VC_AVG)) + 
  geom_line() + facet_wrap(~ TCODE, ncol = 1)

plot(more_the_10dpgs_df[[5]]$DATE, more_the_10dpgs_df[[5]]$POBORO91)
plot(more_the_10dpgs_df[[5]]$DATE, more_the_10dpgs_df[[5]]$POBORO93)




dst_na_omited <- list()
IP.dis <- list()
count = 0
for (dst in more_the_10dpgs) {
  count = count + 1
  print(count)
  dst_na_omited[[count]] = na.omit(dst)
  IP.dis[[count]] = diss(dst_na_omited[[count]], "DWT")
  #hc <- hclust(d = IP.dis) 
  #hc_cluster <- cutree(hc, k = 2)
}

# Merging all together
#all_dpgs = Reduce(function(...) merge(..., by = "DATE", all.x=TRUE), list_of_ts)

# transforming dataset to the time-series data format
all_dpgs_wide_ts <- xts(all_dpgs[, -1], order.by=all_dpgs$DATE)
all_dpgs_wide_ts <- ts(all_dpgs_wide_ts, frequency=12, start=c(2011,7)) 
IP.dis <- diss(all_dpgs_wide_ts, "DWT")


