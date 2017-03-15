load(file = "C:/!zemskov/Deviations/DeviationsAnalysis/gp_data.RData")
load(file = "C:/!zemskov/Deviations/DeviationsAnalysis/non_gp_data.RData")
load(file = "C:/!zemskov/Deviations/DeviationsAnalysis/sn_data.RData")

#### GP ####
# creating similarity dataframe
gp_data_hist <- summarize(group_by(gp_data, gtp, oes), 
                          total_disqual = sum(is_disqualified),
                          history = length(gtp),
                          size = mean(total_fact))

# deleting those, who has changed oes

gp_data_hist <- gp_data_hist %>% 
  group_by(gtp) %>% mutate(count = length(gtp)) %>% filter(count == 1)
gp_data_hist <- gp_data_hist[, c("gtp", "oes", "total_disqual", "history", "size")]

# separating healthy from sick ones
gp_data_healthy <- as.data.frame(filter(gp_data_hist, total_disqual == 0))
gp_data_sick <- as.data.frame(filter(gp_data_hist, total_disqual > 0))

# creating empty dataframe for the accumulation of NNs
acc_nearests <- data.frame(sick_gtp = character(), healthy_gtp = character())

find_nn <- function(df1, df2, g) {
  message(paste("Searching for the nearest neighbor for", g, sep = " "))
  require(RANN)
  df2a <- df2[, -c(1)]
  row <- df1[df1$gtp == g, ]
  row <- row[, -c(1)]
  oes_sick = as.integer(row[1, 1])
  healthy_oes <- df2a[df2a$oes == oes_sick, ]
  frame <- rbind(row, healthy_oes)
  nearests <- as.data.frame(nn2(frame, k = 2))
  nearest <- as.integer(nearests[1, 2])

  nearest_to_join <- merge(df2, frame[nearest, ], 
                           by = c("oes", "total_disqual", "history", "size"))
  result <- data.frame(sick_gtp = g, healthy_gtp = 
                         nearest_to_join[, c("gtp")])

  return (result)
}

for (g in unique(gp_data_sick$gtp)) {
  res <- find_nn(gp_data_sick, gp_data_healthy, g)
  acc_nearests <- rbind(acc_nearests, res)
}

save(acc_nearests, file = "gp_nearest_healthy.RData")

# healthy to join to sick
healthy_to_join <- gp_data_healthy[gp_data_healthy$gtp %in% acc_nearests$healthy_gtp, ]
gp_data_sick <- merge(gp_data_sick, acc_nearests, 
                      by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

gp_data_s <- gp_data[gp_data$gtp %in% gp_data_sick$gtp, ]
gp_data_s <- merge(gp_data_s, acc_nearests, 
                   by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

gp_data_h <- gp_data[gp_data$gtp %in% gp_data_healthy$gtp, ]

gp_data_res <- merge(gp_data_s, gp_data_h[, c("gtp", "month", "op1_indicator")], 
                by.x = c("healthy_gtp", "month"), by.y = c("gtp", "month"), all.x = TRUE)

gp_data_res <- rename(gp_data_res, healthy_ind = op1_indicator.y)
gp_data_res <- rename(gp_data_res, sick_ind = op1_indicator.x)

gp_data_res <- na.omit(gp_data_res)

gp_data_res$target <- gp_data_res$sick_ind - gp_data_res$healthy_ind

save(gp_data_res, file = 'C:/!zemskov/Deviations/DeviationsAnalysis/gp_data_result.RData')

if_negative <- function(i) {
  return(max(0, i))
}
gp_data_res$target <- sapply(gp_data_res$target, if_negative)

gp_data_res_nonzero <- gp_data_res[gp_data_res$gtp %in% unique(gp_data_res[gp_data_res$target != 0, ]$gtp), ]

corrs_gp <- vector()
for (g in unique(gp_data_res_nonzero$gtp)) {
  frame <- gp_data_res_nonzero[gp_data_res_nonzero$gtp == g, ]
  corrs_gp <- append(corrs_gp, cor(frame$disqual_count, frame$target))
}
corrs_gp <- na.omit(corrs_gp) # 71 observations

qplot(corrs_gp, geom = "histogram", main = "Распределение корреляций для ГП")

# t-testing agains > 0
t.test(corrs_gp, alternative = "less", mu = 0)
#One Sample t-test

#data:  corrs_gp
#t = -2.1875, df = 52, p-value = 0.01661
#alternative hypothesis: true mean is less than 0
#95 percent confidence interval:
#  -Inf -0.0232058
#sample estimates:
#  mean of x 
#-0.09899462 

#### NON GP ####
#### GP ####
# creating similarity dataframe
non_gp_data_hist <- summarize(group_by(non_gp_data, gtp, oes), 
                          total_disqual = sum(is_disqualified),
                          history = length(gtp),
                          size = mean(total_fact))

# deleting those, who has changed oes

non_gp_data_hist <- non_gp_data_hist %>% 
  group_by(gtp) %>% mutate(count = length(gtp)) %>% filter(count == 1)
non_gp_data_hist <- non_gp_data_hist[, c("gtp", "oes", "total_disqual", "history", "size")]

# separating healthy from sick ones
non_gp_data_healthy <- as.data.frame(filter(non_gp_data_hist, total_disqual == 0))
non_gp_data_sick <- as.data.frame(filter(non_gp_data_hist, total_disqual > 0))

# creating empty dataframe for the accumulation of NNs
non_gp_acc_nearests <- data.frame(sick_gtp = character(), healthy_gtp = character())


for (g in unique(non_gp_data_sick$gtp)) {
  res <- find_nn(non_gp_data_sick, non_gp_data_healthy, g)
  non_gp_acc_nearests <- rbind(non_gp_acc_nearests, res)
}

save(non_gp_acc_nearests, file = "non_gp_nearest_healthy.RData")

# healthy to join to sick
non_gp_healthy_to_join <- non_gp_data_healthy[non_gp_data_healthy$gtp 
                                              %in% non_gp_acc_nearests$healthy_gtp, ]
non_gp_data_sick <- merge(non_gp_data_sick, non_gp_acc_nearests, 
                      by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

non_gp_data_s <- non_gp_data[non_gp_data$gtp %in% non_gp_data_sick$gtp, ]
non_gp_data_s <- merge(non_gp_data_s, non_gp_acc_nearests, 
                   by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

non_gp_data_h <- non_gp_data[non_gp_data$gtp %in% non_gp_data_healthy$gtp, ]

non_gp_data_res <- merge(non_gp_data_s, non_gp_data_h[, c("gtp", "month", "op2_indicator")], 
                     by.x = c("healthy_gtp", "month"), by.y = c("gtp", "month"), all.x = TRUE)

non_gp_data_res <- rename(non_gp_data_res, healthy_ind = op2_indicator.y)
non_gp_data_res <- rename(non_gp_data_res, sick_ind = op2_indicator.x)

non_gp_data_res <- na.omit(non_gp_data_res)

non_gp_data_res$target <- non_gp_data_res$sick_ind - non_gp_data_res$healthy_ind

non_gp_data_res$target <- sapply(non_gp_data_res$target, if_negative)

non_gp_data_res_nonzero <- non_gp_data_res[non_gp_data_res$gtp 
                                           %in% unique(non_gp_data_res[non_gp_data_res$target != 0, ]$gtp), ]

corrs_non_gp <- vector()
for (g in unique(non_gp_data_res_nonzero$gtp)) {
  frame <- non_gp_data_res_nonzero[non_gp_data_res_nonzero$gtp == g, ]
  corrs_non_gp <- append(corrs_non_gp, cor(frame$disqual_count, frame$target))
}
corrs_non_gp <- na.omit(corrs_non_gp) # 98 observations

qplot(corrs_non_gp, geom = "histogram", main = "Распределение корреляций для не ГП")

# t-testing agains mu > 0
t.test(corrs_non_gp, alternative = "less", mu = 0)
# OUTPUT
#One Sample t-test

#data:  corrs_non_gp
#t = -2.37, df = 97, p-value = 0.009885
#alternative hypothesis: true mean is less than 0
#95 percent confidence interval:
#  -Inf -0.02718196
#sample estimates:
#  mean of x 
#-0.09082988 


#### SN ####

# creating similarity dataframe
sn_data_hist <- summarize(group_by(sn_data, gtp, oes), 
                          total_disqual = sum(is_disqualified),
                          history = length(gtp),
                          size = mean(total_fact))

# deleting those, who has changed oes

sn_data_hist <- sn_data_hist %>% 
  group_by(gtp) %>% mutate(count = length(gtp)) %>% filter(count == 1)
sn_data_hist <- sn_data_hist[, c("gtp", "oes", "total_disqual", "history", "size")]

# separating healthy from sick ones
sn_data_healthy <- as.data.frame(filter(sn_data_hist, total_disqual == 0))
sn_data_sick <- as.data.frame(filter(sn_data_hist, total_disqual > 0))

# creating empty dataframe for the accumulation of NNs
sn_acc_nearests <- data.frame(sn_data_sick = character(), healthy_gtp = character())


for (g in unique(sn_data_sick$gtp)) {
  res <- find_nn(sn_data_sick, sn_data_healthy, g)
  sn_acc_nearests <- rbind(sn_acc_nearests, res)
}

# healthy to join to sick
sn_healthy_to_join <- sn_data_healthy[sn_data_healthy$gtp %in% sn_acc_nearests$healthy_gtp, ]
sn_data_sick <- merge(sn_data_sick, sn_acc_nearests, 
                      by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

sn_data_s <- sn_data[sn_data$gtp %in% sn_data_sick$gtp, ]
sn_data_s <- merge(sn_data_s, sn_acc_nearests, 
                   by.x = c("gtp"), by.y = c("sick_gtp"), all.x = TRUE)

sn_data_h <- sn_data[sn_data$gtp %in% sn_data_healthy$gtp, ]

sn_data_res <- merge(sn_data_s, sn_data_h[, c("gtp", "month", "op1_indicator", "op2_indicator")], 
                     by.x = c("healthy_gtp", "month"), by.y = c("gtp", "month"), all.x = TRUE)

sn_data_res$indicator.y <- sn_data_res$op1_indicator.y + sn_data_res$op2_indicator.y
sn_data_res$indicator.x <- sn_data_res$op1_indicator.x + sn_data_res$op2_indicator.x
sn_data_res <- rename(sn_data_res, healthy_ind = indicator.y)
sn_data_res <- rename(sn_data_res, sick_ind = indicator.x)

sn_data_res <- na.omit(sn_data_res)

sn_data_res$target <- sn_data_res$sick_ind - sn_data_res$healthy_ind

sn_data_res$target <- sapply(sn_data_res$target, if_negative)

sn_data_res_nonzero <- sn_data_res[sn_data_res$gtp %in% unique(sn_data_res[sn_data_res$target != 0, ]$gtp), ]

corrs_sn <- vector()
for (g in unique(sn_data_res_nonzero$gtp)) {
  frame <- sn_data_res_nonzero[sn_data_res_nonzero$gtp == g, ]
  corrs_sn <- append(corrs_sn, cor(frame$disqual_count, frame$target))
}
corrs_sn <- na.omit(corrs_sn) # 47 observations

qplot(corrs_sn, geom = "histogram", main = "Распределение корреляций для СН")

# t-testing agains > 0
t.test(corrs_sn, alternative = "less", mu = 0)
       
# OUTPUT
#One Sample t-test

#data:  corrs_sn
#t = 1.3833, df = 46, p-value = 0.9134
#alternative hypothesis: true mean is less than 0
#95 percent confidence interval:
#  -Inf 0.1357592
#sample estimates:
#  mean of x 
#0.06133089 
