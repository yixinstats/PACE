### PACE data ###

### data management ###
library(readxl)
data <- read_excel("G:/My Drive/Applied Health/PACE data/PACE DATA 21.10.2024.xlsx")

colnames(data)[1] <- 'Building'

## add cluster 'cl'
library(dplyr)
library(stringr)
data$cl <- sub("\\(.*", "", data$Building)

##allocate number code of clusters to the building
data$cl[data$cl == "NCSEM "] <- '1'
data$cl[data$cl == "James France "] <- '2'
data$cl[data$cl == "Sir David Davies"] <- '3'
data$cl[data$cl == "Design School"] <- '4'
data$cl[data$cl == "Beckwith"] <- '5'
data$cl[data$cl == "Towers Dining Hall "] <- '6'
data$cl[data$cl == "Holywell IT"] <- '7'
data$cl[data$cl == "Hollywell "] <- '7'
data$cl[data$cl == "Stewart Miller"] <- '8'
data$cl[data$cl == "Royce"] <- '9'
data$cl[data$cl == "EHB Shop "] <- '10'
data$cl[data$cl == "Wolfson School Downstairs"] <- '11'
data$cl[data$cl == "Business School "] <- '12'
data$cl[data$cl == "ATTIC"] <- '13'
data$cl[data$cl == "Hazlerigg "] <- '14'
data$cl[data$cl == "Matthew Arnold"] <- '15'
data$cl[data$cl == "Cayley"] <- '16'
data$cl[data$cl == "Rutland staff devl"] <- '17'
data$cl[data$cl == "Rutland staff devl "] <- '17'
data$cl[data$cl == "Elvyn Richards "] <- '18'
data$cl[data$cl == "Faraday"] <- '19'
data$cl[data$cl == "West Park Teaching Hub "] <- '20'
data$cl[data$cl == "Powerbase Gym"] <- '21'
data$cl[data$cl == "Wolfson School "] <- '22'
data$cl[data$cl == "S Building "] <- '23'
data$cl[data$cl == "David Collett "] <- '24'
data$cl[data$cl == "Rutherford" ] <- '25'

## add sequence according to cl
data$sq <- NA
data$sq [data$cl==1] <- 1
data$sq [data$cl==2] <- 1
data$sq [data$cl==3] <- 1
data$sq [data$cl==4] <- 2
data$sq [data$cl==5] <- 2
data$sq [data$cl==6] <- 2
data$sq [data$cl==7] <- 3
data$sq [data$cl==8] <- 3
data$sq [data$cl==9] <- 3
data$sq [data$cl==10] <- 4
data$sq [data$cl==11] <- 4
data$sq [data$cl==12] <- 4
data$sq [data$cl==13] <- 5
data$sq [data$cl==14] <- 5
data$sq [data$cl==15] <- 5
data$sq [data$cl==16] <- 5
data$sq [data$cl==17] <- 6
data$sq [data$cl==18] <- 6
data$sq [data$cl==19] <- 6
data$sq [data$cl==20] <- 7
data$sq [data$cl==21] <- 7
data$sq [data$cl==22] <- 7
data$sq [data$cl==23] <- 8
data$sq [data$cl==24] <- 8
data$sq [data$cl==25] <- 8

## indicate period for later use
data$period <- NA
data$period[data$Week==1] <- 1
data$period[data$Week==2] <- 1
data$period[data$Week==3] <- 1

## indicate intervention
data$int <- 0
#data$int [data$period==1] <- 0
data$int [data$Week >3 & data$sq ==1] <- 1
data$int [data$Week >4 & data$sq ==2] <- 1
data$int [data$Week >5 & data$sq ==3] <- 1
data$int [data$Week >6 & data$sq ==4] <- 1
data$int [data$Week >7 & data$sq ==5] <- 1
data$int [data$Week >8 & data$sq ==6] <- 1
data$int [data$Week >9 & data$sq ==7] <- 1
data$int [data$Week >10 & data$sq ==8] <- 1


## change outcomes from string to numeric   
data[, 5:20] <- lapply(data[, 5:20], as.numeric)
## breakdown version: 
#data$`Espresso Total` <- as.numeric(data$`Espresso Total`)
#data$`Americano Total` <- as.numeric(data$`Americano Total`)
#data$`Cappuccino Total` <- as.numeric(data$`Cappuccino Total`)
#data$`Latte Total` <- as.numeric(data$`Latte Total`)
#data$`Flat White Total` <- as.numeric(data$`Flat White Total`)
#data$`Mocha Total` <- as.numeric(data$`Mocha Total`)
#data$`Hot Chocolate Total` <- as.numeric(data$`Hot Chocolate Total`)
#data$`Hot Water Total` <- as.numeric(data$`Hot Water Total`)

#data$`Espresso Change` <- as.numeric(data$`Espresso Change`)
#data$`Americano Change` <- as.numeric(data$`Americano Change`)
#data$`Cappuccino Change` <- as.numeric(data$`Cappuccino Change`)
#data$`Latte Change` <- as.numeric(data$`Latte Change`)
#data$`Flat White Change` <- as.numeric(data$`Flat White Change`)
#data$`Mocha Change` <- as.numeric(data$`Mocha Change`)
#data$`Hot Chocolate Change` <- as.numeric(data$`Hot Chocolate Change`)
#data$`Hot Water Change` <- as.numeric(data$`Hot Water Change`)

## dealing with missing observations for water due to the machines did not have hot water beverage available for purchase. 
data$`Hot Water Total`[is.na(data$`Hot Water Total`)] <- 0
data$`Hot Water Change`[is.na(data$`Hot Water Change`)] <- 0

## get rid of missing rows due to machine malfunctions and inaccessible data
data <- data[which(!is.na(data$`Espresso Total`)),] #572 observations 

## in terms of transaction change from previous period to current period, there is no value for 
data1 <- data1[!is.na(data1$`Americano Change`),] ## 532


### Description Analysis ###

# 1, average number of calories per transition ## primary outcome 

data1$total_transchange <- data1$`Espresso Change`+ data1$`Americano Change`+ data1$`Cappuccino Change` + data1$`Latte Change`+ data1$`Flat White Change` + data1$`Mocha Change` + data1$`Hot Chocolate Change` + data1$`Hot Water Change`
data1$total_calchange <- 2*data1$`Espresso Change`+ 2*data1$`Americano Change`+ 97*data1$`Cappuccino Change` + 144*data1$`Latte Change`+ 78*data1$`Flat White Change` + 163*data1$`Mocha Change` + 128*data1$`Hot Chocolate Change`                      
data1$aver_calchange <- data1$total_calchange/data1$total_transchange

# 2, the total number of hot beverage transactions ## secondary outcome
data1$total_transchange

#average number of calories per transition ## primary outcome 
aggregate(aver_calchange ~ sq, data = data1[data1$period==1,], FUN = function(x) {
  round(c(
    case_count = sum(x >= 0),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  ),2)
})

aggregate(aver_calchange ~ Week, data = data1, FUN = function(x) {
  round(c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  ),2)
})

#the total number of hot beverage transactions  #secondary outcome
aggregate(total_transchange ~ sq, data = data1[data1$period==1,], FUN = function(x) {
  round(c(
    #case_count = sum(x >= 0),
    mean = mean(x, na.rm = TRUE),
    #var = var(x, na.rm = TRUE),   
    sd = sd(x, na.rm = TRUE)
  ),2)
})
# log
data1$log_total_transchange <- log(data1$total_transchange)
aggregate(log_total_transchange ~ sq, data = data1[data1$period==1,], FUN = function(x) {
  round(c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  ),2)
})


aggregate(total_transchange ~ Week, data = data1, FUN = function(x) {
  round(c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  ),2)
})

### plots
library(dplyr)
# average number of calories per transition ## primary outcome 
aver_cal_mean1 <- aggregate(
  aver_calchange ~ Week + sq, 
  data = data1, 
  FUN = function(x) mean(x, na.rm = TRUE)
)
library(ggplot2)

intervention_data <- data.frame(
  sq = c(1, 2, 3, 4, 5, 6, 7, 8),  # Sequence numbers
  intervention_week = c(4, 5, 6,7,8,9,10,11)  # Week of intervention for each sequence
)

aver_cal_mean1 <- left_join(aver_cal_mean1, intervention_data, by='sq')
intervention_data1 <- left_join(intervention_data, aver_cal_mean1, by = c("sq", "intervention_week"))
intervention_data1 <- intervention_data1[intervention_data1$intervention_week == intervention_data1$Week,]

library(ggplot2)

# adding dates to period 2 to 15
date_labels <- c("20.10.23", "27.10.23", "03.11.23", "17.11.23", 
                 "01.12.23", "15.12.23", "09.02.24", "23.02.24", "08.03.24", 
                 "22.03.24", "26.04.24", "10.05.24", "24.05.24", "31.05.24")

average_plot <- ggplot(data = aver_cal_mean1, aes(x = Week, y = aver_calchange, colour = factor(sq))) +
  geom_point(size = 2) +
  geom_point(data = intervention_data1, aes(x = intervention_week, y = aver_calchange, color = factor(sq)), 
             size =4, shape = 7) +  
  geom_line() +
  scale_x_continuous(breaks = unique(aver_cal_mean1$Week), labels = date_labels) + 
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    x = "Week", 
    y = "Average Calories per Transaction",
    colour = "Sequence"
  ) +
  scale_color_brewer(palette = "Set1") 


# the total number of hot beverage transactions
high_trans_mean1 <- aggregate(
  total_transchange ~ Week + sq, 
  data = data1, 
  FUN = function(x) mean(x, na.rm = TRUE)
)

high_trans_mean1 <- left_join(high_trans_mean1, intervention_data, by='sq')
intervention_data2 <- left_join(intervention_data, high_trans_mean1, by = c("sq", "intervention_week"))
intervention_data2 <- intervention_data2[intervention_data2$intervention_week == intervention_data2$Week,]

ggplot(data = total_trans_mean1, aes(x = Week, y = total_transchange, colour = factor(sq))) +
  geom_point(size = 2) +
  geom_line() +
  geom_point(data = intervention_data2, aes(x = intervention_week, y = total_transchange, color = factor(sq)), 
             size =4, shape = 7) +  
  scale_x_continuous(breaks = unique(total_trans_mean1$Week), labels = date_labels) + 
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    x = "Week", 
    y = "Total Number of Hot Beverage Transactions",
    colour = "Sequence"
  ) +
  scale_color_brewer(palette = "Set1") # Optional: Improve color palette

## before and after intervention 
#  average number of calories per transition ## primary outcome 
aver_cal_mean_int1 <- aggregate(
  aver_calchange ~ int, 
  data = data1, 
  FUN = function(x) mean(x, na.rm = TRUE)
)

library(ggplot2)
box_average <- ggplot(data = data1, aes(x = factor(int, labels = c("Before", "After")), y = aver_calchange, color = factor(int))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") +
  facet_wrap(~ sq, ncol = 4, labeller = labeller(sq = function(x) paste0("Sequence ", x))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    x = "Intervention",
    y = "Average Calories per Transaction",
    color = "Intervention"
  ) +
  scale_color_manual(
    values = c("skyblue", "orange"),
    labels = c("Before", "After") 
  )

# the total number of hot beverage transactions ## secondary outcome
total_trans_mean1 <- aggregate(
  total_transchange ~ Week + sq, 
  data = data1, 
  FUN = function(x) mean(x, na.rm = TRUE)
)
ggplot(data = data1, aes(x = factor(int, labels = c("Before", "After")), y = log_total_transchange, color = factor(int))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") +
  facet_wrap(~ sq, ncol = 4, labeller = labeller(sq = function(x) paste0("Sequence ", x))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    x = "Intervention",
    y = "Total Number of Beverage Transactions (log)",
    color = "Intervention"
  ) +
  scale_color_manual(
    values = c("skyblue", "orange"),
    labels = c("Before", "After") 
  )

### STATISTICAL ANALYSIS ###

library(lme4)
library(lmerTest) 

## primary analysis
#adjust 'buiding type', 'number of vending machines' and 'average baseline mocha transactions per cluster'

# add 'number of vending machines'
data1$novm <- NA
data1$novm [data1$cl==1] <- 2
data1$novm [data1$cl==2] <- 1
data1$novm [data1$cl==3] <- 1
data1$novm [data1$cl==4] <- 1
data1$novm [data1$cl==5] <- 1
data1$novm [data1$cl==6] <- 2
data1$novm [data1$cl==7] <- 2
data1$novm [data1$cl==8] <- 1
data1$novm [data1$cl==9] <- 1
data1$novm [data1$cl==10] <- 1
data1$novm [data1$cl==11] <- 1
data1$novm [data1$cl==12] <- 4
data1$novm [data1$cl==13] <- 1
data1$novm [data1$cl==14] <- 4
data1$novm [data1$cl==15] <- 1
data1$novm [data1$cl==16] <- 1
data1$novm [data1$cl==17] <- 2
data1$novm [data1$cl==18] <- 2
data1$novm [data1$cl==19] <- 1
data1$novm [data1$cl==20] <- 2
data1$novm [data1$cl==21] <- 1
data1$novm [data1$cl==22] <- 1
data1$novm [data1$cl==23] <- 2
data1$novm [data1$cl==24] <- 2
data1$novm [data1$cl==25] <- 1

# add average baseline mocha transactions per cluster
data1$basemocha <- NA
data1$basemocha [data1$cl==1] <- 17
data1$basemocha [data1$cl==2] <- 52
data1$basemocha [data1$cl==3] <- 12.3
data1$basemocha [data1$cl==4] <- 21.3
data1$basemocha [data1$cl==5] <- 10.3
data1$basemocha [data1$cl==6] <- 166.5
data1$basemocha [data1$cl==7] <- 2.6
data1$basemocha [data1$cl==8] <- 14
data1$basemocha [data1$cl==9] <- 277.3
data1$basemocha [data1$cl==10] <- 40
data1$basemocha [data1$cl==11] <- 21.7
data1$basemocha [data1$cl==12] <- 70.4
data1$basemocha [data1$cl==13] <- 14.3
data1$basemocha [data1$cl==14] <- 19.2
data1$basemocha [data1$cl==15] <- 5.3
data1$basemocha [data1$cl==16] <- 110.7
data1$basemocha [data1$cl==17] <- 11.3
data1$basemocha [data1$cl==18] <- 91
data1$basemocha [data1$cl==19] <- 164.3
data1$basemocha [data1$cl==20] <- 61.5
data1$basemocha [data1$cl==21] <- 6
data1$basemocha [data1$cl==22] <- 29.3
data1$basemocha [data1$cl==23] <- 23.8
data1$basemocha [data1$cl==24] <- 84.7
data1$basemocha [data1$cl==25] <- 91.7

colnames(data1)[2] <- 'Building_type'
data1$Building_type1 <- as.numeric(as.factor(data1$Building_type))

## average_calchange 
model_lme1 = lmer(aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int + (1 | cl), data=data1) 

library(lmerTest) #The lmerTest package automatically applies Satterthwaite
summary(model_lme1)
confint(model_lme1, method = "Wald") 

## Secondary analysis
## total transactions
model_poisson <- glmer(
  total_transchange ~ factor(Week) + factor(Building_type1) + novm + log(basemocha) + int  + (1 | cl),
  data = data1,
  family = poisson(link = 'log')
)
summary(model_poisson)
confint(model_poisson,method = "Wald")

### sensitive analysis 1 ###
# add cluster-specific time trend
for(k in 1:total_clusters){
  data1$tmp <- data1$Week*I(data1$cl==k)
  colnames(data1)[ncol(data1)] <- paste0("t_cl",k)}

form <- paste0("t_cl", 1:total_clusters, collapse = "+")

#### cluster specific

SS1_out1 = lmer(aver_calchange ~ t_cl1+t_cl2+t_cl3+t_cl4+t_cl5+t_cl6+t_cl7+t_cl8+t_cl9+t_cl10+t_cl11+t_cl12
                +t_cl13+t_cl14+t_cl15+t_cl16+t_cl17+t_cl18+t_cl19+t_cl20+t_cl21+t_cl22+t_cl23+t_cl24+t_cl25 
                + factor(Building_type) + novm + basemocha + int + (1 | cl), data=data1) 

summary(SS1_out1)
confint(SS1_out1, method='Wald')

SS1_out2 <- glmer(
  total_transchange ~ t_cl1+t_cl2+t_cl3+t_cl4+t_cl5+t_cl6+t_cl7+t_cl8+t_cl9+t_cl10+t_cl11+t_cl12
  +t_cl13+t_cl14+t_cl15+t_cl16+t_cl17+t_cl18+t_cl19+t_cl20+t_cl21+t_cl22+t_cl23+t_cl24+t_cl25 
  + factor(Building_type) + novm + log(basemocha) + int  + (1 | cl),
  data = data1,
  family = poisson(link = "log")
)
summary(SS1_out2)
confint(SS1_out2,method = "Wald")

### sensitive analysis  2 ###
SS2_out1 = lmer(aver_calchange ~ factor(Week)+ factor(Building_type) + novm + basemocha + int + (1 | cl) + (1|cl:int), data=data1) 
summary(SS2_out1)
confint(SS2_out1, method='Wald')

SS2_out2 <- glmer(
  total_transchange ~ factor(Week)+ factor(Building_type) + novm + log(basemocha) + int  + (1 | cl) + (1|cl:int),
  data = data1,
  family = poisson(link = "log")
)
summary(SS2_out2)
confint(SS2_out2,method = "Wald")

### sensitive analysis 3 ###
#Include an interaction between treatment and number of periods since first treated
# add indicator for number of periods since first treated
data1$not_1 <- 0

# Initialize all indicators to 0
for (i in 1:12) {
  data1[[paste0("not_", i)]] <- 0
}

# Assign 1 based on the pattern, ensuring Week does not exceed 15
for (i in 1:12) {
  for (j in 1:8) { 
    week_value <- i + (j + 2)  
    if (week_value <= 15) {  
      data1[[paste0("not_", i)]][data1$Week == week_value & data1$sq == j] <- 1
    }
  }
}

SS3_out1 = lmer(aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int:not_1+int:not_2+int:not_3+int:not_4+int:not_5+int:not_6+int:not_7+int:not_8+int:not_9+int:not_10+int:not_11+int:not_12 + (1 | cl), data=data1) 
estimates1 <- summary(SS3_out1)$coefficients[20:31,]  # Extract fixed effect estimates
# Get confidence intervals
conf_intervals1 <- confint(SS3_out1, method = "Wald")
# Combine into a dataframe
results_df1 <- data.frame(
  Estimate = estimates,
  Lower_CI = conf_intervals[22:33, 1],  # Exclude the first row (random effects variance)
  Upper_CI = conf_intervals[22:33, 2],
  x_label = factor(1:12)
)

results_average <- ggplot(results_df1, aes(x = x_label, y = Estimate.Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  labs(x = "Number of period since first treated", y = "Mean difference of average calories per transaction") +
  theme_bw()

SS3_out2 <- glmer(
  total_transchange ~ factor(Week)+ factor(Building_type) + novm + log(basemocha)+ int:not_1+int:not_2+int:not_3+int:not_4+int:not_5+int:not_6+int:not_7+int:not_8+int:not_9+int:not_10+int:not_11+int:not_12 + (1 | cl),
  data = data1,
  family = poisson(link = "log")
)
estimates2 <- summary(SS3_out2)$coefficients[20:31,]  # Extract fixed effect estimates
estimates2[,1] <- exp(estimates2[,1])
estimates2[,2] <- estimates2[,2]*estimates2[,1]
conf_intervals2 <- confint(SS3_out2, method = "Wald")
conf_intervals2 <- exp(conf_intervals2)
results_df2 <- data.frame(
  Estimate = estimates2,
  Lower_CI = conf_intervals2[21:32, 1],  
  Upper_CI = conf_intervals2[21:32, 2],
  x_label = factor(1:12)
)
ggplot(results_df2, aes(x = x_label, y = Estimate.Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  #labs(x = "Number of period since first treated", y = "Proportion of high-calorie beverages dispensed") +
  labs(x = "Number of period since first treated", y = "Ratio of number of hot beverage transactions") +
  theme_bw()
