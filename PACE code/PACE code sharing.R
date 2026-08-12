# =============================================================================
# PACE Trial: Effect of a Vending Machine Intervention on Hot Beverage
#             Calorie Purchases — Stepped-Wedge Cluster Randomised Trial
#
# Outcomes:
#   Primary   - average calories per transaction (aver_calchange)
#   Secondary - number of hot beverage transactions (total_transchange)
#               number high-calorie hot beverage transactions (high_transchange) 
#               proportion of high-calorie hot beverage transactions
#
# Design: 8 sequences, 25 clusters (39 vending machine locations), rolled out
#         in a stepped-wedge design over 15 weekly periods. Building is
#         nested within cluster (cl); several models additionally include a
#         (1 | Building) random effect alongside (1 | cl).
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(lme4)
library(lmerTest)   # applies Satterthwaite df to lmer() output
library(splines)
library(merDeriv)
library(sandwich)
library(writexl)
library(emmeans)    # required for emtrends() in the subgroup analyses

# -----------------------------------------------------------------------------
# 1. Data import
# -----------------------------------------------------------------------------
# NOTE: replace with a relative / project-root path before sharing the
# repository, e.g. file.path("data", "PACE_data.xlsx")
data_path <- "G:/PACE DATA 21.10.2024.xlsx"
data <- read_excel(data_path)

colnames(data)[1] <- "Building"

# -----------------------------------------------------------------------------
# 2. Cluster, sequence, and intervention-timing assignment
# -----------------------------------------------------------------------------
# Building name -> cluster ID.
# Several buildings appear under more than one raw label in the source data
# (trailing whitespace / naming variants); both variants are mapped to the
# same cluster below.
building_to_cluster <- c(
  "NCSEM "                     = "1",
  "James France "               = "2",
  "Sir David Davies"            = "3",
  "Design School"                = "4",
  "Beckwith"                     = "5",
  "Towers Dining Hall "          = "6",
  "Holywell IT"                  = "7",
  "Hollywell "                   = "7",
  "Stewart Miller"               = "8",
  "Royce"                        = "9",
  "EHB Shop "                    = "10",
  "Wolfson School Downstairs"    = "11",
  "Business School "             = "12",
  "ATTIC"                        = "13",
  "Hazlerigg "                   = "14",
  "Matthew Arnold"               = "15",
  "Cayley"                       = "16",
  "Rutland staff devl"           = "17",
  "Rutland staff devl "          = "17",
  "Elvyn Richards "              = "18",
  "Faraday"                      = "19",
  "West Park Teaching Hub "      = "20",
  "Powerbase Gym"                = "21",
  "Wolfson School "              = "22",
  "S Building "                  = "23",
  "David Collett "               = "24",
  "Rutherford"                   = "25"
)

# Cluster-level covariates: randomisation sequence (sq), number of vending
# machines (novm), and mean baseline mocha transactions (basemocha).

cluster_covariates <- tibble::tribble(
  ~cl,  ~sq, ~novm, ~basemocha,
  "1",   1,    2,     17.0,
  "2",   1,    1,     52.0,
  "3",   1,    1,     12.3,
  "4",   2,    1,     21.3,
  "5",   2,    1,     10.3,
  "6",   2,    2,    166.5,
  "7",   3,    2,      2.6,
  "8",   3,    1,     14.0,
  "9",   3,    1,    277.3,
  "10",  4,    1,     40.0,
  "11",  4,    1,     21.7,
  "12",  4,    4,     70.4,
  "13",  5,    1,     14.3,
  "14",  5,    4,     19.2,
  "15",  5,    1,      5.3,
  "16",  5,    1,    110.7,
  "17",  6,    2,     11.3,
  "18",  6,    2,     91.0,
  "19",  6,    1,    164.3,
  "20",  7,    2,     61.5,
  "21",  7,    1,      6.0,
  "22",  7,    1,     29.3,
  "23",  8,    2,     23.8,
  "24",  8,    2,     84.7,
  "25",  8,    1,     91.7
)

data$cl <- sub("\\(.*", "", data$Building)
data$cl <- unname(building_to_cluster[data$cl])
data <- left_join(data, cluster_covariates, by = "cl")

## Period indicator (currently only period 1 is coded from the source data;
## kept as in the original script)
data$period <- NA
data$period[data$Week %in% 1:3] <- 1

## Intervention indicator: a cluster in sequence `sq` switches on in the week
## after (sq + 2), i.e. sq 1 -> Week > 3, sq 2 -> Week > 4, ..., sq 8 -> Week > 10
data$int <- as.integer(data$Week > (data$sq + 2))

# -----------------------------------------------------------------------------
# 3. Outcome cleaning
# -----------------------------------------------------------------------------
## Beverage columns are read in as character; convert to numeric.
## Columns 5:20 correspond to the "Total" and "Change" beverage count columns.
data[, 5:20] <- lapply(data[, 5:20], as.numeric)

## Hot water had no purchase option at some machines -> treat missing as zero
data$`Hot Water Total`[is.na(data$`Hot Water Total`)] <- 0
data$`Hot Water Change`[is.na(data$`Hot Water Change`)] <- 0

## Drop rows lost to machine malfunction / inaccessible data
data <- data[!is.na(data$`Espresso Total`), ]  # 572 observations

## Drop rows with no valid period-to-period change (first observation for a
## machine has no prior period to compare against)

data1 <- data[!is.na(data$`Americano Change`), ]  # 532 observations

# -----------------------------------------------------------------------------
# 4. Derived outcome variables
# -----------------------------------------------------------------------------
## 1. Average calories per transaction (primary outcome)
data1$total_transchange <- data1$`Espresso Change` + data1$`Americano Change` +
  data1$`Cappuccino Change` + data1$`Latte Change` + data1$`Flat White Change` +
  data1$`Mocha Change` + data1$`Hot Chocolate Change` + data1$`Hot Water Change`

data1$total_calchange <- 2 * data1$`Espresso Change` + 2 * data1$`Americano Change` +
  97 * data1$`Cappuccino Change` + 144 * data1$`Latte Change` +
  78 * data1$`Flat White Change` + 163 * data1$`Mocha Change` +
  128 * data1$`Hot Chocolate Change`

data1$aver_calchange <- data1$total_calchange / data1$total_transchange

# 2, the total number of high calorie hot beverage transactions ## secondary outcome
data1$total_transchange

# 3, the total number of high-calories hot beverage transactions  ## secondary outcome
data1$high_transchange <- data1$`Cappuccino Change` + data1$`Latte Change`+ data1$`Flat White Change` + data1$`Mocha Change` + data1$`Hot Chocolate Change` 

# 4, the  proportion of high calorie hot beverage transactions ## secondary outcome
data1$prop_highchange <- data1$high_transchange/data1$total_transchange

colnames(data1)[2] <- "Building_type"
data1$Building_type1 <- as.numeric(as.factor(data1$Building_type))

# -----------------------------------------------------------------------------
# 5. Descriptive statistics
# -----------------------------------------------------------------------------
## Average calories per transaction, by sequence (baseline period only) and by week
aggregate(aver_calchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(case_count = sum(x >= 0), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

aggregate(aver_calchange ~ Week, data = data1, FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

## Total beverage transactions, by sequence (baseline period only) and by week
aggregate(total_transchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

aggregate(log_total_transchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

aggregate(total_transchange ~ Week, data = data1, FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

## High-calorie transaction counts, by sequence (baseline period only) and by week
aggregate(high_transchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

aggregate(log_high_transchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

aggregate(high_transchange ~ Week, data = data1, FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2)
})

## Proportion of high-calorie transactions, by sequence (baseline period only)
aggregate(high_transchange ~ sq, data = data1[data1$period == 1, ], FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 0)
})

data1[data1$period == 1, ] %>%
  group_by(sq) %>%
  summarise(
    sum_total_transchange = sum(total_transchange, na.rm = TRUE),
    sum_highchange = sum(high_transchange, na.rm = TRUE),
    prop_highchange = sum_highchange / sum_total_transchange
  ) %>%
  print()

aggregate(prop_highchange ~ Week, data = data1, FUN = function(x) {
  round(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 2) * 100
})

# -----------------------------------------------------------------------------
# 6. Descriptive plots
# -----------------------------------------------------------------------------
date_labels <- c(
  "20.10.23", "27.10.23", "03.11.23", "17.11.23", "01.12.23", "15.12.23",
  "09.02.24", "23.02.24", "08.03.24", "22.03.24", "26.04.24", "10.05.24",
  "24.05.24", "31.05.24"
)

intervention_data <- data.frame(
  sq = 1:8,
  intervention_week = 4:11
)

## helper: mark the week each sequence crosses over, for annotating plots
mark_intervention_week <- function(mean_by_week_sq) {
  left_join(intervention_data, mean_by_week_sq, by = c("sq", "intervention_week" = "Week"))
}

## 6a. Average calories per transaction over time, by sequence
aver_cal_mean1 <- aggregate(aver_calchange ~ Week + sq, data = data1, FUN = mean, na.rm = TRUE)
aver_cal_mean1 <- left_join(aver_cal_mean1, intervention_data, by = "sq")
intervention_data1 <- mark_intervention_week(aver_cal_mean1)

average_plot <- ggplot(aver_cal_mean1, aes(x = Week, y = aver_calchange, colour = factor(sq))) +
  geom_point(size = 2) +
  geom_point(data = intervention_data1, aes(x = intervention_week, y = aver_calchange, colour = factor(sq)),
             size = 4, shape = 7) +
  geom_line() +
  scale_x_continuous(breaks = unique(aver_cal_mean1$Week), labels = date_labels) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week", y = "Average Calories per Transaction", colour = "Sequence") +
  scale_color_brewer(palette = "Set1")

## 6b. Total high-calorie transactions over time, by sequence (change to total_transchange for total transactions )
high_trans_mean1 <- aggregate(high_transchange ~ Week + sq, data = data1, FUN = mean, na.rm = TRUE)
high_trans_mean1 <- left_join(high_trans_mean1, intervention_data, by = "sq")
intervention_data2 <- mark_intervention_week(high_trans_mean1)

high_trans_plot <- ggplot(high_trans_mean1, aes(x = Week, y = high_transchange, colour = factor(sq))) +
  geom_point(size = 2) +
  geom_line() +
  geom_point(data = intervention_data2, aes(x = intervention_week, y = high_transchange, colour = factor(sq)),
             size = 4, shape = 7) +
  scale_x_continuous(breaks = unique(high_trans_mean1$Week), labels = date_labels) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week", y = "Total Number of High Calorie Hot Beverage Transactions", colour = "Sequence") +
  scale_color_brewer(palette = "Set1")

## 6c. Proportion of high-calorie transactions over time, by sequence
prop_high_mean1 <- aggregate(prop_highchange ~ Week + sq, data = data1, FUN = mean, na.rm = TRUE)
prop_high_mean1 <- left_join(prop_high_mean1, intervention_data, by = "sq")
intervention_data3 <- mark_intervention_week(prop_high_mean1)

pop_plot <- ggplot(prop_high_mean1, aes(x = Week, y = prop_highchange, colour = factor(sq))) +
  geom_point(size = 2) +
  geom_point(data = intervention_data3, aes(x = intervention_week, y = prop_highchange, colour = factor(sq)),
             size = 4, shape = 7) +
  geom_line() +
  scale_x_continuous(breaks = unique(prop_high_mean1$Week), labels = date_labels) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week", y = "Proportion of High Calories Hot Beverage Transactions", colour = "Sequence") +
  scale_color_brewer(palette = "Set1")

average_plot / pop_plot + plot_layout(guides = "collect")

## 6d. Before vs. after intervention, faceted by sequence
aver_cal_mean_int1 <- aggregate(aver_calchange ~ int, data = data1, FUN = mean, na.rm = TRUE)
prop_high_mean_int1 <- aggregate(prop_highchange ~ int, data = data1, FUN = mean, na.rm = TRUE)

box_average <- ggplot(data1, aes(x = factor(int, labels = c("Before", "After")),
                                  y = aver_calchange, colour = factor(int))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, colour = "black") +
  facet_wrap(~sq, ncol = 4, labeller = labeller(sq = function(x) paste0("Sequence ", x))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey", colour = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  labs(x = "Intervention", y = "Average Calories per Transaction") +
  scale_color_manual(values = c("skyblue", "orange"), labels = c("Before", "After"))

box_pop <- ggplot(data1, aes(x = factor(int, labels = c("Before", "After")),
                              y = prop_highchange, colour = factor(int))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, colour = "black") +
  facet_wrap(~sq, ncol = 4, labeller = labeller(sq = function(x) paste0("Sequence ", x))) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey", colour = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  ) +
  labs(x = "Intervention", y = "Proportion of High Calories Hot Beverage Transactions") +
  scale_color_manual(values = c("skyblue", "orange"), labels = c("Before", "After"))

# the total number of beverage transactions (same for high_transchange)
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

# -----------------------------------------------------------------------------
# 7. Primary and secondary models
# -----------------------------------------------------------------------------
## Sanity check: number of distinct buildings feeding into each cluster
data1 %>%
  group_by(cl) %>%
  summarise(n_Building = n_distinct(Building)) %>%
  print(n = Inf)

data1$int <- as.factor(data1$int)
## Primary analysis: average calories per transaction, adjusting for building
## type, number of vending machines, and baseline mocha transactions, with
## crossed random effects for cluster and building
model_lme1_m <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int +
    (1 | cl) + (1 | Building),
  data = data1
)
summary(model_lme1_m)
confint(model_lme1_m, method = "Wald")

## Secondary analysis 1: total number of high-calorie hot beverage transactions
#add number of days to each period(week)
data1$nod <- NA
data1$nod [data1$Week==2] <- 7
data1$nod [data1$Week==3] <- 7
data1$nod [data1$Week==4] <- 7
data1$nod [data1$Week==5] <- 14
data1$nod [data1$Week==6] <- 14
data1$nod [data1$Week==7] <- 14
data1$nod [data1$Week==8] <- 54
data1$nod [data1$Week==9] <- 14
data1$nod [data1$Week==10] <- 14
data1$nod [data1$Week==11] <- 14
data1$nod [data1$Week==12] <- 35
data1$nod [data1$Week==13] <- 14
data1$nod [data1$Week==14] <- 14
data1$nod [data1$Week==15] <- 7

high_nb_m <- glmer.nb(
  high_transchange ~ factor(Week) + factor(Building_type1) + novm + log(basemocha) + int +
    offset(log(nod)) + (1 | cl) + (1 | Building),
  data = data1
)

## Secondary analysis 2: total number of beverage transactions
total_nb_m <- glmer.nb(
  total_transchange ~ factor(Week) + factor(Building_type1) + novm + log(basemocha) + int +
    offset(log(nod)) + (1 | cl) + (1 | Building),
  data = data1
)

## Secondary analysis 3: proportion of high-calorie transactions (Poisson,
## offset by total transactions)
data1$total_transchange <- data1$total_transchange + 1e-6  # avoid log(0) offset
data1$Building_type1 <- as.numeric(as.factor(data1$Building_type))
data2 <- data1[!is.na(data1$aver_calchange), ]  # 529 observations

model_poisson <- glmer(
  high_transchange ~ factor(Week) + factor(Building_type1) + novm + log(basemocha) + int +
    offset(log(total_transchange)) + (1 | cl),
  data = data2,
  family = poisson(link = "log")
)

## Cluster-robust (sandwich) standard errors for the Poisson model
sandwich_cov <- sandwich(model_poisson, bread. = bread.glmerMod, meat. = meat(model_poisson, level = 2))
sandwich_se  <- sqrt(diag(sandwich_cov))

beta_hat <- fixef(model_poisson)[20]  # coefficient of interest (int) - confirm index against current formula
ci_lower <- beta_hat - 1.96 * sandwich_se[20]
ci_upper <- beta_hat + 1.96 * sandwich_se[20]

z_value  <- beta_hat / sandwich_se[20]
p_value  <- 2 * (1 - pnorm(abs(z_value)))  # two-tailed Wald test

# -----------------------------------------------------------------------------
# 8. Sensitivity analyses
# -----------------------------------------------------------------------------
## SA1 (primary outcome only): time modelled as a restricted cubic spline
model_lme1_spline <- lmer(
  aver_calchange ~ ns(Week, df = 3) + factor(Building_type) + novm + basemocha + int +
    (1 | cl) + (1 | Building),
  data = data1
)
summary(model_lme1_spline)
confint(model_lme1_spline, method = "Wald")

## SA2: cluster-by-intervention random effect

SA2_out1 <- lmer(aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int +
                    (1 | cl) + (1 | cl:int), data = data1)
summary(SA2_out1)
confint(SA2_out1, method = "Wald")

## SA3: interaction between treatment and number of periods since first treated
total_clusters <- 25

for (i in 1:12) {
  data1[[paste0("not_", i)]] <- 0
  for (j in 1:8) {
    week_value <- i + (j + 2)
    if (week_value <= 15) {
      data1[[paste0("not_", i)]][data1$Week == week_value & data1$sq == j] <- 1
    }
  }
}

not_terms <- paste0("int:not_", 1:12, collapse = " + ")
SA3_out1 <- lmer(
  as.formula(paste(
    "aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha +",
    not_terms, "+ (1 | cl) + (1 | Building)"
  )),
  data = data1
)

## Extract the 12 "periods since first treated" estimates.
## confint() on an lmer fit returns one extra row per random-effect grouping
## plus one for the residual SD, above the fixed effects. This model has two
## random effects (cl, Building), i.e. 3 extra rows, hence the +3 row offset
## between the `summary()` coefficient table (rows 20:31) and the
## `confint()` table (rows 23:34) below.
estimates1      <- summary(SA3_out1)$coefficients[20:31, c(1, 2, 5)]  # Estimate, Std. Error, p-value
conf_intervals1 <- confint(SA3_out1, method = "Wald")

results_df1 <- data.frame(
  x_label  = factor(1:12),
  Estimate = estimates1,
  CI       = paste0("(", round(conf_intervals1[23:34, 1], 2), ", ", round(conf_intervals1[23:34, 2], 2), ")"),
  Lower_CI = conf_intervals1[23:34, 1],
  Upper_CI = conf_intervals1[23:34, 2]
)

write_xlsx(results_df1, "ss3.xlsx")

results_plot_ss3 <- ggplot(results_df1, aes(x = x_label, y = Estimate.Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  labs(x = "Number of Periods Since First Treated",
       y = "Mean Difference in Average Calories per Transaction") +
  theme_bw()
results_plot_ss3

## SA4 (primary outcome only): adjust for method of vending machine payment
## (1 = payment, 2 = free dispense, 3 = payment and prepaid token)
payment_method_file <- read_excel("how vending machines are operated file.xlsx")
vending_payment_lookup <- tibble::tribble(
  ~cl_name,                              ~cl,
  "NCSEM",                               "1",
  "James France",                        "2",
  "Sir David Davies - upstairs",         "3",
  "Design School",                       "4",
  "Beckwith",                            "5",
  "Towers Dining Hall - Left",           "6",
  "Holywell",                            "7",
  "Stewart Miller",                      "8",
  "Royce",                               "9",
  "EHB Shop",                            "10",
  "Wolfson School - downstairs retail",  "11",
  "Business School",                     "12",
  "ATTIC",                               "13",
  "Hazlerigg",                           "14",
  "Matthew Arnold",                      "15",
  "Cayley",                              "16",
  "Rutland staff devl",                  "17",
  "Elvyn Richards",                      "18",
  "Faraday",                             "19",
  "West Park Teaching Hub",              "20",
  "Powerbase Gym",                       "21",
  "Wolfson School upstairs",             "22",
  "S Building",                          "23",
  "David Collett",                       "24",
  "Rutherford",                          "25"
)

payment_method_file <- payment_method_file %>%
  left_join(vending_payment_lookup, by = c("...2" = "cl_name"))

data1 <- data1 %>%
  left_join(payment_method_file %>% select(cl, `code for method of payment`), by = "cl")

model_lme1_payment <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + factor(`code for method of payment`) +
    novm + basemocha + int + (1 | cl) + (1 | Building),
  data = data1
)
summary(model_lme1_payment)
confint(model_lme1_payment, method = "Wald")

## SA5 (primary outcome only): excluding each cluster's transition period
data1_no_transition <- data1 %>%
  group_by(cl) %>%
  mutate(
    first_int_week = ifelse(any(int == 1), min(Week[int == 1]), NA)
  ) %>%
  filter(Week != first_int_week) %>%
  ungroup() %>%
  select(-first_int_week)

transition_periods <- data1 %>%
  filter(int == 1) %>%
  group_by(cl) %>%
  summarise(transition_week = min(Week), .groups = "drop")
print(transition_periods, n = Inf)

model_lme1_no_transition <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int +
    (1 | cl) + (1 | Building),
  data = data1_no_transition
)
summary(model_lme1_no_transition)
confint(model_lme1_no_transition, method = "Wald")

# -----------------------------------------------------------------------------
# 9. Subgroup analyses
# -----------------------------------------------------------------------------
# Proposed subgroups: baseline mocha transactions, building type, and number of
# vending machines. Each model adds a single
# treatment-by-subgroup interaction to the primary model and is compared to
# it via a likelihood-ratio test.

## 9a. Baseline mocha transactions (continuous)
## Centred so the main effect of `int` stays interpretable at mean baseline mocha
data1$basemocha_c <- scale(data1$basemocha, scale = FALSE)

model_mocha <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha_c * int +
    (1 | cl) + (1 | Building),
  data = data1
)

mean_mocha <- mean(data1$basemocha, na.rm = TRUE)
mocha_quantiles <- quantile(data1$basemocha, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
p10_c <- mocha_quantiles["10%"] - mean_mocha
p50_c <- mocha_quantiles["50%"] - mean_mocha
p90_c <- mocha_quantiles["90%"] - mean_mocha

emt_mocha <- emtrends(model_mocha, ~basemocha_c, var = "int",
                      at = list(basemocha_c = c(p10_c, p50_c, p90_c)))

anova(model_lme1_m, model_mocha)   # LRT for the interaction
summary(emt_mocha, infer = c(TRUE, TRUE))

## 9b. Building type (categorical)
model_building <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) * int + novm + basemocha +
    (1 | cl) + (1 | Building),
  data = data1
)
emt_building <- emtrends(model_building, ~Building_type, var = "int")
summary(emt_building, infer = c(TRUE, TRUE))
anova(model_lme1_m, model_building)

## 9c. Number of vending machines
model_nvm <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + factor(novm) * int + basemocha +
    (1 | cl) + (1 | Building),
  data = data1
)
anova(model_lme1_m, model_nvm)

emt_novm <- emtrends(model_nvm, ~novm, var = "int")
summary(emt_novm, infer = c(TRUE, TRUE))

# -----------------------------------------------------------------------------
# 10. Supplementary analysis
# -----------------------------------------------------------------------------
## Add a cluster-by-week random effect (clustering by period)
smodel_lme1 <- lmer(
  aver_calchange ~ factor(Week) + factor(Building_type) + novm + basemocha + int +
    (1 | cl) + (1 | cl:Week),
  data = data1
)

# -----------------------------------------------------------------------------
# 11. Secular trend summary table (Table export)
# -----------------------------------------------------------------------------
## Model 1: linear mixed model (average calories per transaction)
coef1 <- summary(model_lme1_m)$coefficients[2:14, 1]
ci1   <- confint(model_lme1_m, method = "Wald")[5:17, ]
secular_out1 <- paste0(round(coef1, 2), " (", round(ci1[, 1], 2), ", ", round(ci1[, 2], 2), ")")

## Model 2: negative binomial model (high-calorie transactions), exponentiated
coef2 <- summary(high_nb_m)$coefficients[2:14, 1]
se2   <- summary(high_nb_m)$coefficients[2:14, 2]
ci2   <- cbind(coef2 - 1.96 * se2, coef2 + 1.96 * se2)
secular_out2 <- paste0(round(exp(coef2), 2), " (", round(exp(ci2[, 1]), 2), ", ", round(exp(ci2[, 2]), 2), ")")

## Model 3: negative binomial model (total transactions), exponentiated
coef3 <- summary(total_nb_m)$coefficients[2:14, 1]
se3   <- summary(total_nb_m)$coefficients[2:14, 2]
ci3   <- cbind(coef3 - 1.96 * se3, coef3 + 1.96 * se3)
secular_out3 <- paste0(round(exp(coef3), 2), " (", round(exp(ci3[, 1]), 2), ", ", round(exp(ci3[, 2]), 2), ")")

secular_trend <- data.frame(
  Model_1 = secular_out1,
  Model_2 = secular_out2,
  Model_3 = secular_out3
)
secular_trend

write_xlsx(secular_trend, "secular_trend.xlsx")
