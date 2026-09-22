# required libraries. install.packages(c("MASS","car","writexl", etc)) if not present
library(MASS)
library(car)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("") #Replace with your working directory path
df <- read.csv("SetA.train.csv")

# cleaning white space in gender column values
df$gender <- trimws(df$gender)

# import and join exposure data. Using the column id as primary key
df_exposure <- read.csv("SetC.exposure.csv")

joined_df <- merge(df, df_exposure, by = "id", all.x = TRUE)

# import vehiclecost data
df_vehcost <- read.csv("SetB.vehiclecost.csv")

#creating join key by combining the below three columns
cols <- c("veh.make", "veh.body", "veh.age")
df_vehcost$join_key <- apply(df_vehcost[, cols], 1, paste, collapse = "_")
print(head(df_vehcost))

joined_df$join_key <- apply(joined_df[, cols], 1, paste, collapse = "_")
print(head(joined_df))

# remove duplicates before joining data
cat("Number of rows before dropping duplicates:", nrow(df_vehcost), "\n")
df_vehcost <- unique(df_vehcost)
cat("Number of rows after dropping duplicates:", nrow(df_vehcost), "\n")

final_df <- merge(joined_df, df_vehcost[, c("join_key", "veh.value")], by = "join_key", all.x = TRUE)
final_df <- final_df %>% dplyr::select(-join_key)
print(head(final_df))

#Validating there are no duplicates
cat("Total rows before joining", nrow(joined_df), "\n")
cat("Total rows after joining", nrow(final_df), "\n")

#Validating there are no null values
cat("Null values in veh.value column: ", sum(is.na(final_df$veh.value)))
cat("Null valies in exposure column: ", sum(is.na(final_df$exposure)))
###Joining data successful without missing or duplicating data

####################### DATA EXPLORATION #############################################################

## Checking average veh.value falls into, given that the policy had a claim
mean(final_df$veh.value[final_df$claim == 1])

#Of veh.body types with more than 10 claims in the dataset, which veh.body type has the highest frequency
final_df %>%
  group_by(veh.body) %>%
  summarise(
    total_claims = sum(num.claims),
    total_exposure = sum(exposure),
    frequency = sum(num.claims)/sum(exposure)
  ) %>%
  filter(total_claims > 10)

# Response variable checks
hist(final_df$tot.cost[final_df$tot.cost > 0], breaks = 50, xlim = c(0, 20000),
     main = "Total Cost Distribution", xlab = "tot.cost", ylab = "Count")

# Check for overdispersion
cat("\nMean of claim count", mean(final_df$num.claims), "\n")
cat("\nVariance of claim count", var(final_df$num.claims), "\n")

#check for zero inflated claims
poisson_mod <- glm(num.claims ~ 1 + offset(log(exposure)), data = final_df, family = poisson)
actual_zeros <- sum(final_df$num.claims == 0)
expected_zeros <- sum(dpois(0,fitted(poisson_mod)))

cat("Actual zeros:", actual_zeros, "\n")
cat("Expected zeros:", expected_zeros, "\n")
# Mean and variance are very close, there is no overdispersion. Total predicted is close to total actual zero claims, so no evidence of zero inflation
#Poisson distribution will be considered if negative binomial performs similar to poisson

#checking skewness of possible numerical variables
hist(final_df$veh.age, main = "veh.age distribution") #nothing of note from veh.age

hist(final_df$veh.value, main = "Veh.Value distribution")
#veh.value seems to be right skewed. log transformation would be appropriate
#checking for zero values that could create issues in log transformation
sum(final_df$veh.value == 0)/nrow(final_df)
#About 4.4% of vehicles seem to have 0 veh.value..Logically doesn't make sense for a vehicle to have no value. 
sum(final_df$num.claims[final_df$veh.value == 0])/sum(final_df$num.claims)
#Need more clarity on the data and if veh.value = 0 is an error. As these are only 4% of total records, we can exclude them from the model
final_df <- final_df %>% filter(veh.value !=0)
final_df$log_veh.value = log(final_df$veh.value)
# Variable Exploration
var <- c("age.cat", "gender", "area", "veh.make", "veh.body", "veh.age")
for (i in var) {
  stat <- data.frame(sum   = tapply(final_df$num.claims, final_df[[i]], sum),
                     count = tapply(final_df$num.claims, final_df[[i]], length))
  cat("Sum by variable", i, "\n")
  print(stat)
  cat("\n")
}

# Looks like veh.body has a lot of levels, not ideal for glm. And not all levels have enough claims so will bin them base
profile <- data.frame(
  record_count = tapply(final_df$exposure, final_df$veh.body, sum),
  exposure     = tapply(final_df$exposure,   final_df$veh.body, sum),
  claim_count  = tapply(final_df$num.claims, final_df$veh.body, sum),
  claim_amount = tapply(final_df$tot.cost,   final_df$veh.body, sum)
)
profile$freq <- profile$claim_count / profile$exposure
profile$sev  <- profile$claim_amount / profile$claim_count
print(profile)

# Binning veh.body types bus, convt, mcara, rdstr, mibus, panvn (binning these as they have much less than 30 claim counts as well as less than 100 record counts)
final_df$vehbody_binned <- final_df$veh.body
final_df$vehbody_binned[final_df$vehbody_binned %in% c("CONVT", "BUS", "MCARA", "RDSTR", "MIBUS","PANVN")] <- "OTHER"
unique(final_df$vehbody_binned)

# Cramer's V to see correlation for categorical and drop any columns if required
library(vcd)
library(corrplot)
cols <- c("age.cat", "gender", "area", "veh.make", "vehbody_binned")
cram_df <- final_df[,cols]
vars <- names(cram_df)
n <- length(vars)
cv_matrix <- matrix(NA, n, n, dimnames = list(vars, vars))

for (i in seq_along(vars)) {
  for(j in seq_along(vars)){
    if(i == j){
      cv_matrix[i,j] <- 1
    } else {
      tbl <- table(cram_df[[i]], cram_df[[j]])
      cv_matrix[i,j] <- assocstats(tbl)$cramer
    }
  }
}

cv_df <- as.data.frame(cv_matrix)

corrplot(cv_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Cramer's V", mar = c(0,0,1,0))

# No correlation amongst categorical variable so not dropping any


# Checking correlation for continuos var
cont_cols <- c("veh.age", "veh.value")
corr <- cor(final_df[cont_cols], use = "complete.obs")
print(round(corr, 1))

# veh.age is correlated to veh.value, dropping either veh.value or veh.age based on significance from glm model
#final_df <- final_df %>% dplyr::select(-veh.value)

######### Model Fit #####################################################################################################################

# creating response variables
final_df$severity <- ifelse(final_df$num.claims == 0, 0, final_df$tot.cost / final_df$num.claims)
print(head(final_df))

##### Freq Model #######################

# set reference levels (choosing base levels based on the highest number of records)
final_df$age.cat        <- relevel(factor(final_df$age.cat), ref = "4")
final_df$gender         <- relevel(factor(final_df$gender), ref = "F")
final_df$area           <- relevel(factor(final_df$area), ref = "C")
final_df$veh.make       <- relevel(factor(final_df$veh.make), ref = "D")
final_df$vehbody_binned <- relevel(factor(final_df$vehbody_binned), ref = "SEDAN")

# Train-test split for frequency model. Doing the train test split by taking a 80/20 train/test random split
set.seed(17)
test_idx <- sample(seq_len(nrow(final_df)), size = floor(0.2 * nrow(final_df)))
train <- final_df[-test_idx, ]
test  <- final_df[test_idx, ]
test_copy <- test

# Fitting GLM with Poisson response, performing stepwise selection with AIC as evaluation criteria
pois_full <- glm(num.claims ~ age.cat + gender + area + veh.make + vehbody_binned + veh.age + log_veh.value + offset(log(exposure)),
                      data = train,
                      family = poisson(link="log"))
summary(pois_full)
pois_null <- glm(num.claims ~ 1 + offset(log(exposure)), data = train, family = poisson(link="log"))

step_model <- step(pois_null, scope = list(lower = pois_null, upper = pois_full),direction = "both")
summary(step_model)

test$predicted_claims <- predict(step_model, newdata = test, type = "response")

pois_full_trial <- glm(num.claims ~ age.cat + gender + area + veh.make + vehbody_binned + log_veh.value + offset(log(exposure)),
                 data = train,
                 family = poisson(link="log"))
summary(pois_full_trial)
pois_null_trial <- glm(num.claims ~ 1 + offset(log(exposure)), data = train, family = poisson(link="log"))

pois_trial <- step(pois_null_trial, scope = list(lower = pois_null_trial, upper = pois_full_trial),direction = "both")
summary(pois_trial)

test_copy$predicted_claims <- predict(pois_trial, newdata = test, type = "response")

pois_full_trial_2 <- glm(num.claims ~ (age.cat + gender + area + veh.make + vehbody_binned + log_veh.value)^2 + offset(log(exposure)),
                       data = train,
                       family = poisson(link="log"))

pois_trial_2 <- step(pois_null_trial, scope = list(lower = pois_null_trial, upper = pois_full_trial_2),direction = "both")
summary(pois_trial_2)
#### Comparing models with and without veh.age to see if it is worth including even though it is correlated with veh.value
compare_ap <- function(var) {
  data.frame(
    variable  = var,
    level     = as.character(tapply(test[[var]], test[[var]], function(x) x[1]) |> names()),
    actual    = tapply(test$num.claims,         test[[var]], sum),
    pred_m1   = tapply(test$predicted_claims,   test[[var]], sum),
    pred_m2   = tapply(test_copy$predicted_claims,   test_copy[[var]], sum)
  ) |>
    transform(ap_m1 = actual / pred_m1,
              ap_m2 = actual / pred_m2)
}
vars <- c("age.cat", "gender", "area", "veh.age", "vehbody_binned")

result <- do.call(rbind, lapply(vars, compare_ap))
total_row <- data.frame(
  variable = "Total",
  level    = "Total",
  actual   = sum(test$num.claims),
  pred_m1  = sum(test$predicted_claims),
  pred_m2  = sum(test_copy$predicted_claims)
) |>
  transform(ap_m1 = actual / pred_m1,
            ap_m2 = actual / pred_m2)

final_result <- rbind(total_row, result)
rownames(final_result) <- NULL
print(final_result)

#Keeping both veh.age and veh.value is not ideal as they are highly correlated. Comparing the AIC of models with veh.age, without veh.value and vice-versa, looks like including only veh.value is giving a better model
summary(pois_trial) #AIC 7843.8
summary(pois_trial_2) # 7839
####Selecting pois_trial_2 as the final poisson model (this is the stepwise selected model which include veh.value)


############## Fitting GLM with Negative Binomial response, stepwise selection on AIC criteria (excluding veh.age based on previous exercise)
nb_full <- glm.nb(num.claims ~ (age.cat + gender + area + veh.make + vehbody_binned + log_veh.value)^2 + offset(log(exposure)),
                       data = train)
nb_null <- glm.nb(num.claims ~ 1 + offset(log(exposure)),
                  data = train)
nb_step <- step(nb_null, scope = list(lower = nb_null, upper = nb_full),direction = "both")
summary(nb_step)

### Model Fit and comparison################################

#double lift chart
double_lift_plot <- function(model1, model2, df, n_bins = 10,
                             model1_label = "Poisson", model2_label = "Negative Binomial") {
df$model1_pred <- predict(model1, newdata = df, type = "response")
df$model2_pred <- predict(model2, newdata = df, type = "response")
df$model1_freq <- df$model1_pred/df$exposure
df$model2_freq <- df$model2_pred/df$exposure
df$actual_freq <- df$num.claims/df$exposure

df$sort_ratio <- df$model1_freq/df$model2_freq

model_lift <- df %>%
  arrange(sort_ratio) %>%
  mutate(bin = ntile(sort_ratio, 10)) %>%
  group_by(bin) %>%
  summarise(
    avg_actual = mean(actual_freq),
    avg_freq1 = mean(model1_freq),
    avg_freq2 = mean(model2_freq)
  ) %>%
  bind_rows(
    df %>% summarise(bin = NA, avg_actual = mean(actual_freq), avg_freq1 = mean(model1_freq), avg_freq2=mean(model2_freq))
  )
model_lift <- model_lift %>%
  mutate(
    avg_actual = avg_actual / model_lift$avg_actual[is.na(model_lift$bin)],
    avg_freq1 = avg_freq1 / model_lift$avg_freq1[is.na(model_lift$bin)],
    avg_freq2 = avg_freq2 / model_lift$avg_freq2[is.na(model_lift$bin)]
  ) %>%
  filter(!is.na(bin))

model_lift %>%
  pivot_longer(-bin, names_to = "Series", values_to = "Value") %>%
  mutate(
    bin    = paste0("D", bin),
    Series = case_when(
      Series == "avg_actual" ~ "Actual",
      Series == "avg_freq1"  ~ model1_label,
      Series == "avg_freq2"  ~ model2_label,
      TRUE ~ Series
    )
  ) %>%
  ggplot(aes(x = bin, y = Value, color = Series, group = Series)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 2.5) +
  scale_x_discrete(limits = paste0("D", 1:10)) +
  labs(title = "Double Lift Plot - Claim Frequency", x = NULL, y = "Ratio to Overall Average", color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "top")

}

double_lift_plot(model1 = pois_trial_2, model2 = nb_step, test)
## lift charts for poisson and nb models looks pretty similar, but model 1 is closer to actual at lower deciles
compare_ap <- function(var, model1, model2, data, actual_col = "num.claims") {
  data$pred_m1 <- predict(model1, newdata = data, type = "response")
  data$pred_m2 <- predict(model2, newdata = data, type = "response")
  
  data.frame(
    variable = var,
    level    = names(tapply(data[[actual_col]], data[[var]], sum)),
    actual   = tapply(data[[actual_col]], data[[var]], sum),
    pred_m1  = tapply(data$pred_m1, data[[var]], sum),
    pred_m2  = tapply(data$pred_m2, data[[var]], sum)
  ) %>%
    mutate(ap_m1 = actual / pred_m1,
           ap_m2 = actual / pred_m2)
}

vars <- c("age.cat", "gender", "area", "veh.age", "vehbody_binned")
result <- do.call(rbind, lapply(vars, compare_ap, model1 = pois_trial_2, model2 = nb_step, data = test))
result
## The average to predicted ratio for both the models are almost equal. The green line in double lift plot is almost 

######## Severity Model #####################

# filtering data to include ids which have claims
cat("Total records before dropping is", nrow(final_df), "and total number of claims is", sum(final_df$num.claims), "\n")
severity_df <- final_df[final_df$num.claims > 0, ]
cat("Total records after dropping is", nrow(severity_df), "and total number of claims is", sum(severity_df$num.claims), "\n")
# There is no loss of claims after filtering.


# Binning variables
var <- "veh.make"
profile <- data.frame(
  record_count = tapply(final_df$exposure, final_df[[var]], sum),
  exposure     = tapply(final_df$exposure,   final_df[[var]], sum),
  claim_count  = tapply(final_df$num.claims, final_df[[var]], sum),
  claim_amount = tapply(final_df$tot.cost,   final_df[[var]], sum)
)
profile$freq <- profile$claim_count / profile$exposure
profile$sev  <- profile$claim_amount / profile$claim_count
print(profile)
#vehbody is already binned, will continue with it
unique(final_df$vehbody_binned)

## Initial model fit without addressing outliers resulted in a very bad performance on test set. So addressing the outliers
percentile_95 <- quantile(severity_df$severity, 0.95)
percentile_99 <- quantile(severity_df$severity, 0.99)
max_sev <- max(severity_df$severity)
cat("\nThe 95th percentile is", percentile_95, "and 99th percentile is", percentile_99, "and the max value is", max_sev, "\n")

hist(severity_df$severity, breaks = 50, xlim = c(0, 20000),
     main = "Severity before capping outlier", xlab = "Severity")

severity_df$severity <- pmin(severity_df$severity, quantile(severity_df$severity, 0.99))

hist(severity_df$severity, breaks = 50, xlim = c(0, 20000),
     main = "Severity after capping outlier", xlab = "Severity")

model_cols <- c("age.cat", "gender", "area", "veh.make", "vehbody_binned", "veh.age","log_veh.value","exposure")

sev <- severity_df[c(model_cols, "severity")]

sev$age.cat        <- relevel(factor(sev$age.cat), ref = "4")
sev$gender         <- relevel(factor(sev$gender), ref = "F")
sev$area    <- relevel(factor(sev$area), ref = "C")
sev$veh.make <- relevel(factor(sev$veh.make), ref = "D")
sev$vehbody_binned <- relevel(factor(sev$vehbody_binned), ref = "SEDAN")

# train test split
set.seed(17)
test_idx <- sample(seq_len(nrow(sev)), size = floor(0.2 * nrow(sev)))
train <- sev[-test_idx, ]
test  <- sev[test_idx, ]

# Gamma GLM with log link, stepwise selection on AIC criteria
gamma_full <- glm(severity ~ age.cat + gender + area + veh.make + vehbody_binned + log_veh.value,
                data = train, family = Gamma(link = "log"))
gamma_null <- glm(severity ~ 1,
                  data = train, family = Gamma(link = "log"))
gamma_step <- step(gamma_null, scope = list(lower = gamma_null, upper = gamma_full),direction = "both")
summary(gamma_step)

gamma_full2 <- glm(severity ~ age.cat + gender + area + veh.make + vehbody_binned + veh.age,
                  data = train, family = Gamma(link = "log"))
##Not including interactions as they we not significant and were removed in stepwise selection
## Compared keeping veh.value vs veh.age using the compare_sev function below, and there is not much difference in the performance so removed veh.age

# Inverse Gaussian with log link, stepwise selection on AIC criteria
invgaus_full <- glm(severity ~ age.cat + gender + area + veh.make + vehbody_binned + log_veh.value,
                    data = train, family = inverse.gaussian(link = "log"))
invgaus_null <- glm(severity ~ 1,
                    data = train, family = inverse.gaussian(link = "log"))
invgaus_step <- step(invgaus_null, scope = list(lower = invgaus_null, upper = invgaus_full),direction="both")
summary(gamma_step)

### Most likely would select full model as stepwise selection eliminates most of the variables. As it is we have less number of variables to work with


##checking better model inv gaussian vs gamma using deviance residual
par(mfrow = c(2, 2))

# Gamma
res_gamma <- residuals(gamma_full, type = "deviance")
hist(res_gamma, freq = FALSE, main = "Gamma - Histogram", xlab = "Deviance Residual")
curve(dnorm(x, mean(res_gamma), sd(res_gamma)), add = TRUE, col = "blue")
qqnorm(res_gamma, main = "Gamma - Q-Q Plot"); qqline(res_gamma, col = "blue")

# Inverse Gaussian
res_ig <- residuals(invgaus_full, type = "deviance")
hist(res_ig, freq = FALSE, main = "Inv Gaussian - Histogram", xlab = "Deviance Residual")
curve(dnorm(x, mean(res_ig), sd(res_ig)), add = TRUE, col = "blue")
qqnorm(res_ig, main = "Inv Gaussian - Q-Q Plot"); qqline(res_ig, col = "blue")

par(mfrow = c(1, 1))

# gamma seems to be systematically over predicting. QQ plot curves significantly above line suggesting a heavier tail and inverse gaussian is doing better
## inclined to go with inverse gaussian. will check the Actual to predicted ratios
test$pred_gamma <- predict(gamma_full,   newdata = test, type = "response")
test$pred_ig    <- predict(invgaus_full, newdata = test, type = "response")

compare_sev <- function(var) {
  data.frame(
    variable  = var,
    level     = names(tapply(test$severity,    test[[var]], mean)),
    actual    = tapply(test$severity,    test[[var]], mean),
    pred_gamma= tapply(test$pred_gamma,  test[[var]], mean),
    pred_ig   = tapply(test$pred_ig,     test[[var]], mean)
  ) %>%
    mutate(ap_gamma = actual / pred_gamma,
           ap_ig    = actual / pred_ig)
}

vars <- c("age.cat", "gender", "area", "veh.make", "vehbody_binned", "veh.age")
result <- do.call(rbind, lapply(vars, compare_sev))

total_row <- data.frame(
  variable   = "Total",
  level      = "Total",
  actual     = mean(test$severity),
  pred_gamma = mean(test$pred_gamma),
  pred_ig    = mean(test$pred_ig)
) %>%
  mutate(ap_gamma = actual / pred_gamma,
         ap_ig    = actual / pred_ig)

final_result <- rbind(total_row, result)
rownames(final_result) <- NULL
print(final_result)

# actual vs predicted tells another story. Although similar, Gamma seems to predict better than inverse gaussian
# I would go ahead with inverse gaussian as that handles the thicker tail nature of data better than gamma. One main reason I would like to put less weightage on actual to predicted in this case is because our holdout set is small and model might not have enough information to give a clearer full picture


### tot.claims A to P 
test_copy$freq_pred <- predict(pois_trial_2, newdata = test_copy, type = "response")
test_copy$sev_pred <- predict(invgaus_full, newdata = test_copy, type = "response")
test_copy$tot.claim_pred <- test_copy$freq_pred*test_copy$sev_pred

compare_tot <- function(var) {
  data.frame(
    variable = var,
    level    = names(tapply(test_copy$tot.cost,       test_copy[[var]], mean)),
    actual   = tapply(test_copy$tot.cost,       test_copy[[var]], mean),
    pred     = tapply(test_copy$tot.claim_pred, test_copy[[var]], mean)
  ) %>%
    mutate(ap = actual / pred)
}

vars <- c("age.cat", "gender", "area", "veh.make", "vehbody_binned", "veh.age")
result_tot <- do.call(rbind, lapply(vars, compare_tot))

total_row_tot <- data.frame(
  variable = "Total",
  level    = "Total",
  actual   = mean(test_copy$tot.cost),
  pred     = mean(test_copy$tot.claim_pred)
) %>%
  mutate(ap = actual / pred)

final_result_tot <- rbind(total_row_tot, result_tot)
rownames(final_result_tot) <- NULL
print(final_result_tot)

# Table 1: Combined A/P table by segment
vars_table <- c("age.cat", "gender", "area", "veh.make", "vehbody_binned")

# Frequency A/P from test_copy
freq_ap <- do.call(rbind, lapply(vars_table, function(var) {
  data.frame(
    Segment  = var,
    Category = names(tapply(test_copy$num.claims, test_copy[[var]], sum)),
    AP_Freq  = round(tapply(test_copy$num.claims, test_copy[[var]], sum) /
                       tapply(test_copy$freq_pred,  test_copy[[var]], sum), 3)
  )
})) %>%
  rbind(data.frame(Segment = "Total", Category = "Total",
                   AP_Freq = round(sum(test_copy$num.claims) / sum(test_copy$freq_pred), 3)))

# Severity A/P from severity test set
sev_ap <- do.call(rbind, lapply(vars_table, function(var) {
  data.frame(
    Segment  = var,
    Category = names(tapply(test$severity, test[[var]], mean)),
    AP_Sev   = round(tapply(test$severity, test[[var]], mean) /
                       tapply(test$pred_ig,  test[[var]], mean), 3)
  )
})) %>%
  rbind(data.frame(Segment = "Total", Category = "Total",
                   AP_Sev = round(mean(test$severity) / mean(test$pred_ig), 3)))

# Total claims A/P from final_result_tot (already computed)
tot_ap <- final_result_tot %>%
  filter(variable != "veh.age") %>%
  transmute(
    Segment  = ifelse(variable == "Total", "Total", variable),
    Category = ifelse(variable == "Total", "Total", as.character(level)),
    AP_Tot   = round(ap, 3)
  )

# Combine all three
ap_table <- freq_ap %>%
  left_join(sev_ap, by = c("Segment", "Category")) %>%
  left_join(tot_ap, by = c("Segment", "Category"))

rownames(ap_table) <- NULL
print(ap_table)
write_xlsx(ap_table, "ap_table.xlsx")
# Figure 3: A/P ratio bar chart by segment (total claims, holdout)
plot_ap <- final_result_tot %>%
  filter(variable != "Total", variable != "veh.age") %>%
  mutate(variable = case_when(
    variable == "age.cat" ~ "Age Category",
    variable == "gender"~ "Gender",
    variable == "area"~ "Area",
    variable == "veh.make"~ "Vehicle Make",
    variable == "vehbody_binned" ~ "Vehicle Body",
    TRUE ~ variable
  ))

ggplot(plot_ap, aes(x = level, y = ap, fill = ap >= 1)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_text(aes(label = round(ap, 2),
                vjust = ifelse(ap >= 1, -0.4, 1.3)),
            size = 3.2, color = "black") +
  annotate("label", x = Inf, y = Inf,
           label = paste0("Overall A/P: ", round(final_result_tot$ap[final_result_tot$variable == "Total"], 3)),
           hjust = 1.1, vjust = 1.5,
           size = 4, fontface = "bold",
           fill = "lightyellow", color = "black", label.size = 0.5) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"), guide = "none") +
  labs(
    title = "Tot.cost Actual-to-Predicted Ratios by Rating Factor (Total Claims, Holdout)",
    x = NULL, y = "A/P Ratio"
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table 2: Model coefficients and rate relativities
freq_coef <- as.data.frame(summary(pois_trial_2)$coefficients) %>%
  mutate(Predictor = rownames(.)) %>%
  filter(!grepl(":", Predictor), Predictor != "(Intercept)") %>%
  transmute(Predictor,
            Freq_Est = round(Estimate, 4),
            Freq_Rel = round(exp(Estimate), 3))

sev_coef <- as.data.frame(summary(invgaus_full)$coefficients) %>%
  mutate(Predictor = rownames(.)) %>%
  filter(Predictor != "(Intercept)") %>%
  transmute(Predictor,
            Sev_Est = round(Estimate, 4),
            Sev_Rel = round(exp(Estimate), 3))

coef_table <- full_join(freq_coef, sev_coef, by = "Predictor") %>%
  mutate(Tot_Rel = round(
    ifelse(is.na(Freq_Rel), 1, Freq_Rel) *
      ifelse(is.na(Sev_Rel),  1, Sev_Rel), 3)
  ) %>%
  arrange(Predictor)

rownames(coef_table) <- NULL
print(coef_table)
write_xlsx(coef_table, "coef_table.xlsx")
########################### Predicting the SetA.test file ###################################################################################
test_raw <- read.csv("SetA.test.csv")
test_raw$gender <- trimws(test_raw$gender)

# Save original column names to restore at export
orig_cols <- names(test_raw)

# Join exposure and vehicle cost
test_raw <- merge(test_raw, df_exposure, by = "id", all.x = TRUE)

cols <- c("veh.make", "veh.body", "veh.age")
test_raw$join_key <- apply(test_raw[, cols], 1, paste, collapse = "_")
test_raw <- merge(test_raw, df_vehcost[, c("join_key", "veh.value")], by = "join_key", all.x = TRUE)
test_raw <- test_raw %>% dplyr::select(-join_key)

# If veh.value is 0, then prediction will be done assuming median veh.value
median_veh_value <- median(final_df$veh.value[final_df$veh.value > 0])
test_raw$log_veh.value <- log(ifelse(test_raw$veh.value == 0, median_veh_value, test_raw$veh.value))

test_raw$vehbody_binned <- test_raw$veh.body
test_raw$vehbody_binned[test_raw$vehbody_binned %in% c("CONVT","BUS","MCARA","RDSTR","MIBUS","PANVN")] <- "OTHER"

test_raw$age.cat        <- relevel(factor(test_raw$age.cat),        ref = "4")
test_raw$gender         <- relevel(factor(test_raw$gender),         ref = "F")
test_raw$area           <- relevel(factor(test_raw$area),           ref = "C")
test_raw$veh.make       <- relevel(factor(test_raw$veh.make),       ref = "D")
test_raw$vehbody_binned <- relevel(factor(test_raw$vehbody_binned), ref = "SEDAN")

# Predict num.claims using Poisson model (final model is pois_trial)
test_raw$num.claims <- predict(pois_trial_2, newdata = test_raw, type = "response")

# Predict severity using Inverse Gaussian model
test_raw$severity_pred <- predict(invgaus_full, newdata = test_raw, type = "response")

# Calculate tot.cost and claim
test_raw$tot.cost <- test_raw$num.claims * test_raw$severity_pred
test_raw$claim <- 1 - exp(-test_raw$num.claims)
#K <- round(sum(test_raw$num.claims))
#thresh <- sort(test_raw$probclaim, decreasing = TRUE)[K]
#test_raw$claim    <- as.integer(test_raw$probclaim >= thresh) 

# Export: original columns only, with three columns now filled
output <- test_raw[, orig_cols]
write.csv(output, "SetA.test.csv", row.names = FALSE)

