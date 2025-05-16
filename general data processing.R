library(tidyverse)
library(glmnet)
library(survival)
library(data.table)
library(dplyr)

# load original dataset
path <- "../"
filename <- "synthetic_data_stats_competition_2025_final.xlsx"

data <- read_excel(paste(path, filename, sep=""), sheet = "in")
data <- as.data.table(data)

# impute NAs in event indicator; for patients with time to outcome is NA, 
# assume event did not occur for patients within fu time
data$outcome_afib_aflutter_new_post <- ifelse(is.na(data$time_to_outcome_afib_aflutter_new_post), 0, 1)
sum(is.na(data$outcome_afib_aflutter_new_post)) # verify; should be 0

# define survival time and rename event indicator
setnames(data, "outcome_afib_aflutter_new_post", "event_afib", skip_absent=TRUE)
data[, time_afib := fifelse(
  event_afib == 1,
  time_to_outcome_afib_aflutter_new_post,
  fifelse(
    outcome_all_cause_death == 1,
    time_to_outcome_all_cause_death,
    follow_up_duration
  )
)]
data <- data[!is.na(time_afib)]

# find missing pct
missing_percent <- data[, lapply(.SD, function(x) sum(is.na(x)) / .N)]

outcome_vars <- c(
  "event_afib",
  "time_afib"
)

drop_vars <- names(missing_percent)[
  (unlist(missing_percent) >= 0.3) & 
    !(names(missing_percent) %in% outcome_vars)
]

cat("Variables dropped:", drop_vars, "\n")

# final vars to keep
final_vars <- union(
  union("patient_id", outcome_vars),
  setdiff(names(data), drop_vars)
)

data <- data[, ..final_vars]

# correct variable types
for (col in names(data)) {
  if (is.character(data[[col]])) next
  if (is.numeric(data[[col]])) {
    uniq_vals <- unique(na.omit(data[[col]]))
    if (length(uniq_vals) == 2) {
      data[, (col) := factor(data[[col]])]
    } else {
      data[, (col) := as.numeric(data[[col]])]
    }
  }
}



# train test split (70% train 30%)
set.seed(123)
train_indices <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[train_indices] # This one to feed into model
test_data <- data[-train_indices] # Use this one for AUC, C-index, etc

# impute on train data
set.seed(123)
imputed_train_data <- mice(train_data, method = 'pmm', m = 5, maxit = 10)
imputed_train_data <- complete(imputed_train_data, 1)

# # variable selection
# imputed_train_data$time_afib[imputed_train_data$time_afib <= 0] <- 1e-8
# 
# outcome_vars <- c(
#   "patient_id", "event_afib", "time_afib",
#   "outcome_afib_aflutter_new_post",
#   "time_to_outcome_afib_aflutter_new_post",
#   "outcome_all_cause_death",
#   "time_to_outcome_all_cause_death",
#   "follow_up_duration"
# )
# 
# ## Define predictor variables
# predictor_vars <- setdiff(names(imputed_train_data), outcome_vars)
# 
# ## Keep only numeric predictors
# numeric_check <- sapply(imputed_train_data[, predictor_vars, drop = FALSE], is.numeric)
# numeric_vars <- predictor_vars[numeric_check]
# 
# ## Initialize storage for all selected variables
# selected_all <- c()
# 
# ## Run Lasso Cox 5 times
# set.seed(123)
# for (i in 1:5) {
#   x <- as.matrix(imputed_train_data[, numeric_vars])
#   y <- Surv(time = imputed_train_data$time_afib, event = imputed_train_data$event_afib)
#   
#   lasso_fit <- cv.glmnet(x, y, family = "cox", alpha = 1)
#   coef_matrix <- coef(lasso_fit, s = lasso_fit$lambda.min)
#   selected_vars <- rownames(coef_matrix)[which(coef_matrix != 0)]
#   
#   cat(paste("Run", i, "- Selected variables:\n"))
#   print(selected_vars)
#   
#   selected_all <- c(selected_all, selected_vars)
# }
# 
# ## Create data frame of frequencies
# selected_df <- as.data.frame(table(selected_all))
# colnames(selected_df) <- c("Variable", "Frequency")
# selected_df <- selected_df[order(-selected_df$Frequency), ]
# 
# ## Show data frame
# print(selected_df)
# 
# ## Plot histogram
# barplot(
#   selected_df$Frequency,
#   names.arg = selected_df$Variable,
#   las = 2,
#   col = "skyblue",
#   main = "Variable Selection Frequency (5 Lasso Runs)",
#   ylab = "Selection Count",
#   cex.names = 0.7
# )
# 
# selected_data <- imputed_train_data[ , names(imputed_train_data) %in% selected_all]

# write prepared train and test data
imputed_train_data <- imputed_train_data %>% select(-c(follow_up_duration,outcome_all_cause_death))
write_fst(imputed_train_data, paste0(path, "imputed_train_data.fst", sep = ""))

test_data <- test_data %>% select(-c(follow_up_duration,outcome_all_cause_death))
write_fst(test_data, paste0(path, "test_data.fst", sep = ""))
