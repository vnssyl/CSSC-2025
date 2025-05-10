library(tidyverse)
library(glmnet)
library(survival)
library(data.table)

# load original dataset
path <- "../"
filename <- "synthetic_data_stats_competition_2025_final.xlsx"

data <- read_excel(paste(path, filename, sep=""), sheet = "in")
data <- as.data.table(data)

# find missing pct
missing_percent <- data[, lapply(.SD, function(x) sum(is.na(x)) / .N)]

outcome_vars <- c(
  "outcome_afib_aflutter_new_post",
  "time_to_outcome_afib_aflutter_new_post",
  "outcome_all_cause_death",
  "time_to_outcome_all_cause_death",
  "follow_up_duration"
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

# define survival time and type
data[, event_afib := as.integer(outcome_afib_aflutter_new_post == 1)]
data[, time_afib := fifelse(
  outcome_afib_aflutter_new_post == 1,
  time_to_outcome_afib_aflutter_new_post,
  fifelse(
    outcome_all_cause_death == 1,
    time_to_outcome_all_cause_death,
    follow_up_duration
  )
)]
data <- data[!is.na(time_afib)]

# variable selection
data$time_afib[data$time_afib <= 0] <- 1e-8

outcome_vars <- c(
  "patient_id", "event_afib", "time_afib",
  "outcome_afib_aflutter_new_post",
  "time_to_outcome_afib_aflutter_new_post",
  "outcome_all_cause_death",
  "time_to_outcome_all_cause_death",
  "follow_up_duration"
)

## Define predictor variables
data <- as.data.frame(data)
predictor_vars <- setdiff(names(data), outcome_vars)

## Keep only numeric predictors
numeric_check <- sapply(data[, predictor_vars, drop = FALSE], is.numeric)
numeric_vars <- predictor_vars[numeric_check]

# Create design matrix for glmnet
x <- as.matrix(data[, numeric_vars, drop = FALSE])
y <- Surv(time = data$time_afib, event = data$event_afib)

## Initialize storage for all selected variables
selected_all <- c()

## Run Lasso Cox 5 times
set.seed(123)
for (i in 1:5) {
  x <- as.matrix(data[, numeric_vars])
  y <- Surv(time = data$time_afib, event = data$event_afib)
  
  lasso_fit <- cv.glmnet(x, y, family = "cox", alpha = 1)
  coef_matrix <- coef(lasso_fit, s = lasso_fit$lambda.min)
  selected_vars <- rownames(coef_matrix)[which(coef_matrix != 0)]
  
  cat(paste("Run", i, "- Selected variables:\n"))
  print(selected_vars)
  
  selected_all <- c(selected_all, selected_vars)
}

## Create data frame of frequencies
selected_df <- as.data.frame(table(selected_all))
colnames(selected_df) <- c("Variable", "Frequency")
selected_df <- selected_df[order(-selected_df$Frequency), ]

## Show data frame
print(selected_df)

## Plot histogram
barplot(
  selected_df$Frequency,
  names.arg = selected_df$Variable,
  las = 2,
  col = "skyblue",
  main = "Variable Selection Frequency (5 Lasso Runs)",
  ylab = "Selection Count",
  cex.names = 0.7
)

# train test split (70% train 30%)
set.seed(123)
train_indices <- sample(seq_len(nrow(selected_df)), size = 0.7 * nrow(selected_df))
train_data <- selected_df[train_indices] # This one to feed into model
test_data <- selected_df[-train_indices] # Use this one for AUC, C-index, etc

# impute on train data
set.seed(123)
imputed_data <- mice(train_data, method = 'pmm', m = 5, maxit = 10)
data <- complete(imputed_data, 1)
setDT(train_data)

# write prepared train and test data
write_fst(train_data, paste0(path, "train_data", sep = ""))
write_fst(test_data, paste0(path, "test_data", sep = ""))
