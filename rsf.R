library(data.table)
library(readxl)
library(fst)
library(corrplot)
library(survival)
library(caret)
library(gbm)
library(survivalROC)
library(purrr)
# library(survAUC) #For calculating C index
library(mice) # for imp
library(glmnet)
library(randomForestSRC)

train_data <- read_fst("C:/Users/tanzh/OneDrive/Desktop/case study/imputed_train_data.fst")

test_data <- read_fst("C:/Users/tanzh/OneDrive/Desktop/case study/test_data.fst")



test_data <- as.data.frame(test_data[, 7:114], with = FALSE)

train_data <- as.data.frame(train_data[,7:114], with = FALSE)


obj <- rfsrc(Surv(time_afib,event_afib)~., data = train_data)

# print(obj)
# Sample size: 74722
# Number of deaths: 4512
# Number of trees: 500
# Forest terminal node size: 15
# Average no. of terminal nodes: 907.582
# No. of variables tried at each split: 11
# Total no. of variables: 106
# Resampling used to grow trees: swor
# Resample size used to grow trees: 47224
# Analysis: RSF
# Family: surv
# Splitting rule: logrank *random*
#   Number of random split points: 10
# (OOB) CRPS: 103.15390764
# (OOB) stand. CRPS: 0.02055268
# (OOB) Requested performance error: 0.02901516




# c index
get.cindex(obj$yvar[,1], obj$yvar[,2], obj$predicted.oob)

get.cindex(obj$yvar[,1], obj$yvar[,2], obj$predicted)


# variable importance
jk.obj <- subsample(obj)
# pdf("VIMPsur.pdf", width = 15, height = 20)
par(oma = c(0.5, 10, 0.5, 0.5))
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,17,1,1), mgp = c(4, 1, 0))
plot(jk.obj, xlab = "Variable Importance (x 100)", cex = 1.2)
# dev.off()






# Predict risk scores (linear predictor) on test set
o.pred <- predict(obj, newdata = test_data)
cc <- complete.cases(test_data)
test_data$predicted_risk <- NA      # initialize
test_data$predicted_risk[cc] <- o.pred$predicted


# 6 month AUC = 0.9802
test_data$binary_afib_6mo <- NA

for (i in 1:nrow(test_data)) {
  time_to_afib <- test_data$time_afib[i]
  afib_event <- test_data$event_afib[i]
  
  if (time_to_afib <= 182 && afib_event == 1) {
    test_data$binary_afib_6mo[i] <- 1
  } else {
    test_data$binary_afib_6mo[i] <- 0
  }
}

roc_obj <- roc(response = test_data$binary_afib_6mo,
               predictor = test_data$predicted_risk,
               direction = "<")

auc_val <- auc(roc_obj)
print(auc_val)


# 4 year AUC = 0.9891
test_data$binary_afib_4yr <- NA

for (i in 1:nrow(test_data)) {
  time_to_afib <- test_data$time_afib[i]
  afib_event <- test_data$event_afib[i]
  
  if (time_to_afib <= 1480 && afib_event == 1) {
    test_data$binary_afib_4yr[i] <- 1
  } else {
    test_data$binary_afib_4yr[i] <- 0
  }
}

roc_obj <- roc(response = test_data$binary_afib_4yr,
               predictor = test_data$predicted_risk,
               direction = "<")

auc_val <- auc(roc_obj)
print(auc_val)


# 5 year AUC = 0.9902
test_data$binary_afib_5yr <- NA

for (i in 1:nrow(test_data)) {
  time_to_afib <- test_data$time_afib[i]
  afib_event <- test_data$event_afib[i]
  
  if (time_to_afib <= 1852 && afib_event == 1) {
    test_data$binary_afib_5yr[i] <- 1
  } else {
    test_data$binary_afib_5yr[i] <- 0
  }
}

roc_obj <- roc(response = test_data$binary_afib_5yr,
               predictor = test_data$predicted_risk,
               direction = "<")

auc_val <- auc(roc_obj)
print(auc_val)


# 1 year AUC = 0.9807
test_data$binary_afib_1yr <- NA

for (i in 1:nrow(test_data)) {
  time_to_afib <- test_data$time_afib[i]
  afib_event <- test_data$event_afib[i]
  
  if (time_to_afib <= 365 && afib_event == 1) {
    test_data$binary_afib_1yr[i] <- 1
  } else {
    test_data$binary_afib_1yr[i] <- 0
  }
}

roc_obj <- roc(response = test_data$binary_afib_1yr,
               predictor = test_data$predicted_risk,
               direction = "<")

auc_val <- auc(roc_obj)
print(auc_val)

# 2 year AUC = 0.9807
test_data$binary_afib_2yr <- NA

for (i in 1:nrow(test_data)) {
  time_to_afib <- test_data$time_afib[i]
  afib_event <- test_data$event_afib[i]
  
  if (time_to_afib <= 730 && afib_event == 1) {
    test_data$binary_afib_2yr[i] <- 1
  } else {
    test_data$binary_afib_2yr[i] <- 0
  }
}

roc_obj <- roc(response = test_data$binary_afib_1yr,
               predictor = test_data$predicted_risk,
               direction = "<")

auc_val <- auc(roc_obj)
print(auc_val)


# c index on test data = 0.9847894
library(nftbart)
test_data$times <- ifelse(is.na(test_data$predicted_risk),NA, test_data$time_afib)
test_data$status <- ifelse(is.na(test_data$predicted_risk),NA, test_data$event_afib)
Cindex(test_data$predicted_risk, test_data$times, test_data$status)
