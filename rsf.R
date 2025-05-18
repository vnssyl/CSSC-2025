library(data.table)
library(readxl)
library(fst)
library(corrplot)
library(survival)
library(caret)
library(gbm)
library(survivalROC)
library(purrr)
library(survAUC) #For calculating C index
library(mice) # for imp
library(glmnet)
library(randomForestSRC)
library(pROC)

path <- "../"


# imputed train data
filename <- "imputed_train_data.fst"
imputed_train_data <- read_fst(paste0(path, filename, sep = ""))



# test data
filename <- "test_data.fst"
test_data <- read_fst(paste0(path, filename, sep = ""))


outcome_vars <- c(
  "patient_id", "event_afib", "time_afib"
)

predictor_vars <- setdiff(
  names(imputed_train_data),
  c(outcome_vars)
)

# Prepare data for training
rsf_data <- imputed_train_data[, c("time_afib", "event_afib", predictor_vars)]


obj <- rfsrc(Surv(time_afib,event_afib)~., data = rsf_data)


print(obj)

# Sample size: 7057
# Number of events: 6837, 220
# Number of trees: 500
# Forest terminal node size: 15
# Average no. of terminal nodes: 522.448
# No. of variables tried at each split: 11
# Total no. of variables: 106
# Resampling used to grow trees: swor
# Resample size used to grow trees: 4460
# Analysis: RSF
# Family: surv-CR
# Splitting rule: logrankCR *random*
#   Number of random split points: 10
# (OOB) Requested performance error: 0.28264857, 0.61284765


expected_vars <- obj$forest$xvar.names
cc <- complete.cases(test_data[, expected_vars, drop = FALSE])
test_rsf_data <- test_data[cc_final, expected_vars, drop = FALSE]

o.pred <- predict(obj, newdata = test_rsf_data)

# Assign predictions correctly
test_data$predicted <- NA
test_data$predicted[which(cc)] <- o.pred$predicted[,2]

# AUC calculations
library(timeROC)

TRoc <- timeROC(
  T       = test_data$time_afib,
  delta   = test_data$event_afib,
  marker  = test_data$predicted,
  cause   = 1,
  weighting = "marginal",
  times     = c(182, 365, 730)
)


# t=182     t=365     t=730 
# 0.8086806 0.8118263 0.8416176 
TRoc$AUC_1 

# t=182     t=365     t=730 
# 0.8098108 0.8132447 0.8431505  
TRoc$AUC_2


T_train <- imputed_train_data$time_afib
delta_train <- imputed_train_data$event_afib
T_test <- test_data$time_afib
delta_test <- test_data$event_afib
lp_test <- test_data$predicted

times <- c(182, 365, 730)
cindexes <- list()

for (time in times){
  cindex <- UnoC(
    Surv.rsp = Surv(T_train, delta_train),
    Surv.rsp.new = Surv(T_test[cc], delta_test[cc]),
    lpnew = lp_test[cc],
    time = time
  )
  name <- paste0(time/365, "-year C-Index")
  cindexes[[name]] <- cindex
}

print(cindexes)

# confusion matrix

