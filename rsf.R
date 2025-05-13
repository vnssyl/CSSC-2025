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


train_data <- as.data.frame(train_data[,7:114])


obj <- rfsrc(Surv(time_afib,event_afib)~., data = train_data)


print(obj)


obj2 <- rfsrc(Surv(time_afib,event_afib)~., train_data,
             ntree = 1000, nodesize = 5, nsplit = 50)
print(obj2)



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
test_data$predicted_risk <- o.pred$predicted

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


