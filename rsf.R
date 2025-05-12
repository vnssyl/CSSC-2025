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

train_data <- as.data.frame(train_data[,7:114])


obj <- rfsrc(Surv(time_afib,event_afib)~., train_data,
             ntree = 100, nodesize = 5, nsplit = 50, importance = TRUE)

print(obj)

# c index
get.cindex(obj$yvar[,1], obj$yvar[,2], obj$predicted.oob)

# variable importance
jk.obj <- subsample(obj)
pdf("VIMPsur.pdf", width = 15, height = 20)
par(oma = c(0.5, 10, 0.5, 0.5))
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,17,1,1), mgp = c(4, 1, 0))
plot(jk.obj, xlab = "Variable Importance (x 100)", cex = 1.2)
dev.off()


