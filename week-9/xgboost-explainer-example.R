

#example of a single decision tree--not great at predicting but very interpretable
#example of xgboost -- great at predicting, but not very interpretable
#example to make xgboost interpretable

#load packages
library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)

#read data
set.seed(123)
full = fread('week-9/turnover.csv', stringsAsFactors = T)
full = full[sample(.N)]

#### Add Random Noise
tmp_std = sd(full[,satisfaction_level])
full[,satisfaction_level:=satisfaction_level + runif(.N,-tmp_std,tmp_std)]
full[,satisfaction_level:=satisfaction_level - min(satisfaction_level)]
full[,satisfaction_level:=satisfaction_level / max(satisfaction_level)]
tmp_std = sd(full[,last_evaluation])
full[,last_evaluation:=last_evaluation + runif(.N,-tmp_std,tmp_std) ]
full[,last_evaluation:=last_evaluation - min(last_evaluation)]
full[,last_evaluation:=last_evaluation / max(last_evaluation)]
tmp_min = min(full[,number_project])
tmp_std = sd(full[,number_project])
full[,number_project:=number_project + sample(-ceiling(tmp_std):ceiling(tmp_std),.N, replace=T)]
full[,number_project:=number_project - min(number_project) + tmp_min]
tmp_min = min(full[,average_montly_hours])
tmp_std = sd(full[,average_montly_hours])
full[,average_montly_hours:=average_montly_hours + sample(-ceiling(tmp_std):ceiling(tmp_std),.N, replace=T)]
full[,average_montly_hours:=average_montly_hours - min(average_montly_hours) + tmp_min]
tmp_min = min(full[,time_spend_company])
tmp_std = sd(full[,time_spend_company])
full[,time_spend_company:=time_spend_company + sample(-ceiling(tmp_std):ceiling(tmp_std),.N, replace=T)]
full[,time_spend_company:=time_spend_company - min(time_spend_company) + tmp_min]
tmp_min = min(full[,number_project])
tmp_std = sd(full[,number_project])
full[,number_project:=number_project + sample(-ceiling(tmp_std):ceiling(tmp_std),.N, replace=T)]
full[,number_project:=number_project - min(number_project) + tmp_min]



#### Create Train / Test and Folds
train = full[1:12000]
test = full[12001:14999]
cv <- createFolds(train[,left], k = 10)
# Control
ctrl <- trainControl(method = "cv",index = cv)

#### Train Tree
tree.cv <- train(x = train[,-"left"], y = as.factor(train[,left]), method = "rpart2", tuneLength = 7,
                 trControl = ctrl, control = rpart.control())
tree.model = tree.cv$finalModel
rpart.plot(tree.model, type = 2,extra =  7,fallen.leaves = T)
rpart.plot(tree.model, type = 2,extra =  2,fallen.leaves = T)
tree.preds = predict(tree.model, test)[,2]
tree.roc_obj <- roc(test[,left], tree.preds)
cat("Tree AUC ", auc(tree.roc_obj))

#### Train XGBoost
xgb.train.data = xgb.DMatrix(data.matrix(train[,-'left']), label = train[,left], missing = NA)
param <- list(objective = "binary:logistic", base_score = 0.5)
xgboost.cv = xgb.cv(param=param, data = xgb.train.data, folds = cv, nrounds = 1500, early_stopping_rounds = 100, metrics='auc')
best_iteration = xgboost.cv$best_iteration
xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=best_iteration)
xgb.test.data = xgb.DMatrix(data.matrix(test[,-'left']), missing = NA)
xgb.preds = predict(xgb.model, xgb.test.data)
xgb.roc_obj <- roc(test[,left], xgb.preds)
cat("Tree AUC ", auc(tree.roc_obj))
cat("XGB AUC ", auc(xgb.roc_obj))

#### Xgb importance
col_names = attr(xgb.train.data, ".Dimnames")[[2]]
imp = xgb.importance(col_names, xgb.model)
xgb.plot.importance(imp)

#### THE XGBoost Explainer
#library(devtools) 
#install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboostExplainer)

explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", base_score = 0.5, n_first_tree = xgb.model$best_ntreelimit - 1)
pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(xgb.preds-pred.xgb),'\n')
idx_to_get = as.integer(802)
test[idx_to_get,-"left"]
showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[,-'left']) ,idx_to_get, type = "binary")

####### IMPACT AGAINST VARIABLE VALUE
plot(test[,satisfaction_level], pred.breakdown[,satisfaction_level], cex=0.4, pch=16, xlab = "Satisfaction Level", ylab = "Satisfaction Level impact on log-odds")
plot(test[,last_evaluation], pred.breakdown[,last_evaluation], cex=0.4, pch=16, xlab = "Last evaluation", ylab = "Last evaluation impact on log-odds")
cr <- colorRamp(c("blue", "red"))
plot(test[,last_evaluation], pred.breakdown[,last_evaluation], col = rgb(cr(round(test[,satisfaction_level])), max=255), cex=0.4, pch=16, xlab = "Last evaluation", ylab = "Last evaluation impact on log-odds")





