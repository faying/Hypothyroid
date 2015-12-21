##################################dat_delete####################################################################
source(file="pcme.R")
####dat_imp_clean
dat_ic <- dat_imp_clean
dat_ic1 <- select(read.csv("Data/Imputation/hypothyroid_imp.csv",header = T),-X)
summary(dat_ic)
dat_ic <- select(dat_ic,-TT4_measured,-T4U_measured,-FTI_measured)
dat_ic1 <- select(dat_ic1,-TT4_measured,-T4U_measured,-FTI_measured)
corrplot(cor(dat_ic1[,-1]),order="hclust",method="color",outline = T)
round(prop.table(table(dat_ic$hypothyroid)),4) #7.87%
######Logistic 非平衡數據敏感性分析
#Imbalanced
#Random Over-Sampling(ROS)
library(ROSE)
#Random Under-Sampling(RUS)
#One-Sided Selection(OSS)
library(unbalanced)
#Synthetic Minority Over-Sampling Technique(SMOTE)
library(DMwR)
##Lasso
library(glmnet)
library(randomForest)
library(ROCR)
####Cross Validation_1#######################################
###修改群組1──5 實現5折交叉驗證
grp_ic <- sample(1:5,nrow(dat_ic),replace = TRUE)
dat_icg <- cbind(dat_ic,grp_ic)
dat_icg1 <- cbind(dat_ic1,grp_ic)
train <- select(filter(dat_icg,grp_ic!=5),-grp_ic)
train1 <- select(filter(dat_icg1,grp_ic!=5),-grp_ic)
test <- select(filter(dat_icg,grp_ic==5),-grp_ic)
# test1 <- select(filter(dat_dg1,grp==1),-grp)

####對訓練集採用不同平衡方法
###不平衡數據##########################################################################################
##Train
#Lasso
mid_model <- cv.glmnet(as.matrix(select(train1,-hypothyroid)),as.factor(train1$hypothyroid),family="binomial",type.measure="deviance")
plot(mid_model)
lasso_orgi <- coef(mid_model, s = "lambda.min")
(newvar <- row.names(lasso_orgi)[as.numeric(lasso_orgi)>0][-1])
glm_orgi <- glm(hypothyroid~.,data = train[,c("hypothyroid",newvar)],family=binomial(link="logit"))
summary(glm_orgi)$coefficients[,-3]
#RF
rf_orgi <- randomForest(hypothyroid ~ .,data=train, importance=TRUE, ntree = 500)
rn <- round(importance(rf_orgi), 2)
rn[order(rn[,3], decreasing=TRUE),]
row.names(rn[order(rn[,3], decreasing=TRUE),])[1:10]
##Predict
#Lasso
pr_orgi_glm <- as.vector(ifelse(predict(glm_orgi, type="response", newdata=test) >= 0.5,1,0))
table(test$hypothyroid, pr_orgi_glm, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_orgi_glm)
proc_orgi_glm <- predict(glm_orgi, type="response", newdata=test)
pred_orgi_glm <- prediction(proc_orgi_glm,test$hypothyroid)
pe_orgi_glm <- performance(pred_orgi_glm, "tpr", "fpr")
au_orgi_glm <- performance(pred_orgi_glm, "auc")@y.values[[1]]
pd_orgi_glm <- data.frame(cut=pe_orgi_glm@alpha.values[[1]],fpr=unlist(pe_orgi_glm@x.values), tpr=unlist(pe_orgi_glm@y.values))
best_cut_orgi_glm <- pd_orgi_glm[which.max(pd_orgi_glm$tpr-pd_orgi_glm$fpr+1),]
(resu_orgi_glm <- cbind(au_orgi_glm,best_cut_orgi_glm))
#RF
pr_orgi_rf <- predict(rf_orgi, newdata=test)
table(test$hypothyroid, pr_orgi_rf, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_orgi_rf)
proc_orgi_rf <- predict(rf_orgi, newdata=test, type="prob")[,2]
pred_orgi_rf <- prediction(proc_orgi_rf, test$hypothyroid)
pe_orgi_rf <- performance(pred_orgi_rf, "tpr", "fpr")
au_orgi_rf <- performance(pred_orgi_rf, "auc")@y.values[[1]]
pd_orgi_rf <- data.frame(cut=pe_orgi_rf@alpha.values[[1]],fpr=unlist(pe_orgi_rf@x.values), tpr=unlist(pe_orgi_rf@y.values))
best_cut_orgi_rf <- pd_orgi_rf[which.max(pd_orgi_rf$tpr-pd_orgi_rf$fpr+1),]
(resu_orgi_rf <- cbind(au_orgi_rf,best_cut_orgi_rf))


###ROS##########################################################################################
dat_icg_train_ros <- ROSE(hypothyroid~.,data = train)$data
dat_icg_train_ros1 <- ROSE(hypothyroid~.,data = train1)$data
table(dat_icg_train_ros$hypothyroid)
##Train
#Lasso
mid_model <- cv.glmnet(as.matrix(select(dat_icg_train_ros1,-hypothyroid)),as.factor(dat_icg_train_ros1$hypothyroid),family="binomial",type.measure="deviance")
# plot(mid_model)
lasso_ros <- coef(mid_model, s = "lambda.min")
(newvar <- row.names(lasso_ros)[as.numeric(lasso_ros)>0][-1])
glm_ros <- glm(hypothyroid~.,data = dat_icg_train_ros[,c("hypothyroid",newvar)],family=binomial(link="logit"))
summary(glm_ros)$coefficients[,-3]
#RF
rf_ros <- randomForest(hypothyroid ~ .,data=dat_icg_train_ros, importance=TRUE, ntree = 500)
rn <- round(importance(rf_ros), 2)
rn[order(rn[,3], decreasing=TRUE),]
row.names(rn[order(rn[,3], decreasing=TRUE),])[1:10]
##Predict
#Lasso
pr_ros_glm <- as.vector(ifelse(predict(glm_ros, type="response", newdata=test) >= 0.5,1,0))
table(test$hypothyroid, pr_ros_glm, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_ros_glm)
proc_ros_glm <- predict(glm_ros, type="response", newdata=test)
pred_ros_glm <- prediction(proc_ros_glm,test$hypothyroid)
pe_ros_glm <- performance(pred_ros_glm, "tpr", "fpr")
au_ros_glm <- performance(pred_ros_glm, "auc")@y.values[[1]]
pd_ros_glm <- data.frame(cut=pe_ros_glm@alpha.values[[1]],fpr=unlist(pe_ros_glm@x.values), tpr=unlist(pe_ros_glm@y.values))
best_cut_ros_glm <- pd_ros_glm[which.max(pd_ros_glm$tpr-pd_ros_glm$fpr+1),]
(resu_ros_glm <- cbind(au_ros_glm,best_cut_ros_glm))
#RF
pr_ros_rf <- predict(rf_ros, newdata=test)
table(test$hypothyroid, pr_ros_rf, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_ros_rf)
proc_ros_rf <- predict(rf_ros, newdata=test, type="prob")[,2]
pred_ros_rf <- prediction(proc_ros_rf, test$hypothyroid)
pe_ros_rf <- performance(pred_ros_rf, "tpr", "fpr")
au_ros_rf <- performance(pred_ros_rf, "auc")@y.values[[1]]
pd_ros_rf <- data.frame(cut=pe_ros_rf@alpha.values[[1]],fpr=unlist(pe_ros_rf@x.values), tpr=unlist(pe_ros_rf@y.values))
best_cut_ros_rf <- pd_ros_rf[which.max(pd_ros_rf$tpr-pd_ros_rf$fpr+1),]
(resu_ros_rf <- cbind(au_ros_rf,best_cut_ros_rf))

###RUS##########################################################################################
dat_icg_train_rus <- ovun.sample(hypothyroid~.,data = train,method = "under")$data
dat_icg_train_rus1 <- ovun.sample(hypothyroid~.,data = train1,method = "under")$data
table(dat_icg_train_rus$hypothyroid)
##Train
#Lasso
mid_model <- cv.glmnet(as.matrix(select(dat_icg_train_rus1,-hypothyroid)),as.factor(dat_icg_train_rus1$hypothyroid),family="binomial",type.measure="deviance")
# plot(mid_model)
lasso_rus <- coef(mid_model, s = "lambda.min")
(newvar <- row.names(lasso_rus)[as.numeric(lasso_rus)>0][-1])
glm_rus <- glm(hypothyroid~.,data = dat_icg_train_rus[,c("hypothyroid",newvar)],family=binomial(link="logit"))
summary(glm_rus)$coefficients[,-3]
#RF
rf_rus <- randomForest(hypothyroid~ .,data=dat_icg_train_rus, importance=TRUE, ntree = 500)
rn <- round(importance(rf_rus), 2)
rn[order(rn[,3], decreasing=TRUE),]
row.names(rn[order(rn[,3], decreasing=TRUE),])[1:10]
##Predict
#Lasso
pr_rus_glm <- as.vector(ifelse(predict(glm_rus, type="response", newdata=test) >= 0.5,1,0))
table(test$hypothyroid, pr_rus_glm, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_rus_glm)
proc_rus_glm <- predict(glm_rus, type="response", newdata=test)
pred_rus_glm <- prediction(proc_rus_glm,test$hypothyroid)
pe_rus_glm <- performance(pred_rus_glm, "tpr", "fpr")
au_rus_glm <- performance(pred_rus_glm, "auc")@y.values[[1]]
pd_rus_glm <- data.frame(cut=pe_rus_glm@alpha.values[[1]],fpr=unlist(pe_rus_glm@x.values), tpr=unlist(pe_rus_glm@y.values))
best_cut_rus_glm <- pd_rus_glm[which.max(pd_rus_glm$tpr-pd_rus_glm$fpr+1),]
(resu_rus_glm <- cbind(au_rus_glm,best_cut_rus_glm))
#RF
pr_rus_rf <- predict(rf_rus, newdata=test)
table(test$hypothyroid, pr_rus_rf, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_rus_rf)
proc_rus_rf <- predict(rf_rus, newdata=test, type="prob")[,2]
pred_rus_rf <- prediction(proc_rus_rf, test$hypothyroid)
pe_rus_rf <- performance(pred_rus_rf, "tpr", "fpr")
au_rus_rf <- performance(pred_rus_rf, "auc")@y.values[[1]]
pd_rus_rf <- data.frame(cut=pe_rus_rf@alpha.values[[1]],fpr=unlist(pe_rus_rf@x.values), tpr=unlist(pe_rus_rf@y.values))
best_cut_rus_rf <- pd_rus_rf[which.max(pd_rus_rf$tpr-pd_rus_rf$fpr+1),]
(resu_rus_rf <- cbind(au_rus_rf,best_cut_rus_rf))


###SMOTE##########################################################################################
dat_icg_train_smote <- SMOTE(hypothyroid~.,data = train)
train2 <- train1
train2$hypothyroid <- as.factor(train2$hypothyroid)
dat_icg_train_smote1 <- SMOTE(hypothyroid~.,data = train2)
table(dat_icg_train_smote$hypothyroid)
##Train
#Lasso
mid_model <- cv.glmnet(as.matrix(select(dat_icg_train_smote1,-hypothyroid)),as.factor(dat_icg_train_smote1$hypothyroid),family="binomial",type.measure="deviance")
# plot(mid_model)
lasso_smote <- coef(mid_model, s = "lambda.min")
(newvar <- row.names(lasso_smote)[as.numeric(lasso_smote)>0][-1])
glm_smote <- glm(hypothyroid~.,data = dat_icg_train_smote[,c("hypothyroid",newvar)],family=binomial(link="logit"))
summary(glm_smote)$coefficients[,-3]
#RF
rf_smote <- randomForest(hypothyroid~ .,data=dat_icg_train_smote, importance=TRUE, ntree = 500)
rn <- round(importance(rf_smote), 2)
rn[order(rn[,3], decreasing=TRUE),]
row.names(rn[order(rn[,3], decreasing=TRUE),])[1:10]
##Predict
#Lasso
pr_smote_glm <- as.vector(ifelse(predict(glm_smote, type="response", newdata=test) >= 0.5,1,0))
table(test$hypothyroid, pr_smote_glm, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_smote_glm)
proc_smote_glm <- predict(glm_smote, type="response", newdata=test)
pred_smote_glm <- prediction(proc_smote_glm,test$hypothyroid)
pe_smote_glm <- performance(pred_smote_glm, "tpr", "fpr")
au_smote_glm <- performance(pred_smote_glm, "auc")@y.values[[1]]
pd_smote_glm <- data.frame(cut=pe_smote_glm@alpha.values[[1]],fpr=unlist(pe_smote_glm@x.values), tpr=unlist(pe_smote_glm@y.values))
best_cut_smote_glm <- pd_smote_glm[which.max(pd_smote_glm$tpr-pd_smote_glm$fpr+1),]
(resu_smote_glm <- cbind(au_smote_glm,best_cut_smote_glm))
#RF
pr_smote_rf <- predict(rf_smote, newdata=test)
table(test$hypothyroid, pr_smote_rf, dnn=c("Actual", "Predicted"))
pcme(test$hypothyroid,pr_smote_rf)
proc_smote_rf <- predict(rf_smote, newdata=test, type="prob")[,2]
pred_smote_rf <- prediction(proc_smote_rf, test$hypothyroid)
pe_smote_rf <- performance(pred_smote_rf, "tpr", "fpr")
au_smote_rf <- performance(pred_smote_rf, "auc")@y.values[[1]]
pd_smote_rf <- data.frame(cut=pe_smote_rf@alpha.values[[1]],fpr=unlist(pe_smote_rf@x.values), tpr=unlist(pe_smote_rf@y.values))
best_cut_smote_rf <- pd_smote_rf[which.max(pd_smote_rf$tpr-pd_smote_rf$fpr+1),]
(resu_smote_rf <- cbind(au_smote_rf,best_cut_smote_rf))


