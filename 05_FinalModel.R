dat_orginal <- dat <- read.csv("Data/hypothyroid_1220.csv")

# dat$hypothyroid <- factor(dat$hypothyroid, levels=c(0,1), labels=c("FALSE","TRUE"))
dat$hypothyroid <- factor(dat$hypothyroid)
dat$sex <- factor(dat$sex)
dat$on_thyroxine <- factor(dat$on_thyroxine)
dat$on_antithyroid_medication <- factor(dat$on_antithyroid_medication)
dat$thyroid_surgery <- factor(dat$thyroid_surgery)
dat$query_TRUE <- factor(dat$query_TRUE)
dat$query_hyperthyroid <- factor(dat$query_hyperthyroid)
dat$pregnant <- factor(dat$pregnant)
dat$sick <- factor(dat$sick)
dat$tumor <- factor(dat$tumor)
dat$goitre <- factor(dat$goitre)
dat$TSH_measured <- factor(dat$TSH_measured)
dat$T3_measured <- factor(dat$T3_measured)
dat$TT4_measured <- factor(dat$TT4_measured)
dat$T4U_measured <- factor(dat$T4U_measured)
dat$FTI_measured <- factor(dat$FTI_measured)
dat$TBG_measured <- factor(dat$TBG_measured)



# names(dat)
library(VIM)
marginmatrix(dat[,c("hypothyroid","TBG","FTI","T4U","TT4","T3","TSH","age")])

summary(dat)
library(missForest)
imp <- missForest(dat)
imp$OOBerror
dat_imp <- imp$ximp
write.csv(dat_imp,"Data/hypothyroid_rfimp.csv")


library(randomForest)
library(ROSE)
library(ROCR)
library(DMwR)

tt <- sample(2,nrow(dat_imp),replace=TRUE,prob=c(0.7,0.3))
rf <- randomForest(hypothyroid ~.,dat_imp[tt==1,],ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE,classwt=c(0.05,0.95))
rf.pred <- predict(rf,dat_imp[tt==1,])
table(observed=dat_imp[tt==1,"hypothyroid"],predicted=rf.pred)
rf.pred1 <- predict(rf,dat_imp[tt==2,])
table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)


dat_imp_rus <- ovun.sample(hypothyroid~.,data =dat_imp[tt==1,],method = "under")$data

rf <- randomForest(hypothyroid ~.,dat_imp_rus,ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
rf.pred <- predict(rf,dat_imp_rus)
table(observed=dat_imp_rus[,"hypothyroid"],predicted=rf.pred)
rf.pred1 <- predict(rf,dat_imp[tt==2,])
table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)

dat_imp_smote <- SMOTE(hypothyroid~.,data = dat_imp[tt==1,])
rf <- randomForest(hypothyroid ~.,dat_imp_smote,ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
rf.pred <- predict(rf,dat_imp_smote)
table(observed=dat_imp_smote[,"hypothyroid"],predicted=rf.pred)
rf.pred1 <- predict(rf,dat_imp[tt==2,])
table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)





require(randomForest, quietly=TRUE)
library(doMC)
library(foreach)
core <- detectCores()
registerDoMC(core)
rf <- foreach(ntree=rep(ceiling(500/core), core), .combine=combine, .packages='randomForest') %dopar%
  randomForest(hypothyroid ~ .,data=dat_imp[tt==1,], importance=TRUE, na.action=na.roughfix, ntree=ntree)
rn <- round(importance(dats$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]



# rf1 <- randomForest(hypothyroid ~.,dat[tt==1,-24],ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE,na.action = na.omit)
# rf1.pred <- predict(rf1,dat[tt==1,-24])
# table(observed=dat[tt==1,"hypothyroid"],predicted=rf1.pred)
# library(party)
# 
# rf2 <- cforest(hypothyroid ~ ., data=dat_imp[tt==1,], controls=cforest_unbiased())
# table(predict(rf2), dat_imp[tt==1,"hypothyroid"])


plot(rf)
print(rf)
varImpPlot(rf)
print(rf)
rf.pred <- predict(rf,dat_imp[tt==1,])
plot(margin(rf, tat[tt==2,"hypothyroid"]))
