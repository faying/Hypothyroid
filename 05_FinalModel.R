dat_orginal <- dat <- read.csv("Data/hypothyroid_1220.csv",na.strings = "")

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

library(dplyr)
library(randomForest)
library(ROSE)
library(ROCR)
library(DMwR)
library(doMC)
library(foreach)
library(ggplot2)



tt <- sample(2,nrow(dat_imp),replace=TRUE,prob=c(0.9,0.1))
# core <- detectCores()
# registerDoMC(core)
# rf <- foreach(ntree=rep(ceiling(800/core), core), .combine=combine, .packages='randomForest') %dopar%
#   randomForest(hypothyroid ~ .,data=dat_imp[tt==1,],nPerm=10,mtry=17,proximity=TRUE,importance=TRUE, ntree=ntree)
rf <- randomForest(hypothyroid ~.,dat_imp[tt==1,],ntree=1000,nPerm=10,mtry=17,proximity=TRUE,importance=TRUE)
print(rf)
rn <- round(importance(rf), 2)
rn[order(rn[,3], decreasing=TRUE),]
rf.pred <- predict(rf,dat_imp[tt==1,])
table(observed=dat_imp[tt==1,"hypothyroid"],predicted=rf.pred)
rf.pred1 <- predict(rf,dat_imp[tt==2,])
table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)


#Variable Important
varImpPlot(rf,main="")
title(main="Variable Importance Random Forest")
#OOB
plot(rf, col=c(1,2,4),main="")
legend("topright", c("OOB", "FALSE", "TRUE"), text.col=c(1,2,4), lty=1:3, col=c(1,2,4))
title(main="Out of Bag Error Rates Random Forest")
#OOB ROC
library(verification)
aucc <- verification::roc.area(as.integer(as.factor(dat_imp[tt==1,"hypothyroid"]))-1,rf$votes[,2])$A
verification::roc.plot(as.integer(as.factor(dat_imp[tt==1,"hypothyroid"]))-1,rf$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc))
title(main="OOB ROC Curve Random Forest")

source("pcme.R")


per <- pcme(dat_imp[tt==2,"hypothyroid"],rf.pred1)
round(per, 2)

# Calculate the overall error percentage.
cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
# Calculate the averaged class error percentage.
cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Risk Chart: requires the ggplot2 package.
# # Generate a risk chart.
# rf.pr <- predict(rf, newdata=dat_imp[tt==2,], type="prob")[,2]
# eval <- evaluateRisk(rf.pr, dat_imp[tt==2,"hypothyroid"])
# # rf.risk <- riskchart(rf.pr,dat_imp[tt==2,"hypothyroid"], 
#                 title="Performance Chart Random Forest", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE)

rf.pre <- prediction(rf.pr, dat_imp[tt==2,"hypothyroid"])
rf.per <- performance(rf.pre, "lift", "rpp")
rf.per@x.values[[1]] <- rf.per@x.values[[1]]*100
lift.rf <- data.frame(caseload=unlist(rf.per@x.values), lift=unlist(rf.per@y.values))
# Plot the lift chart.
ROCR::plot(rf.per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)
legend("topright", c("RandomForest"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))
title(main="Lift Chart of Random Forest")

# ROC Curve: requires the ROCR package.

# Generate an ROC Curve for the rf model on dat_imp [validate].

rf.pr <- predict(rf, newdata=dat_imp[tt==2,], type="prob")[,2]
rf.pre <- prediction(rf.pr, dat_imp[tt==2,"hypothyroid"])
pe <- performance(rf.pre, "tpr", "fpr")
au <- performance(rf.pre, "auc")@y.values[[1]]

pd <- data.frame(cut=pe@alpha.values[[1]],fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
best_cut <- pd[which.max(pd$tpr-pd$fpr+1),]

p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=4,
                  label=paste("AUC =", round(au, 2),"    Best Cut-off =",best_cut$cut))

print(p)

plot(margin(rf, dat[tt==2,"hypothyroid"]))










# 
# rf <- randomForest(hypothyroid ~.,dat_imp[tt==1,],ntree=500,nPerm=10,mtry=17,proximity=TRUE,importance=TRUE)
# rn <- round(importance(rf), 2)
# rn[order(rn[,3], decreasing=TRUE),]


# dat_imp_rus <- ovun.sample(hypothyroid~.,data =dat_imp[tt==1,],method = "under")$data
# rf <- randomForest(hypothyroid ~.,dat_imp_rus,ntree=500,nPerm=10,mtry=17,proximity=TRUE,importance=TRUE)
# rf.pred <- predict(rf,dat_imp_rus)
# table(observed=dat_imp_rus[,"hypothyroid"],predicted=rf.pred)
# rf.pred1 <- predict(rf,dat_imp[tt==2,])
# table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)

# dat_imp_smote <- SMOTE(hypothyroid~.,data = dat_imp[tt==1,])
# rf <- randomForest(hypothyroid ~.,dat_imp_smote,ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
# rf.pred <- predict(rf,dat_imp_smote)
# table(observed=dat_imp_smote[,"hypothyroid"],predicted=rf.pred)
# rf.pred1 <- predict(rf,dat_imp[tt==2,])
# table(observed=dat_imp[tt==2,"hypothyroid"],predicted=rf.pred1)
# 
# library(ada)
# ada(hypothyroid ~ ., data = dat_imp[tt==1,], control = rpart::rpart.control(maxdepth = 30, cp = 0.01, minsplit = 20, xval = 10), iter = 100)






# rf1 <- randomForest(hypothyroid ~.,dat[tt==1,-24],ntree=500,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE,na.action = na.omit)
# rf1.pred <- predict(rf1,dat[tt==1,-24])
# table(observed=dat[tt==1,"hypothyroid"],predicted=rf1.pred)
# library(party)
# 
# rf2 <- cforest(hypothyroid ~ ., data=dat_imp[tt==1,], controls=cforest_unbiased())
# table(predict(rf2), dat_imp[tt==1,"hypothyroid"])