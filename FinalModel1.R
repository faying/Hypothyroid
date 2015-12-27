tt1 <- sample(2,nrow(dat_imp),replace=TRUE,prob=c(0.9,0.1))
rf1 <- randomForest(hypothyroid ~TSH+FTI+TBG+TT4+age,dat_imp[tt1==1,],ntree=500,nPerm=10,mtry=4,proximity=TRUE,importance=TRUE)
print(rf1)
rn1 <- round(importance(rf1), 2)
rn1[order(rn1[,3], decreasing=TRUE),]
rf1.pred <- predict(rf1,dat_imp[tt1==1,])
table(observed=dat_imp[tt1==1,"hypothyroid"],predicted=rf1.pred)
rf1.pred1 <- predict(rf1,dat_imp[tt1==2,])
table(observed=dat_imp[tt1==2,"hypothyroid"],predicted=rf1.pred1)

#Variable Important
varImpPlot(rf1,main="")
title(main="Variable Importance Random Forest")
#OOB
plot(rf1, col=c(1,2,4),main="")
legend("topright", c("OOB", "FALSE", "TRUE"), text.col=c(1,2,4), lty=1:3, col=c(1,2,4))
title(main="Out of Bag Error Rates Random Forest")
#OOB ROC
library(verification)
aucc1 <- verification::roc.area(as.integer(as.factor(dat_imp[tt1==1,"hypothyroid"]))-1,rf1$votes[,2])$A
verification::roc.plot(as.integer(as.factor(dat_imp[tt1==1,"hypothyroid"]))-1,rf1$votes[,2], main="")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc1))
title(main="OOB ROC Curve Random Forest")

source("pcme.R")


per1 <- pcme(dat_imp[tt1==2,"hypothyroid"],rf1.pred1)
round(per1, 2)

# Calculate the overall error percentage.
cat(100*round(1-sum(diag(per1), na.rm=TRUE), 2))
# Calculate the averaged class error percentage.
cat(100*round(mean(per1[,"Error"], na.rm=TRUE), 2))

# Risk Chart: requires the ggplot2 package.
# # Generate a risk chart.
rf1.pr <- predict(rf1, newdata=dat_imp[tt1==2,], type="prob")[,2]
# eval <- evaluateRisk(rf.pr, dat_imp[tt==2,"hypothyroid"])
# # rf.risk <- riskchart(rf.pr,dat_imp[tt==2,"hypothyroid"], 
#                 title="Performance Chart Random Forest", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE)

rf1.pre <- prediction(rf1.pr, dat_imp[tt1==2,"hypothyroid"])
rf1.per <- performance(rf1.pre, "lift", "rpp")
rf1.per@x.values[[1]] <- rf1.per@x.values[[1]]*100
lift1.rf <- data.frame(caseload=unlist(rf1.per@x.values), lift=unlist(rf1.per@y.values))
# Plot the lift chart.
ROCR::plot(rf1.per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)
legend("topright", c("RandomForest"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))
title(main="Lift Chart of Random Forest")

# ROC Curve: requires the ROCR package.

# Generate an ROC Curve for the rf model on dat_imp [validate].

rf1.pr <- predict(rf1, newdata=dat_imp[tt1==2,], type="prob")[,2]
rf1.pre <- prediction(rf1.pr, dat_imp[tt1==2,"hypothyroid"])
pe1 <- performance(rf1.pre, "tpr", "fpr")
au1 <- performance(rf1.pre, "auc")@y.values[[1]]

pd1 <- data.frame(cut=pe1@alpha.values[[1]],fpr=unlist(pe1@x.values), tpr=unlist(pe1@y.values))
best_cut1 <- pd1[which.max(pd1$tpr-pd1$fpr+1),]

p <- ggplot(pd1, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=4,
                  label=paste("AUC =", round(au1, 2),"    Best Cut-off =",best_cut1$cut))

print(p)

plot(margin(rf1, dat[tt1==2,"hypothyroid"]))


