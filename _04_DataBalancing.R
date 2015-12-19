######Logistic 非平衡數據敏感性分析
#Imbalanced
#Random Over-Sampling(ROS)
library(ROSE)
#Random Under-Sampling(RUS)
#One-Sided Selection(OSS)
library(unbalanced)
#Synthetic Minority Over-Sampling Technique(SMOTE)
##################################dat_clean####################################################################
#### dat_clean
dat_c <- dat_clean
rm(dat_clean)
round(prop.table(table(dat_c$hypothyroid)),4) #6.38%

##ROS
dat_c_ros <- ROSE(hypothyroid~.,data = dat_c)$data
round(prop.table(table(dat_c_ros$hypothyroid)),4) #49.81%
##RUS
##OSS
##SMOTE

##################################dat_delete####################################################################
#### dat_delete
round(prop.table(table(dat_d$hypothyroid)),4) #8.75%

##ROS
dat_d_ros <- ROSE(hypothyroid~.,data = dat_d)$data
round(prop.table(table(dat_d_ros$hypothyroid)),4) #50.63%
##RUS
dat_d_rus <- ovun.sample(hypothyroid~.,data = dat_d,method = "under")$data
round(prop.table(table(dat_d_rus$hypothyroid)),4) #50.86%
##OSS
tmpx <- dat_d[,-1]
tmpy <- dat_d$hypothyroid
tmp <- ubOSS(X=tmpx,Y=tmpy)
##SMOTE

##################################dat_imp_clean####################################################################
#### dat_imp_clean
round(prop.table(table(dat_ic$hypothyroid)),4) #6.86%

##ROS
dat_ic_ros <- ROSE(hypothyroid~.,data = dat_ic)$data
round(prop.table(table(dat_ic_ros$hypothyroid)),4) #49.36%
##RUS
dat_ic_rus <- ovun.sample(hypothyroid~.,data = dat_ic,method = "under")$data
round(prop.table(table(dat_ic_rus$hypothyroid)),4) #48.48%
##OSS
##SMOTE