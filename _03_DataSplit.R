##################################dat_delete####################################################################
#### dat_delete
dat_d <- dat_delete
rm(dat_delete)
round(prop.table(table(dat_d$hypothyroid)),4) #8.75%


#
grp <- sample(1:5,nrow(dat_d),replace = TRUE)
dat_dg <- cbind(dat_d,grp)
train <- select(filter(dat_dg,grp!=1),-grp)
test <- select(filter(dat_dg,grp==1),-grp)



##################################dat_imp_clean####################################################################
#### dat_imp_clean
dat_ic <- dat_imp_clean
rm(dat_imp)
rm(dat_imp_clean)
rm(dat_orginal)
round(prop.table(table(dat_ic$hypothyroid)),4) #7.87%

