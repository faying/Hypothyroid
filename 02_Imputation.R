####
dat_old <- dat
dat <- dat[,-26]
####Iterative robust model-based imputation (IRMI)
dat_irmi <- irmi(dat)

####bootstrap EM algorithm
library(Amelia)
dat_amelia <- amelia(x=dat[,c("hypothyroid","age","sex","TT4")],cs=c("hypothyroid","sex")) 

# ####
# library(mvnmle)
# mlest(dat)
# 
# ####
# library(rrcovNA)
# dat_rrcov <- impSeq(dat)

####Multivariate Imputation by Chained Equations
mid_mice <- mice(dat)
mid_mice$imp$age
mid_mice_2 <- complete(mid_mice,'long',inc=TRUE)
col <- rep(c("blue", "red")[1+as.numeric(is.na(mid_mice$data$age))],6)
stripplot(age~.imp, data=mid_mice_2, jit=TRUE,col=col, xlab="imputation Number")

library(mi)
mdf <- missing_data.frame(dat)
show(mdf)
summary(mdf)
image(mdf)
hist(mdf)
options(mc.cores=8)
imp <- mi(mdf,n.iter = 30, n.chains = 4, max.minutes = 20)
