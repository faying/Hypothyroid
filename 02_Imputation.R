####Iterative robust model-based imputation (IRMI)
library(VIM)
dat_irmi <- irmi(dat)

####bootstrap EM algorithm
library(Amelia)
AmeliaView()
dat_amelia <- amelia(x=dat) 

# ####
# library(mvnmle)
# mlest(dat)
# 
# ####
# library(rrcovNA)
# dat_rrcov <- impSeq(dat)

####Multivariate Imputation by Chained Equations
library(mice)
mid_mice <- mice(dat)
mid_mice$imp$age
dat_mice <- complete(mid_mice)

####MCMC
library(mi)
mdf <- missing_data.frame(dat)
show(mdf)
summary(mdf)
image(mdf)
hist(mdf)
options(mc.cores=8)
imp <- mi(mdf,n.chains=8)
show(imp)
round(mipply(imp, mean, to.matrix = TRUE), 3)
Rhats(imp)
imp <- mi(imp, n.iter = 5)
plot(imp)
image(imp)
summary(imp)
names(dat)
analysis <- pool(hypothyroid ~ age + sex + on_thyroxine + query_on_thyroxine + on_antithyroid_medication + thyroid_surgery + 
                   query_TRUE + query_hyperthyroid + pregnant + sick + tumor + lithium + goitre + TSH_measured + 
                   TSH + T3_measured + T3 + TT4_measured + TT4 + T4U_measured + T4U + FTI_measured + FTI +
                   TBG_measured, data = imp, m = 5)
display(analysis)
mid_imp <- complete(imp,1)
dat_imp <- select(mid_imp,-missing_age,-missing_sex,-missing_TSH,-missing_T3,-missing_TT4,-missing_T4U,-missing_FTI)


####
ls()
rm(mdf)
rm(mid_imp)
rm(tmp)
rm(aggr_plot)
rm(analysis)
rm(dat_amelia)
rm(i)
rm(imp)
rm(mid_mice)
