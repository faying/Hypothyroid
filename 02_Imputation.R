####Iterative robust model-based imputation (IRMI)
# library(VIM)
# dat_irmi <- irmi(dat)
####bootstrap EM algorithm
# library(Amelia)
# AmeliaView()
# dat_amelia <- amelia(x=dat) 
# ####
# library(mvnmle)
# mlest(dat)
# 
# ####
# library(rrcovNA)
# dat_rrcov <- impSeq(dat)
# 
# ####Multivariate Imputation by Chained Equations
# library(mice)
# mid_mice <- mice(dat)
# mid_mice$imp$age
# dat_mice <- complete(mid_mice)
# summary(dat_mice)
# dat_mice_clean <- dc(dat_mice)
dc <- function(x){
  require(dplyr,quietly = TRUE)
  # 刪除沒有一個有效化驗值的樣本（3163->3162）
  tmp <- filter(x,!(is.na(x$TSH) & is.na(x$T3) & is.na(x$TT4) & 
                      is.na(x$T4U) & is.na(x$FTI)))
  # 刪除T3值不大於0或為空的樣本(3162->3161)
  tmp <- filter(tmp,!tmp$T3<=0)
  # 刪TT4值不大於0或為空的樣本(3161->3159)
  tmp <- filter(tmp,!tmp$TT4<=0)
  # 刪除T4U值不大於0或為空的樣本(3161->3159)
  tmp <- filter(tmp,!tmp$T4U<=0)
  # 刪除測量FTI但是值不大於0或為空的樣本(3159->3151)
  tmp <- filter(tmp,!tmp$FTI<=0)
  # 刪除TSH值不大於0或為空的樣本(3151->2258)
  tmp <- filter(tmp,!tmp$TSH<=0)
  # 刪除TSH值不大於0或為空的樣本(3151->2258)
  tmp <- filter(tmp,!tmp$age<=0)
}
####MCMC
library(VIM)
aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

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
# mean_age mean_sex mean_TSH  mean_T3   sd_age   sd_sex   sd_TSH    sd_T3 
# 1.116698 1.103819 1.823643 2.111727 1.182499 1.105104 1.212727 1.126656 
imp <- mi(imp, n.iter = 5)
plot(imp)
image(imp)
summary(imp)
# names(dat)
analysis <- pool(hypothyroid ~ age + sex + on_thyroxine + query_on_thyroxine + on_antithyroid_medication + thyroid_surgery + 
                   query_TRUE + query_hyperthyroid + pregnant + sick + tumor + lithium + goitre + TSH_measured + 
                   TSH + T3_measured + T3 + TT4_measured + TT4 + T4U_measured + T4U + FTI_measured + FTI +
                   TBG_measured, data = imp, m = 5)
display(analysis)
mid_imp <- complete(imp,1)
dat_imp <- select(mid_imp,-missing_age,-missing_sex,-missing_TSH,-missing_T3)
#刪除插補后小於0的數值型數據（2011->1805）
dat_imp_clean <- dc(dat_imp)

####
summary(dat)
summary(dat_imp)
summary(dat_imp_clean)
table(dat_imp_clean$hypothyroid)
# 0    1 
# 1663  142 


####
ls()
rm(mdf)
rm(mid_imp)
rm(tmp)
rm(aggr_plot)
rm(analysis)
rm(imp)

####Delete
dat_delete <- na.omit(dat_clean)
table(dat_delete$hypothyroid)

# 插補＋清洗
write.csv(dat_imp_clean,"Data/Imputation/hypothyroid_imp.csv")
# 直接刪除空值
write.csv(dat_delete,"Data/Imputation/hypothyroid_delete.csv")
# 只清洗數據
write.csv(dat_clean,"Data/Imputation/hypothyroid_clean.csv")
