####Import Data
dat_orginal <- dat <- read.csv("Data/hypothyroid_1212.csv")

# dat$hypothyroid <- factor(dat$hypothyroid, levels=c(0,1), labels=c("FALSE","TRUE"))
dat$hypothyroid <- factor(dat$hypothyroid)
dat$sex <- factor(dat$sex)
dat$on_thyroxine <- factor(dat$on_thyroxine)
dat$query_on_thyroxine <- factor(dat$query_on_thyroxine)
dat$on_antithyroid_medication <- factor(dat$on_antithyroid_medication)
dat$thyroid_surgery <- factor(dat$thyroid_surgery)
dat$query_TRUE <- factor(dat$query_TRUE)
dat$query_hyperthyroid <- factor(dat$query_hyperthyroid)
dat$pregnant <- factor(dat$pregnant)
dat$sick <- factor(dat$sick)
dat$tumor <- factor(dat$tumor)
dat$lithium <- factor(dat$lithium)
dat$goitre <- factor(dat$goitre)
dat$TSH_measured <- factor(dat$TSH_measured)
dat$T3_measured <- factor(dat$T3_measured)
dat$TT4_measured <- factor(dat$TT4_measured)
dat$T4U_measured <- factor(dat$T4U_measured)
dat$FTI_measured <- factor(dat$FTI_measured)
dat$TBG_measured <- factor(dat$TBG_measured)

####Data Summary
library(Hmisc)
summary(dat)
describe(dat)

####Finding Missing Data
library(mice)
md.pattern(dat)
# md.pairs(dat)
flux(dat)
fluxplot(dat)
barMiss(dat[,c("hypothyroid","TBG")]) 
barMiss(dat[,c("hypothyroid","FTI")]) 
barMiss(dat[,c("hypothyroid","T4U")]) 
barMiss(dat[,c("hypothyroid","TT4")]) 
barMiss(dat[,c("hypothyroid","T3")]) 
barMiss(dat[,c("hypothyroid","TSH")]) 
barMiss(dat[,c("hypothyroid","age")]) 

histMiss(dat[,c("age","TBG")])
histMiss(dat[,c("age","FTI")])
histMiss(dat[,c("age","T4U")])
histMiss(dat[,c("age","TT4")])
histMiss(dat[,c("age","T3")])
histMiss(dat[,c("age","TSH")])

# names(dat)
library(VIM)
aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(dat[,c("hypothyroid","TBG")])
marginplot(dat[,c("hypothyroid","FTI")])
marginplot(dat[,c("hypothyroid","T4U")])
marginplot(dat[,c("hypothyroid","TT4")])
marginplot(dat[,c("hypothyroid","T3")])
marginplot(dat[,c("hypothyroid","TSH")])
marginplot(dat[,c("hypothyroid","age")])
matrixplot(dat)  
marginmatrix(dat[,c("hypothyroid","TBG","FTI","T4U","TT4","T3","TSH","age")])
pbox(dat, pos=2)


####Data Cleaning
library(dplyr)
# 刪除沒有一個有效化驗值的樣本（3163->3162）
tmp <- filter(dat,!((dat$TSH<=0 | is.na(dat$TSH)) & (dat$T3<=0 | is.na(dat$T3)) & (dat$TT4<=0 | is.na(dat$TT4)) & 
                      (dat$T4U<=0 | is.na(dat$T4U)) & (dat$FTI<=0 | is.na(dat$FTI)) & (dat$TBG<=0 | is.na(dat$TBG))))

# 刪除測量T3但是值不大於0或為空的樣本(3162->3161)
tmp <- filter(tmp,!(tmp$T3_measured==1 & (tmp$T3<=0 | is.na(tmp$T3))))
# 刪除測量T4U但是值不大於0或為空的樣本(3161->3159)
tmp <- filter(tmp,!(tmp$T4U_measured==1 & (tmp$T4U<=0 | is.na(tmp$T4U))))
# 刪除測量FTI但是值不大於0或為空的樣本(3159->3151)
tmp <- filter(tmp,!(tmp$FTI_measured==1 & (tmp$FTI<=0 | is.na(tmp$FTI))))
# 刪除測量TSH但是值不大於0或為空的樣本(3151->2258)
tmp <- filter(tmp,!(tmp$TSH_measured==1 & (tmp$TSH<=0 | is.na(tmp$TSH))))

table(dat$hypothyroid)
table(tmp$hypothyroid)
# 刪除TBG測量值
dat <- select(tmp,-TBG)

rm(dat_1)
