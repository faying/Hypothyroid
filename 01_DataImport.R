####Import Data
dat <- read.csv("Data/hypothyroid_1109.csv")

####Data Summary
library(Hmisc)
describe(dat)
sum(complete.cases(dat))

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
aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
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