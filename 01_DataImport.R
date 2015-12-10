dat <- read.csv("Data/hypothyroid_1109.csv")

library(Hmisc)
describe(dat)

library(mice)
md.pattern(dat)
# md.pairs(dat)
flux(dat)
fluxplot(dat)

names(dat)
library(VIM)
aggr_plot <- aggr(dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(dat[,c("hypothyroid","TBG")])
marginplot(dat[,c("hypothyroid","FTI")])
marginplot(dat[,c("hypothyroid","T4U")])
marginplot(dat[,c("hypothyroid","TT4")])
marginplot(dat[,c("hypothyroid","T3")])
marginplot(dat[,c("hypothyroid","TSH")])
marginplot(dat[,c("hypothyroid","age")])

