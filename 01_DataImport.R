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

###
# 刪除沒有一個有效化驗值的樣本（3163->3162）
tmp <- filter(dat,!((dat$TSH<=0 | is.na(dat$TSH)) & (dat$T3<=0 | is.na(dat$T3)) & (dat$TT4<=0 | is.na(dat$TT4)) & 
                      (dat$T4U<=0 | is.na(dat$T4U)) & (dat$FTI<=0 | is.na(dat$FTI)) & (dat$TBG<=0 | is.na(dat$TBG))))
##刪除測量值為空的樣本 不符合常理的觀測值
# 刪除測量T3但是值不大於0或為空的樣本(3162->3161)
tmp <- filter(tmp,!(tmp$T3_measured==1 & (tmp$T3<=0 | is.na(tmp$T3))))
# 刪除測量T4U但是值不大於0或為空的樣本(3161->3159)
tmp <- filter(tmp,!(tmp$T4U_measured==1 & (tmp$T4U<=0 | is.na(tmp$T4U))))
# 刪除測量FTI但是值不大於0或為空的樣本(3159->3151)
tmp <- filter(tmp,!(tmp$FTI_measured==1 & (tmp$FTI<=0 | is.na(tmp$FTI))))
# 刪除測量TSH但是值不大於0或為空的樣本(3151->2258)
tmp <- filter(tmp,!(tmp$TSH_measured==1 & (tmp$TSH<=0 | is.na(tmp$TSH))))


table(dat$hypothyroid)
# 0    1 
# 3012  151 
table(tmp$hypothyroid)
# 0    1 
# 2114  144 

# 刪除TBG測量值
# dat <- select(tmp,-TBG)
# rm(tmp)
# dat_clean <- dat
dat_clean <- tmp


####剔除"TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured" 超過三個為空的樣本（不利插補）
table(dat[,c("hypothyroid","TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured")])
# T3_measured = 0, TT4_measured = 0, T4U_measured = 0, FTI_measured = 0 TSH_measured = 0 (2258->2013)
tmp <- filter(dat,!(T3_measured == 0 & TT4_measured == 0 & T4U_measured == 0 & FTI_measured == 0 & TSH_measured == 0))
# T3_measured = 0, TT4_measured = 0, T4U_measured = 0  TSH_measured＝0 (2013->2012)
tmp <- filter(tmp,!(T3_measured == 0 & TT4_measured == 0 & T4U_measured == 0  & TSH_measured==0))
# T3_measured = 0, TT4_measured = 0  TSH_measured (2012->2011)
tmp <- filter(tmp,!(T3_measured == 0 & TT4_measured == 0 & TSH_measured==0))

dat <- tmp
table(dat$hypothyroid)
# 0    1 
# 1867  144 
rm(tmp)
table(dat_orginal$hypothyroid)

# library(dplyr)
# pr <- function(x){
#   (round(prop.table(table(x))*100,2))
# }
# 
# 
# num <- function(x){
#     
# }
# 
# pr(na.omit(dat$sex))
# pr(na.omit(filter(dat,hypothyroid==FALSE)$sex))
# pr(na.omit(filter(dat,hypothyroid==TRUE)$sex))
# chisq.test(cbind(table(na.omit(filter(dat,hypothyroid==FALSE)$sex)),table(na.omit(filter(dat,hypothyroid==TRUE)$sex))))
# 
# pr(na.omit(dat$query_on_thyroxine))
# pr(na.omit(filter(dat,hypothyroid==FALSE)$query_on_thyroxine))
# pr(na.omit(filter(dat,hypothyroid==TRUE)$query_on_thyroxine))
# chisq.test(cbind(table(na.omit(filter(dat,hypothyroid==FALSE)$query_on_thyroxine)),
#                  table(na.omit(filter(dat,hypothyroid==TRUE)$query_on_thyroxine))))


library(dplyr) # Provides select().
library(ggplot2) # Provides ggplot(), aes(), geom_density(), xlab(), ggtitle(), labs().
library(gridExtra) # Provides grid.arrange().

# Generate the plot.

p01 <- dat_imp %>%
  with(dat_imp) %>%
  select(age, hypothyroid) %>%
  ggplot(aes(x=age)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of age by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p02 <- dat_imp %>%
  with(dat_imp) %>%
  select(TSH, hypothyroid) %>%
  ggplot(aes(x=TSH)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of TSH by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p03 <- dat_imp %>%
  with(dat_imp) %>%
  select(T3, hypothyroid) %>%
  ggplot(aes(x=T3)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of T3 by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p04 <- dat_imp %>%
  with(dat_imp) %>%
  select(TT4, hypothyroid) %>%
  ggplot(aes(TT4)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of TT4 by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p05 <- dat_imp %>%
  with(dat_imp) %>%
  select(T4U, hypothyroid) %>%
  ggplot(aes(T4U)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of T4U by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p06 <- dat_imp %>%
  with(dat_imp) %>%
  select(FTI, hypothyroid) %>%
  ggplot(aes(FTI)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of FTI by hypothyroid") +
  labs(fill="hypothyroid", y="Density")
p07 <- dat_imp %>%
  with(dat_imp) %>%
  select(TBG, hypothyroid) %>%
  ggplot(aes(TBG)) +
  geom_density(lty=3) +
  geom_density(aes(fill=hypothyroid, colour=hypothyroid), alpha=0.55) +
  ggtitle("Distribution of TBG by hypothyroid") +
  labs(fill="hypothyroid", y="Density")





grid.arrange(p01,p02,p03,p04,p05,p06,p07,ncol=2)

