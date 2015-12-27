datl <- read.csv("Data/hypothyroid_1212.csv",na.strings = "")
datl$hypothyroid <- factor(datl$hypothyroid)
datl$sex <- factor(datl$sex)
datl$on_thyroxine <- factor(datl$on_thyroxine)
datl$on_antithyroid_medication <- factor(datl$on_antithyroid_medication)
datl$thyroid_surgery <- factor(datl$thyroid_surgery)
datl$query_TRUE <- factor(datl$query_TRUE)
datl$query_hyperthyroid <- factor(datl$query_hyperthyroid)
datl$pregnant <- factor(datl$pregnant)
datl$sick <- factor(datl$sick)
datl$tumor <- factor(datl$tumor)
datl$goitre <- factor(datl$goitre)
datl$TSH_measured <- factor(datl$TSH_measured)
datl$T3_measured <- factor(datl$T3_measured)
datl$TT4_measured <- factor(datl$TT4_measured)
datl$T4U_measured <- factor(datl$T4U_measured)
datl$FTI_measured <- factor(datl$FTI_measured)
datl$TBG_measured <- factor(datl$TBG_measured)

library(missForest)
impl <- missForest(datl)
impl$OOBerror
dat_impl <- impl$ximp
summary(dat_impl)

write.csv(dat_impl,"Data/hypothyroid_1226.csv")
dat_impl1 <- read.csv("Data/hypothyroid_1226.csv")
dat_impl1 <- dat_impl1[,-1]


library(corrplot)
corrplot(cor(dat_impl1[,-1]),order="hclust",method="color",outline = T,hclust.method = "ward",family="SimHei")


##Clust
library(ClustOfVar)
tree <- hclustvar(X.quanti = dat[,-1])
plot(tree)
stab <- stability(tree,B=2)
which.max(stab$meanCR)
g_init <- cutreevar(tree,19)$cluster
cluster <- kmeansvar(X.quanti = dat[,-1],init = g_init, matsim = TRUE)
summary(cluster)
group <- as.integer(cluster$cluster)

##Regression
library(glmnet)
library(ncvreg)
library(grpreg)

trainx <- dat_impl1[tt==1,-1]
trainy <- dat_impl1[tt==1,"hypothyroid"]
testx <- dat_impl1[tt==2,-1]
testy <- dat_impl1[tt==2,"hypothyroid"]


log <- glm(hypothyroid~.,data = dat_impl[tt==1,],family = "binomial")

summary(log)
logit <- log$coefficients
names(dat_impl1[,-1])

cv_l$glmnet.fit
cv_l <- cv.glmnet(as.matrix(trainx),as.factor(trainy),family="binomial",type.measure = "class",alpha=1)
lasso <- coef(cv_l, s = "lambda.min")
plot(cv_l$glmnet.fit, label=T,col=1:25)
legend("topright", paste(1:25,names(dat_impl1[,-1])))
plot(cv_l)
#glm_l <- glmnet(xx,y,family="binomial",lambda=cv_l$lambda.min)
#glm_l$call
#coef(glm_l)

##elastic net
#cv_en <- cv.glmnet(as.matrix(trainx),as.factor(trainy),family="binomial",type.measure = "class",alpha=0.5)
#coef(cv_en, s = "lambda.min")
#cv_en$lambda.min
#plot(cv_en$glmnet.fit, "lambda", label=T)
glm_en <- glmnet(as.matrix(trainx),as.factor(trainy),family="binomial",lambda=0.001,alpha=0.5)
en <- coef(glm_en)

##group mcp
grpm <- grpreg(as.matrix(trainx),trainy, group, family="binomial",penalty = "grMCP",lambda.min = .05)
gmcp <- select(grpm,"BIC")$beta


##group bridge
grpb <- gBridge(as.matrix(trainx),trainy, group, family="binomial")
gb <- select(grpb,"BIC")$beta
#plot(grpb,label=T)
