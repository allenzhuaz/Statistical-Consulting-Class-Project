cardiac <- read.csv("C:/njit/664/project/cardiac.csv",header=T,sep=',')
attach(cardiac)

z.test2sam = function(pkhr, maxhr, var.pkhr, var.maxhr) {
  n.pkhr = length(pkhr)
  n.maxhr = length(maxhr)
  zeta = (mean(pkhr) - mean(maxhr))/(sqrt(var.pkhr/n.pkhr + var.maxhr/n.maxhr))
  return(zeta)
}
var(pkhr)
var(maxhr)

zstar = z.test2sam(pkhr, maxhr, var(pkhr), var(maxhr))
zstar

z.test2sam = function(sbp, mbp, var.sbp, var.mbp) {
  n.sbp = length(sbp)
  n.maxhr = length(mbp)
  zeta = (mean(sbp) - mean(mbp))/(sqrt(var.sbp/n.sbp + var.mbp/n.mbp))
  return(zeta)
}


zstar = z.test2sam(sbp, mbp, var(sbp), var(mbp))
zstar


hist(bhr)
library(reshape)
library(reshape2)
library(ggplot2)
library(GGally)
library(lme4)

fit <- glm(any.event ~    dobEF +  posECG
             +  restwma +posSE + hxofHT 
             , family = "binomial")

summary(fit)



fit <- glm(any.event ~  dp +  sbp + dpmaxdo +mphr + mbp 
             + dpmaxdo + age +dobEF+  posECG
             + restwma + posSE + hxofHT 
summary(fit)

modelCART <- glm(any.event ~  basebp + age +dobEF+  hxofHT, family=binomial) 

summary(modelCART)

ggpairs(cardiac[, c("pkhr", "maxhr")])
model <- glm(any.event~basedp+dp+gender+hxofMI+hxofPTCA+hxofCABG,family='binomial')
summary(model)


colnames(y)[32] <- "any_event"
colnames(y)[9] <- "mphr"

glm(any.event~pkhr+maxhr)
summary(glm(any.event~pkhr+maxhr))
library(car)
vif(glm(any.event~pkhr+maxhr))



library(rpart)

fit <- rpart(any.event ~  gender+dobdose+baseEF+chestpain+hxofdm+hxofMI+hxofCABG+hxofPTCA+age+dobEF+posECG+equivecg+restwma+posSE+hxofHT+bhr+basebp+pkhr+sbp+dose+maxhr+mphr, method="anova")
printcp(fit)
plotcp(fit)
summary(fit)
#create additional plots
#par(mfrow=c(1,2))
rsq.rpart(fit)
plot(fit,uniform=TRUE,main="Regression Tree for cardiac")
text(fit,use.n=TRUE,all=TRUE,cex=.8)
post(fit,file="E:/academic/PhD/course/Math664/project/tree2.ps",title="Regression Tree for cardiac")
#prune the tree 
pfit1<- prune(fit, cp=0.018699) # from cptable 
