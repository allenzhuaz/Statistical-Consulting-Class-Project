cardiac <- read.csv("C:/njit/664/project/cardiac.csv",sep=',',header=TRUE)
attach(cardiac)
library(rpart)

# Regression TREE
fit <- rpart(any.event ~  gender+dobdose+baseEF+chestpain+hxofdm+hxofMI+hxofCABG+hxofPTCA+age+dobEF+posECG+equivecg+restwma+posSE+hxofHT+bhr+basebp+pkhr+sbp+dose+maxhr+X.mphr.b., method="anova",data=cardiac)
printcp(fit)
plotcp(fit)
summary(fit)
#create additional plots
#par(mfrow=c(1,2))
rsq.rpart(fit)
plot(fit,uniform=TRUE,main="Regression Tree for cardiac")
text(fit,use.n=TRUE,all=TRUE,cex=.8)
post(fit,file="C:/njit/664/project/tree2.ps",title="Regression Tree for cardiac")
#prune the tree 
pfit1<- prune(fit, cp=0.018699) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Cardiac")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "C:/njit/664/project/ptree2.ps", 
     title = "Pruned Regression Tree for Cardiac")

