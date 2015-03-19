cardiac <- read.csv("C:/njit/664/project/cardiac.csv",header=T,sep=",")


order <- c("gender","chestpain","posECG","equivecg","restwma","posSE","hxofHT","hxofdm","hxofMI","hxofPTCA","hxofCABG")
for (i in 1:11)
{negative <- tapply(cardiac[,"event..."],list(y[,order[i]]),sum)
 positive <- rbind(220-negative["0"],338-negative["1"])
 table <- cbind(positive,negative)
 chisqtest <- chisq.test(table)
 print(order[i])
 print(table)
 print(chisqtest)}
 
