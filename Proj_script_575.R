setwd("C:\\Users\\adity\\OneDrive\\Documents\\UIC\\IDS 575\\Project")
fdata<-read.csv("UCI_Credit_Card.csv",header = T,sep = ",")
fdata_pca<-read.csv("PCA data v1.0.csv",header = T,sep = ",")
factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
fdata[factor_vars] <- lapply(fdata[factor_vars], function(x) as.factor(x))
str(fdata)
table(fdata$default.payment.next.month)
boxplot(fdata$LIMIT_BAL)
nrow(fdata)
table(fdata$default.payment.next.month)
#x = fdata$EDUCATION[fdata$EDUCATION == c(0,4,5,6)]
fdata$EDUCATION[fdata$EDUCATION== 5]  <- 0 
fdata$EDUCATION[fdata$EDUCATION== 4]  <- 0
fdata$EDUCATION[fdata$EDUCATION== 6]  <- 0
fdata$EDUCATION<-factor(fdata$EDUCATION)
table(fdata$EDUCATION)
fdata$PAY_0[fdata$PAY_0<0]<-0
fdata$PAY_2[fdata$PAY_2<0]<-0
fdata$PAY_3[fdata$PAY_3<0]<-0
fdata$PAY_4[fdata$PAY_4<0]<-0
fdata$PAY_5[fdata$PAY_5<0]<-0
fdata$PAY_6[fdata$PAY_6<0]<-0
str(fdata)

fdata_pca$EDUCATION[fdata_pca$EDUCATION== 5]  <- 0 
fdata_pca$EDUCATION[fdata_pca$EDUCATION== 4]  <- 0
fdata_pca$EDUCATION[fdata_pca$EDUCATION== 6]  <- 0
fdata_pca$EDUCATION<-factor(fdata_pca$EDUCATION)
table(fdata_pca$EDUCATION)
fdata_pca$PAY_0[fdata_pca$PAY_0<0]<-0
fdata_pca$PAY_2[fdata_pca$PAY_2<0]<-0
fdata_pca$PAY_3[fdata_pca$PAY_3<0]<-0
fdata_pca$PAY_4[fdata_pca$PAY_4<0]<-0
fdata_pca$PAY_5[fdata_pca$PAY_5<0]<-0
fdata_pca$PAY_6[fdata_pca$PAY_6<0]<-0
str(fdata_pca)

library(caTools)
set.seed(12345)
split_log <- sample.split(fdata_pca$default.payment.next.month,SplitRatio=0.7)
dataTrain_log=subset(fdata_pca,split_log==TRUE)
dataTest_log=subset(fdata_pca,split_log==FALSE)
table(dataTrain_log$default.payment.next.month)
table(dataTest_log$default.payment.next.month)
Log_reg<-glm(dataTrain_log$default.payment.next.month ~ .-ID -X ,family=binomial(logit),data=dataTrain_log)

summary(Log_reg)
backwards= step(Log_reg)
formula(backwards)

Log_reg_refined<-glm(dataTrain_log$default.payment.next.month ~ LIMIT_BAL + SEX + 
                       EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + 
                       PAY_5 + PAY_6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + 
                       PAY_AMT5,family=binomial(logit),data=dataTrain_log)
summary(Log_reg_refined)



pred = predict(Log_reg_refined,newdata=dataTest_log,type="response")

boxplot(pred)



#pred.default=round(pred)

# cut_off<-0.4
# pred[pred<cut_off]<-0
# pred[pred>=cut_off]<-1

# install.packages(epicalc)
# library(Epi)
# x <- rnorm( 100 )
# z <- rnorm( 100 )
# w <- rnorm( 100 )
# tigol <- function( x ) 1 - ( 1 + exp( x ) )^(-1)
# y <- rbinom( 100, 1, tigol( 0.3 + 3*x + 5*z + 7*w ) )
# ROC( form = y ~ x + z , plot="sp" )

table(dataTest_log$default.payment.next.month)

library(AUC)

library("caret")
library("e1071")
library(ROCR)

#creating an Array of Cutoff Values from 0 to 1, in steps of 0.05
Cutoff<- seq(0,1,0.01)
sns<-vector()
sps<-vector()
nrow(dataTest_log)
remove(pred)
length(pred)

#Running a loop 20 times to update Sensitivity and Specificity from corresponding Cutoffs
for(i in 1:101){
  pred = predict(Log_reg_refined,newdata=dataTest_log,type="response")
  cut_off<-Cutoff[i]
  pred[pred<cut_off]<-0
  pred[pred>=cut_off]<-1
  C<-confusionMatrix(pred,dataTest_log$default.payment.next.month)
  str(C)
  #Calculating the Specificity and Sensitivity from the Confusion matrix
  CM<-as.data.frame.matrix(C$table)
  sns[i]<-CM[1,1]/(CM[1,1]+CM[2,1])
  sps[i]<-CM[2,2]/(CM[2,2]+CM[1,2])
}




#plotting the Sensitivty and Specificity vs Cutoff
plot(Cutoff,sns,type="l",col="red", ylab = 'Sensitiivity/Specificity')
lines(Cutoff,sps,col="green")
legend("topright", c("Sensitivity", "Specificity"), fill=c("red","green"))


#To find intersection, create an array where sensitivity > specifity
above<-sns>sps
# Points always intersect when above=TRUE, then FALSE or reverse
pos<-which(diff(above)!=0)
# To find Threshold, Find the "Position"th value of Cutoff
Threshold<-Cutoff[pos]
Threshold






# plot(sensitivity(dataTest_log$pred ,dataTest_log$default.payment.next.month))
# dim(dataTest_log)
# dim(pred)
# pred<-as.data.frame(pred)
# dataTest_log$pred<-pred
#Setting Threshold to 0.2

pred = predict(Log_reg_refined,newdata=dataTest_log,type="response")

cut_off<-Threshold
pred[pred<cut_off]<-0
pred[pred>=cut_off]<-1

#confusion Matrix 
C<-confusionMatrix(pred,dataTest_log$default.payment.next.month)

C


#install.packages("ROCR")
yhat <- predict(Log_reg_refined, dataTest_log, type = "response")


score <- prediction(yhat, dataTest_log$default.payment.next.month)
plot(performance(score, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")
