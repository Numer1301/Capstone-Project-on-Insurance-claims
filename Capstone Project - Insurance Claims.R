#==========================///////PROJECT NOTES 1//////============================================
#=================================Set Working Directory============================================
setwd("/Users/numerp/Documents/PGP-BABI/PGP-BABI Capstone Project 2020/Insurance Claims")
getwd()
#=================================Libraries Loaded=================================================
library(readxl)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(car)
library(DataExplorer)
library(ggcorrplot)
library(tidyverse)
library(janitor)
library(lubridate)
library(usdm)
library(raster)
library(lattice)
library(scales)
library(GGally)
library(vcd)
library(flexclust)
library(fpc)
library(factoextra)
library(rms)
library(pscl)
library(caret)
library(rpart)
library(rattle)
library(ROCR)
library(data.table)
library(ineq)
library(randomForest)
library(pROC)
library(ROSE)
library(xgboost)
library(DMwR)
library(NbClust)
library(cluster)
#=================================Importing Dataset================================================
claim=read_excel("Insurance Claims Data.xlsx",sheet = 1)
dim(claim) #75200 Obs 32 Var
str(claim)
summary(claim)
#=================================Variable and Values Conversion===================================
claim$DRV_CLAIM_STATUS=as.factor(claim$DRV_CLAIM_STATUS)
summary(claim$DRV_CLAIM_STATUS)
claim$DRV_CLAIM_STATUS=factor(claim$DRV_CLAIM_STATUS,levels = c("CLOSED","REJECTED"),
                              labels = c("0","1"))
summary(claim$DRV_CLAIM_STATUS)
prop.table(table(claim$DRV_CLAIM_STATUS))
#3876/(71234+3876) *****0.051*****
class(claim$DRV_CLAIM_STATUS)
claim$Boo_Endorsement=as.factor(claim$Boo_Endorsement)
summary(claim$Boo_Endorsement)
class(claim$Boo_Endorsement)
claim$Boo_TPPD_Statutory_Cover_only=as.factor(claim$Boo_TPPD_Statutory_Cover_only)
summary(claim$Boo_TPPD_Statutory_Cover_only)
class(claim$Boo_TPPD_Statutory_Cover_only)
claim$Boo_OD_Total_Loss=as.factor(claim$Boo_OD_Total_Loss)
summary(claim$Boo_OD_Total_Loss)
class(claim$Boo_OD_Total_Loss)
claim$Boo_AntiTheft=as.factor(claim$Boo_AntiTheft)
summary(claim$Boo_AntiTheft)
class(claim$Boo_AntiTheft)
claim$Boo_NCB=as.factor(claim$Boo_NCB)
summary(claim$Boo_NCB)
class(claim$Boo_NCB)
claim$Date_Accident_Loss=as_date(claim$Date_Accident_Loss)
summary(claim$Date_Accident_Loss)
class(claim$Date_Accident_Loss)
claim$Date_Claim_Intimation=as_date(claim$Date_Claim_Intimation)
summary(claim$Date_Claim_Intimation)
class(claim$Date_Claim_Intimation)
claim$Date_Disbursement=as_date(claim$Date_Disbursement)
summary(claim$Date_Disbursement)
class(claim$Date_Disbursement)
claim$Txt_Policy_Year
claim$Txt_Policy_Year=factor(claim$Txt_Policy_Year,
                             levels = c("1998-99","1999-00","2000-01","2001-02","2002-03",
                                        "2003-04","2004-05","2005-06","2006-07","2007-08",
                                        "2008-09","2009-10","2010-11","2011-12","2012-13"),
                             labels = c("1998","1999","2000","2001","2002","2003","2004",
                                        "2005","2006","2007","2008","2009","2010","2011",
                                        "2012"))
summary(claim$Txt_Policy_Year)
claim$Txt_Policy_Year=as.character(claim$Txt_Policy_Year)
claim$Txt_Policy_Year=as_date(strptime(claim$Txt_Policy_Year,format ='%Y'))
summary(claim$Txt_Policy_Year)
class(claim$Txt_Policy_Year)
claim$Txt_Claim_Year
claim$Txt_Claim_Year=factor(claim$Txt_Claim_Year,
                             levels = c("1999-00","2000-01","2001-02","2002-03",
                                        "2003-04","2004-05","2005-06","2006-07","2007-08",
                                        "2008-09","2009-10","2010-11","2011-12","2012-13"),
                             labels = c("1999","2000","2001","2002","2003","2004",
                                        "2005","2006","2007","2008","2009","2010","2011",
                                        "2012"))
claim$Txt_Claim_Year=as.character(claim$Txt_Claim_Year)
claim$Txt_Claim_Year=as_date(strptime(claim$Txt_Claim_Year,format ='%Y'))
summary(claim$Txt_Claim_Year)
class(claim$Txt_Claim_Year)
claim$Num_Vehicle_Age=as.numeric(claim$Num_Vehicle_Age)
str(claim)
#=================================Colnames Edit=========================================================
names(claim)
names(claim)[2]="Policy_Year"
names(claim)[3]="Endorsement"
names(claim)[4]="Location_RTA"
names(claim)[5]="Policy"
names(claim)[6]="Vehicle_Class"
names(claim)[7]="Zone"
names(claim)[8]="Vehicle_Age"
names(claim)[9]="Vehicle_CC"
names(claim)[10]="Vehicle_Colour"
names(claim)[11]="IDV"
names(claim)[12]="Permit"
names(claim)[13]="Nature_of_Goods"
names(claim)[14]="Road_Type"
names(claim)[15]="Driver_Type"
names(claim)[16]="Driver_Exp"
names(claim)[17]="Claims_History"
names(claim)[18]="Driver_Qualification"
names(claim)[19]="Incurred_Claims"
names(claim)[20]="Statutory_Cover"
names(claim)[21]="Claim_Year"
names(claim)[22]="Accident_Date"
names(claim)[23]="Accident_Place"
names(claim)[24]="Claim_Intimation_Date"
names(claim)[25]="Nature_of_Loss"
names(claim)[26]="Disbursement_Date"
names(claim)[27]="Total_Loss"
names(claim)[28]="Claim_Amount"
names(claim)[29]="Claim_Status"
names(claim)[30]="Antitheft"
names(claim)[31]="Discount_NCB"
names(claim)[32]="Net_Premium"
colnames(claim)
#=================================Duplicate Value Treatment=============================================
claim=subset(claim,select = -c(1))
get_dupes(claim)
claim=unique(claim)
get_dupes(claim)
#=================================Variable Classification==============================================
names(claim)
num_var=subset(claim,select = c(7,10,27,31))
names(num_var)
head(num_var,4)
cat_var=subset(claim,select = -c(1,2,7,10,19,20,21,23,25,26,27:31))
names(cat_var)
head(cat_var,4)
factor_var=subset(claim,select = -c(1,3:18,20:25,27,31))
names(factor_var)
head(factor_var,4)
date_var=subset(claim,select = c(1,20,21,23,25))
names(date_var)
head(date_var,4)
#=================================Missing Value Treatment===========================================
summary(claim)
colSums(is.na(claim))
class(claim)
claim=as.data.frame(claim)
claim$Disbursement_Date[which(is.na(claim$Disbursement_Date))] = median(claim$Disbursement_Date,
                                                                         na.rm = TRUE)
any(is.na(claim))
summary(claim$Disbursement_Date)
#=================================Outlier Treatment================================================
summary(claim[,c(7,10,27,31)])
boxplot(claim[c(7,10,27,31)],plot = FALSE)$out
IQRage = IQR(claim$Vehicle_Age)
LLage = quantile(claim$Vehicle_Age,0.25) - 1.5*IQRage
ULage = quantile(claim$Vehicle_Age,0.75) + 1.5*IQRage
ageOut = subset(claim, Vehicle_Age >= LLage & Vehicle_Age <= ULage)
dim(ageOut)
max(ageOut$Vehicle_Age)
summary(ageOut$Vehicle_Age)
claim$Vehicle_Age[claim$Vehicle_Age > 10] = 10
summary(claim$Vehicle_Age)
claim$IDV=squish(claim$IDV,round(quantile(claim$IDV,c(0.5,0.95))))
summary(claim$IDV)
claim$Claim_Amount=squish(claim$Claim_Amount,round(quantile(claim$Claim_Amount,c(0.5,0.95))))
summary(claim$Claim_Amount)
claim$Net_Premium=squish(claim$Net_Premium,round(quantile(claim$Net_Premium,c(0.5,0.95))))
summary(claim$Net_Premium)
summary(claim[,c(7,10,27,31)])
#=================================Univariate Analysis==============================================
attach(claim)
boxplot(num_var)
plot_bar(data = factor_var,title = "Benefits Summary",ggtheme = theme_minimal())
plot_bar(data = cat_var,title = "Summary",ncol = 4,ggtheme = theme_minimal())
plot_bar(data = claim[,c(2,14,15,12,16,17,19,20,26,31)],nrow = 3,ncol = 3,
         ggtheme = theme_minimal())
#par(mfrow=c(2,2))
hist(Claim_Amount,label=TRUE,col = "red")
hist(Vehicle_Age,label=TRUE,col = "grey")
hist(IDV,label=TRUE,col = "beige")
hist(Net_Premium,label=TRUE, col = "gold")
boxplot(Claim_Amount,label =TRUE, horizontal =TRUE, col = "red",main = "Boxplot - Claim Amount")
boxplot(Vehicle_Age,label =TRUE, horizontal =TRUE, col = "grey",main = "Boxplot - Vehicle Age")
boxplot(IDV,label =TRUE, horizontal =TRUE, col = "beige",main = "Boxplot - IDV")
boxplot(Net_Premium,label =TRUE, horizontal =TRUE, col = "gold",main = "Boxplot - Net Premium")
#=================================Bivariate Analysis===============================================
plot_boxplot(data = claim,by="Claim_Status",ncol = 2,nrow = 3,ggtheme = theme_minimal())
#Policy Coverages
ggplot(data = claim,aes(x=Policy,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Vehicle_CC,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Zone,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Permit,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Nature_of_Goods,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Nature_of_Loss,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Road_Type,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Driver_Type,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Driver_Exp,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Driver_Qualification,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Claims_History,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Incurred_Claims,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Diff_Claim_Days,fill=Claim_Status))+geom_dotplot()+theme_minimal()
#Additional Benefits
ggplot(data = claim,aes(x=Statutory_Cover,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Endorsement,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Antitheft,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Discount_NCB,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
ggplot(data = claim,aes(x=Total_Loss,fill=Claim_Status))+geom_bar(position = "stack")+
  theme_minimal()
scatterplot(Vehicle_Age,Claim_Amount,data=claim,ellipse = TRUE)
scatterplot(Vehicle_Age,Net_Premium,data=claim,ellipse = TRUE)
ggplot(data = claim,aes(x=Policy_Year,y=IDV,color=Policy,shape=Claim_Status))+
  geom_point(size=3,alpha=0.6)
ggplot(data = claim,aes(x=Vehicle_Age,y=IDV,color=Claims_History,shape=Claim_Status))+
  geom_point(size=3,alpha=0.6)
ggplot(data = claim,aes(x=Vehicle_Class,y=IDV,color=Claims_History,shape=Claim_Status))+
  geom_point(size=3,alpha=0.6)
ggplot(data = claim,aes(x=Vehicle_Age,y=Policy_Year,color=Claims_History,shape=Claim_Status))+
  geom_point(size=3,alpha=0.6)
#=================================Cross Tables=====================================================
table(Claim_Status,Zone)
table(Claim_Status,Policy)
table(Claim_Status,Vehicle_Age)
table(Claim_Status,Vehicle_Class)
table(Claim_Status,Claims_History)
table(Claim_Status,Driver_Type)
table(Claim_Status,Road_Type)
table(Antitheft,Claim_Status)
table(Discount_NCB,Claim_Status)
table(Endorsement,Claim_Status)
table(Statutory_Cover,Claim_Status)
table(Total_Loss,Claim_Status)
#=================================Correlation======================================================
correlation=cor(claim[,c(7,10,27,31)])
correlation
corrplot::corrplot(correlation)
#=================================Variance Inflation Factor========================================
num_data=unlist(lapply(claim,is.numeric))
usdm::vif(subset(claim,select=(num_data)))
#==========================///////PROJECT NOTES 2/////=============================================
#=================================Splitting Data===================================================
library(caTools)
set.seed(123)
my_split=sample.split(claim$Claim_Status,SplitRatio = 0.8)
my_train=subset(claim,my_split==TRUE)
my_test=subset(claim,my_split==FALSE)
table(my_train$Claim_Status)
prop.table(table(my_train$Claim_Status))
table(my_test$Claim_Status)
prop.table(table(my_test$Claim_Status))
attach(claim)
#=================================Model Building===================================================
#=================================Logistics Regression=============================================
model1=glm(Claim_Status~. -Location_RTA -Vehicle_Colour -Accident_Place,
           data = my_train,family = binomial(link = logit))
summary(model1)
#Removing Insignificant Variables
model2=glm(Claim_Status~Endorsement+Vehicle_Class+Zone+Vehicle_Age+Permit+Nature_of_Goods+
             Road_Type+Driver_Type+Driver_Exp+Claims_History+Driver_Qualification+
             Statutory_Cover+Claim_Year+Claim_Intimation_Date+
             Total_Loss+Net_Premium,data = my_train,family = binomial(link = logit))
summary(model2)
car::vif(model2)
#Removing higher Variance Inflation Rate Variables
model3=glm(Claim_Status~Endorsement+Vehicle_Class+Zone+Vehicle_Age+Nature_of_Goods+
             Driver_Type+Driver_Exp+Claims_History+Driver_Qualification+
             Statutory_Cover+Claim_Year+Claim_Intimation_Date+Total_Loss+
             Net_Premium,data = my_train,family = binomial(link = logit))
summary(model3)
#Removing Insignificant Variables
model4=glm(Claim_Status~Endorsement+Vehicle_Class+Zone+Nature_of_Goods+Driver_Type+
             Driver_Exp+Claims_History+Driver_Qualification+Statutory_Cover+Claim_Year+
             Claim_Intimation_Date+Total_Loss+Net_Premium,
           data = my_train,family = binomial(link = logit))
summary(model4)
car::vif(model4)
#Removing Higher Variance Inflation Rate Variables
model5=glm(Claim_Status~Endorsement+Driver_Type+Driver_Exp+Nature_of_Goods+Claims_History+
             Driver_Qualification+Statutory_Cover+Total_Loss+
             Net_Premium,data = my_train,family = binomial(link = logit))
summary(model5)
car::vif(model5)
exp(coef(model5))
exp(coef(model5))/(1+exp(coef(model5)))
#McFadden 0to0.10 - Bad,0.10to0.15 - Average,0.15to0.3 - Moderate,0.3to0.5 - Good,>0.5 Excellent
pscl::pR2(model5)["McFadden"]
#Likelihood Estimate
logLik(model5)
#=================================Prediction on Train - LR Model=======================================
LR_pred=predict(model5,newdata = my_train,type = "response")
roc_pred=prediction(LR_pred,my_train$Claim_Status)
as.numeric(performance(roc_pred,"auc")@y.values)
perf=performance(roc_pred,"tpr","fpr")
plot(perf)
plot(perf,colorize=TRUE,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))
LR_table=table(my_train$Claim_Status,LR_pred>0.1)
LR_table
sum(diag(LR_table))/sum(LR_table)
1540/(1540+1561) #Recall 0.49
1540/(1540+6926) #Precision 0.18
pred=ifelse(model5$fitted.values>0.1,1,0)
actual=my_train$Claim_Status
cm_log=caret::confusionMatrix(as.factor(pred),actual,positive="1")
cm_log
#=================================Top 10 Deciles Ranking================================================
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
#=================================Ranking - Train LR Model============================================
LR_train=my_train
LR_train$pred=predict(model5,LR_train,type = "response")
LR_train$deciles=decile(LR_train$pred)
m=data.table::data.table(LR_train)
rank_lr = m[, list(cnt=length(Claim_Status),
                        cnt_resp=sum(Claim_Status==1),
                        cnt_non_resp=sum(Claim_Status==0)
), by=deciles][order(-deciles)]
rank_lr$rrate=round(rank_lr$cnt_resp/rank_lr$cnt,4)
rank_lr$cum_resp=cumsum(rank_lr$cnt_resp)
rank_lr$cum_non_resp=cumsum(rank_lr$cnt_non_resp)
rank_lr$cum_rel_resp=round(rank_lr$cum_resp/sum(rank_lr$cnt_non_resp),4)
rank_lr$cum_rel_non_resp=round(rank_lr$cum_non_resp/sum(rank_lr$cnt_non_resp),4)
rank_lr$ks=abs(rank_lr$cum_rel_resp - rank_lr$cum_rel_non_resp)*100
rank_lr$rrate=scales::percent(rank_lr$rrate)
rank_lr$cum_rel_resp=scales::percent(rank_lr$cum_rel_resp)
rank_lr$cum_rel_non_resp=scales::percent(rank_lr$cum_rel_non_resp)
LR_train_rank=rank_lr
print(LR_train_rank)
plot(LR_train_rank$ks)
lines(LR_train_rank$ks)
#=================================Prediction on Test - LR Model========================================
LR_pred1=predict.glm(model5,newdata = my_test,type = "response")
LR_pred1
roc_pred1=prediction(LR_pred1,my_test$Claim_Status)
as.numeric(performance(roc_pred1,"auc")@y.values)
perf1=performance(roc_pred1,"tpr","fpr")
plot(perf1)
plot(perf1,colorize=TRUE,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.1,1.7))
LR_table1=table(my_test$Claim_Status,LR_pred1>0.1)
LR_table1
sum(diag(LR_table1))/sum(LR_table1)
387/(387+388) #Recall 0.49
387/(387+1711) #Precision 0.18
pred1=ifelse(LR_pred1>0.1,1,0)
actual1=my_test$Claim_Status
cm_log1=caret::confusionMatrix(as.factor(pred1),actual1,positive="1")
cm_log1
#=================================Ranking - Test LR Model==============================================
LR_test=my_test
LR_test$pred=predict(model5,LR_test,type = "response")
LR_test$deciles=decile(LR_test$pred)
n=data.table::data.table(LR_test)
rank_lr1 = n[, list(cnt=length(Claim_Status),
                   cnt_resp=sum(Claim_Status==1),
                   cnt_non_resp=sum(Claim_Status==0)
), by=deciles][order(-deciles)]
rank_lr1$rrate=round(rank_lr1$cnt_resp/rank_lr1$cnt,4)
rank_lr1$cum_resp=cumsum(rank_lr1$cnt_resp)
rank_lr1$cum_non_resp=cumsum(rank_lr1$cnt_non_resp)
rank_lr1$cum_rel_resp=round(rank_lr1$cum_resp/sum(rank_lr1$cnt_non_resp),4)
rank_lr1$cum_rel_non_resp=round(rank_lr1$cum_non_resp/sum(rank_lr1$cnt_non_resp),4)
rank_lr1$ks=abs(rank_lr1$cum_rel_resp - rank_lr1$cum_rel_non_resp)*100
rank_lr1$rrate=scales::percent(rank_lr1$rrate)
rank_lr1$cum_rel_resp=scales::percent(rank_lr1$cum_rel_resp)
rank_lr1$cum_rel_non_resp=scales::percent(rank_lr1$cum_rel_non_resp)
LR_test_rank=rank_lr1
print(LR_test_rank)
plot(LR_test_rank$ks)
lines(LR_test_rank$ks)
#=================================KS, ROC, AUC, GINI - LR Model========================================
roc_curve=roc(my_train$Claim_Status,model5$fitted.values)
plot.roc(roc_curve)
AUC=as.numeric(performance(roc_pred1,"auc")@y.values)
AUC
KS=max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
KS
GINI=ineq(LR_pred1,type = "Gini")
GINI
#=================================CART================================================================
modelcart=rpart(formula = Claim_Status ~ .,data = my_train,method = "class")
printcp(modelcart)
modelcart
names(my_train)
tree_control=rpart.control(minsplit=99, minbucket = 4, cp = 0, xval = 4)
#Removing Dates from CART Model
tree=rpart(formula = Claim_Status ~ .,data = my_train[,-c(3,9,22,24,1,20,21,23,25)], method = "class", 
                     control = tree_control)
printcp(tree)
tree
rpart::plotcp(tree)
#Removing Claim Amount, since it is significant only
tree_control1=rpart.control(minsplit = 250,minbucket = 5,cp = 0,xval = 5)
tree1=rpart(formula = Claim_Status ~ .,data = my_train[,-c(3,9,22,24,1,20,21,23,25,27)], 
            method = "class",control = tree_control1)
printcp(tree1)
tree1
rpart::plotcp(tree1)
rattle::fancyRpartPlot(tree1)
tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]
#Prune Tree
ptree=prune(tree1,cp=0.00064495,"CP")
printcp(ptree)
plotcp(ptree)
fancyRpartPlot(ptree)
ptree
#=================================Prediction on Train - CART Model======================================
train_cart=my_train
train_cart$predict=predict(ptree,train_cart,type="class")
train_cart$score=predict(ptree,train_cart,type="prob")
train_cart$deciles=decile(train_cart$score[,2])
#=================================Ranking - Train CART Model============================================
table_train_cart = data.table(train_cart)
rank = table_train_cart[, list(
  cnt = length(as.integer(as.character(Claim_Status))), 
  cnt_resp = sum(as.integer(as.character(Claim_Status))), 
  cnt_non_resp = sum(as.integer(as.character(Claim_Status)) == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate = round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp = cumsum(rank$cnt_resp)
rank$cum_non_resp = cumsum(rank$cnt_non_resp)
rank$cum_rel_resp = round(rank$cum_resp  / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp = round(rank$cum_non_resp  / sum(rank$cnt_non_resp),4);
rank$rrate_perc = percent(rank$rrate)
rank$cum_rel_resp_perc = percent(rank$cum_rel_resp)
rank$cum_rel_non_resp_perc = percent(rank$cum_rel_non_resp)
rank$cum_cnt = cumsum(rank$cnt)
rank$cum_resp_rate = round(rank$cum_resp  / rank$cum_cnt,4)
overall_resp_rate = sum(as.integer(as.character(train_cart$Claim_Status)))/nrow(train_cart)
rank
overall_resp_rate
#=================================Lift - Train CART Model==============================================
rank$lift=round(rank$cum_resp_rate/overall_resp_rate,2)
rank$lift
#=================================KS, AUC, GINI - Train CART Model=====================================
pred_train_cart = prediction(train_cart$score[,2], train_cart$Claim_Status)
perf_train_cart = performance(pred_train_cart, "tpr", "fpr")
plot(perf_train_cart)
KS_train_cart = max(attr(perf_train_cart, 'y.values')[[1]]-attr(perf_train_cart, 'x.values')[[1]])
KS_train_cart
auc_train_cart = performance(pred_train_cart,"auc"); 
performance(pred_train_cart,"auc")
gini_train_cart = ineq(train_cart$score[,2], type="Gini")
gini_train_cart
cm_train_cart=confusionMatrix((table(train_cart$predict,train_cart$Claim_Status)),
                              mode = "everything")
cm_train_cart
#=================================Prediction on Test - CART Model======================================
test_cart=my_test
test_cart$predict=predict(ptree,test_cart,type="class")
test_cart$score=predict(ptree,test_cart,type="prob")
test_cart$deciles=decile(test_cart$score[,2])
#=================================Ranking - Test CART Model============================================
table_test_cart = data.table(test_cart)
rank1 = table_test_cart[, list(
  cnt = length(as.integer(as.character(Claim_Status))), 
  cnt_resp = sum(as.integer(as.character(Claim_Status))), 
  cnt_non_resp = sum(as.integer(as.character(Claim_Status)) == 0)) , 
  by=deciles][order(-deciles)]
rank1$rrate = round(rank1$cnt_resp / rank1$cnt,4);
rank1$cum_resp = cumsum(rank1$cnt_resp)
rank1$cum_non_resp = cumsum(rank1$cnt_non_resp)
rank1$cum_rel_resp = round(rank1$cum_resp  / sum(rank1$cnt_resp),4);
rank1$cum_rel_non_resp = round(rank1$cum_non_resp  / sum(rank1$cnt_non_resp),4);
rank1$rrate_perc = percent(rank1$rrate)
rank1$cum_rel_resp_perc = percent(rank1$cum_rel_resp)
rank1$cum_rel_non_resp_perc = percent(rank1$cum_rel_non_resp)
rank1$cum_cnt = cumsum(rank1$cnt)
rank1$cum_resp_rate = round(rank1$cum_resp  / rank1$cum_cnt,4)
overall_resp_rate1 = sum(as.integer(as.character(test_cart$Claim_Status)))/nrow(test_cart)
rank1
overall_resp_rate1
#=================================Lift - Test CART Model===============================================
rank1$lift=round(rank1$cum_resp_rate/overall_resp_rate1,2)
rank1$lift
plot(rank1$lift)
lines(rank1$lift)
#=================================KS, AUC, GINI - Test CART Model======================================
pred_test_cart = prediction(test_cart$score[,2], test_cart$Claim_Status)
perf_test_cart = performance(pred_test_cart, "tpr", "fpr")
plot(perf_test_cart)
KS_test_cart = max(attr(perf_test_cart, 'y.values')[[1]]-attr(perf_test_cart, 'x.values')[[1]])
KS_test_cart
auc_test_cart <- performance(pred_test_cart,"auc"); 
performance(pred_test_cart,"auc")
gini_test_cart = ineq(test_cart$score[,2], type="Gini")
gini_test_cart
cm_test_cart=confusionMatrix((table(test_cart$predict,test_cart$Claim_Status)),
                              mode = "everything")
cm_test_cart
#=================================Random Forest=======================================================
set.seed(123)
rftrain=subset(my_train,select=-c(3,9,22,24,1,20,21,23,25))
rftest=subset(my_test,select=-c(3,9,22,24,1,20,21,23,25))
#Removing Higher Level Variables and Date Variables
rf_claim=randomForest::randomForest(as.factor(Claim_Status) ~ ., 
                                    data = rftrain, 
                                    ntree = 101,mtry = 5, nodesize = 100,importance = TRUE)
print(rf_claim)
plot(rf_claim, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")
rf_claim$err.rate
importance(rf_claim)
#Removing Claim Amount
names(rftrain)
rf_claim1=randomForest::randomForest(as.factor(Claim_Status) ~ ., 
                                    data = rftrain[,-18], 
                                    ntree = 101,mtry = 5, nodesize = 100,importance = TRUE)
print(rf_claim1)
plot(rf_claim1, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")
rf_claim1$err.rate
important_var=round(importance(rf_claim1),2)
important_var[order(important_var[,4],decreasing = TRUE),]
#tune RF Model
set.seed(1234)
tune_rf_claim=tuneRF(x = rftrain[,-c(18,19)],
                     y=rftrain$Claim_Status,
                     mtryStart = 5,
                     ntreeTry = 61,
                     stepFactor = 1.2,
                     improve = 1.1,
                     trace = TRUE,
                     plot = TRUE,
                     doBest = TRUE,
                     nodesize = 100,
                     importance = TRUE
                     )
tune_rf_claim
importance(tune_rf_claim)
important_var=round(importance(tune_rf_claim),2)
important_var
important_var[order(important_var[,4],decreasing = TRUE),]
plot(tune_rf_claim)
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
#=================================Prediction on Train - RF Model======================================
train_rf=rftrain
train_rf$predict=predict(tune_rf_claim,train_rf,type="class")
train_rf$score=predict(tune_rf_claim,train_rf,type="prob")
train_rf$deciles=decile(train_rf$score[,2])
#=================================Ranking - Train RF Model============================================
table_train_rf = data.table(train_rf)
rank_rf = table_train_rf[, list(
  cnt = length(as.integer(as.character(Claim_Status))), 
  cnt_resp = sum(as.integer(as.character(Claim_Status))), 
  cnt_non_resp = sum(as.integer(as.character(Claim_Status)) == 0)) , 
  by=deciles][order(-deciles)]
rank_rf$rrate = round(rank_rf$cnt_resp / rank_rf$cnt,4);
rank_rf$cum_resp = cumsum(rank_rf$cnt_resp)
rank_rf$cum_non_resp = cumsum(rank_rf$cnt_non_resp)
rank_rf$cum_rel_resp = round(rank_rf$cum_resp  / sum(rank_rf$cnt_resp),4);
rank_rf$cum_rel_non_resp = round(rank_rf$cum_non_resp  / sum(rank_rf$cnt_non_resp),4);
rank_rf$rrate_perc = percent(rank_rf$rrate)
rank_rf$cum_rel_resp_perc = percent(rank_rf$cum_rel_resp)
rank_rf$cum_rel_non_resp_perc = percent(rank_rf$cum_rel_non_resp)
rank_rf$cum_cnt = cumsum(rank_rf$cnt)
rank_rf$cum_resp_rate = round(rank_rf$cum_resp  / rank_rf$cum_cnt,4)
overall_resp_rate_rf = sum(as.integer(as.character(train_rf$Claim_Status)))/nrow(train_rf)
rank_rf
overall_resp_rate_rf
#=================================Lift - Train RF Model================================================
rank_rf$lift=round(rank_rf$cum_resp_rate/overall_resp_rate_rf,2)
rank_rf$lift
#=================================KS, AUC, GINI - Train RF Model=======================================
pred_train_rf = prediction(train_rf$score[,2], train_rf$Claim_Status)
perf_train_rf = performance(pred_train_rf, "tpr", "fpr")
plot(perf_train_rf)
KS_train_rf = max(attr(perf_train_rf, 'y.values')[[1]]-attr(perf_train_rf, 'x.values')[[1]])
KS_train_rf
auc_train_rf = performance(pred_train_rf,"auc"); 
performance(pred_train_rf,"auc")
gini_train_rf = ineq(train_rf$score[,2], type="Gini")
gini_train_rf
cm_train_rf=confusionMatrix((table(train_rf$predict,train_rf$Claim_Status)),
                              mode = "everything")
cm_train_rf
#=================================Prediction on Test - RF Model=======================================
test_rf=rftest
test_rf$predict=predict(tune_rf_claim,test_rf,type="class")
test_rf$score=predict(tune_rf_claim,test_rf,type="prob")
test_rf$deciles=decile(test_rf$score[,2])
#=================================Ranking - Test RF Model=============================================
table_test_rf = data.table(test_rf)
rank_rf1 = table_test_rf[, list(
  cnt = length(as.integer(as.character(Claim_Status))), 
  cnt_resp = sum(as.integer(as.character(Claim_Status))), 
  cnt_non_resp = sum(as.integer(as.character(Claim_Status)) == 0)) , 
  by=deciles][order(-deciles)]
rank_rf1$rrate = round(rank_rf1$cnt_resp / rank_rf1$cnt,4);
rank_rf1$cum_resp = cumsum(rank_rf1$cnt_resp)
rank_rf1$cum_non_resp = cumsum(rank_rf1$cnt_non_resp)
rank_rf1$cum_rel_resp = round(rank_rf1$cum_resp  / sum(rank_rf1$cnt_resp),4);
rank_rf1$cum_rel_non_resp = round(rank_rf1$cum_non_resp  / sum(rank_rf1$cnt_non_resp),4);
rank_rf1$rrate_perc = percent(rank_rf1$rrate)
rank_rf1$cum_rel_resp_perc = percent(rank_rf1$cum_rel_resp)
rank_rf1$cum_rel_non_resp_perc = percent(rank_rf1$cum_rel_non_resp)
rank_rf1$cum_cnt = cumsum(rank_rf1$cnt)
rank_rf1$cum_resp_rate = round(rank_rf1$cum_resp  / rank_rf1$cum_cnt,4)
overall_resp_rate_rf1 = sum(as.integer(as.character(test_rf$Claim_Status)))/nrow(test_rf)
rank_rf1
overall_resp_rate_rf1
#=================================Lift - Test RF Model================================================
rank_rf1$lift=round(rank_rf1$cum_resp_rate/overall_resp_rate_rf1,2)
rank_rf1$lift
#=================================KS, AUC, GINI - Test RF Model======================================
pred_test_rf = prediction(test_rf$score[,2], test_rf$Claim_Status)
perf_test_rf = performance(pred_test_rf, "tpr", "fpr")
plot(perf_test_rf)
KS_test_rf = max(attr(perf_test_rf, 'y.values')[[1]]-attr(perf_test_rf, 'x.values')[[1]])
KS_test_rf
auc_test_rf <- performance(pred_test_rf,"auc"); 
performance(pred_test_rf,"auc")
gini_test_rf = ineq(test_rf$score[,2], type="Gini")
gini_test_rf
cm_test_rf=confusionMatrix((table(test_rf$predict,test_rf$Claim_Status)),
                             mode = "everything")
cm_test_rf
#=================================Extreme Gradient Boosting=========================================
xgbtrain=my_train
xgbtest=my_test
xgbftrain=as.matrix(xgbtrain[,c(7,10,27,31)])
xgbltrain=as.matrix(xgbtrain[,28])
xgbftest=as.matrix(xgbtest[,c(7,10,27,31)])
xgbfit=xgboost::xgboost(
  data = xgbftrain,
  label = xgbltrain,
  eta = 0.001,
  max_depth = 3,
  min_child_weight = 3,
  nrounds = 100,
  nfold = 5,
  objective = "binary:logistic",
  verbose = 0,
  early_stopping_rounds = 10
)
xgbfit
xgbtest$pred.class.xgb=predict(xgbfit,xgbftest)
table.xgb=table(xgbtest$Claim_Status,xgbtest$pred.class.xgb>0.5)
table.xgb
#Extreme Gradient Boosting Tuning
t.xgb=vector()
l=c(0.001, 0.01, 0.1, 0.3, 0.5, 0.7, 1)
m=c(1,3,5,7,9,15)
n=c(2, 50, 100,1000,10000)
for (i in l) {
  xgbfit=xgboost::xgboost(
    data = xgbftrain,
    label = xgbltrain,
    eta = i,
    max_depth = 5,
    nrounds = 10,
    nfold = 5,
    objective = "binary:logistic",
    verbose = 0,
    early_stopping_rounds = 10
  )
  xgbtest$pred.class.xgb=predict(xgbfit,xgbftest)
  t.xgb=cbind(t.xgb,sum(xgbtest$Claim_Status==1 & xgbtest$pred.class.xgb>=0.5))
}
t.xgb
#Best Fit
xgbtest$pred.class.xgb1=predict(xgbfit,xgbftest)
sum(xgbtest$Claim_Status==1 & xgbtest$pred.class.xgb1>=0.5)
table.xgb1=table(xgbtest$Claim_Status,xgbtest$pred.class.xgb1>=0.5)
table.xgb1
xgbtest$pred.class.xgb1=ifelse(xgbtest$pred.class.xgb1<0.5,0,1)
cm_xgb=caret::confusionMatrix(data = factor(xgbtest$pred.class.xgb1),
                              reference = factor(xgbtest$Claim_Status),
                              positive = "1")
cm_xgb
#=================================SMOTE=============================================================
smote_train=my_train[,c(2,7,10,19,26,27:31)]
smote_test=my_test[,c(2,7,10,19,26,27:31)]
Balanced.data=SMOTE(Claim_Status~.,data = smote_train,perc.over = 400,k=5,perc.under = 100)
table(Balanced.data$Claim_Status)
Balanced.data[,c(1,4,5,8,9)]=as.numeric(unlist(Balanced.data[,c(1,4,5,8,9)]))
smote_test[,c(1,4,5,8,9)]=as.numeric(unlist(smote_test[,c(1,4,5,8,9)]))
sftrain=as.matrix(Balanced.data[,-c(7)])
sltrain=as.matrix(Balanced.data$Claim_Status)
smote.xgb=xgboost::xgboost(
  data = sftrain,
  label = sltrain,
  eta = 0.7,
  max_depth = 5,
  nrounds = 50,
  nfold = 5,
  objective = "binary:logistic",
  verbose = 0,
  early_stopping_rounds = 10
)
smote.xgb
sftest=as.matrix(smote_test[,-c(7)])
smote_test$pred.class.smote=predict(smote.xgb,sftest)
smote_test$pred.class.smote=ifelse(smote_test$pred.class.smote<0.5,0,1)
cm_smote=caret::confusionMatrix(data = factor(smote_test$pred.class.smote),
                                reference = factor(smote_test$Claim_Status),
                                positive = "1")
cm_smote
table.smote=table(smote_test$Claim_Status,smote_test$pred.class.smote>=0.5)
table.smote
sum(smote_test$Claim_Status==1 & smote_test$pred.class.smote >= 0.5)
#=================================K-Means Clustering===============================================
num_var1=subset(claim,select=c(7,10,27,31))
num_var1=sapply(num_var1,as.numeric)
head(num_var1)
claims_scaled = scale(num_var1)
head(claims_scaled)
set.seed(123)
K_cluster = kmeans(x=claims_scaled, centers = 2, nstart = 5)
print(K_cluster)
cluster::clusplot(claims_scaled, K_cluster$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)
WSS=rep(0,5)
#Elbow Method
for(k in 1:20){
  set.seed(123)
  clust=kmeans(x=claims_scaled, centers=k, nstart=5)
  WSS[k]=clust$tot.withinss
}
plot(c(1:20), WSS, type="b", xlab="Number of Clusters",
     ylab="sum of 'Within groups sum of squares'") 
K_means = kmeans(x=claims_scaled, centers = 3, nstart = 5)
print(K_means)
cluster::clusplot(claims_scaled,K_means$cluster,
                  color=TRUE,shade=TRUE,labels=2,lines=1)
claims_scaled$clusters=K_means$cluster
print(claims_scaled$clusters)
head(claims_scaled)
claims_profile=aggregate(claim[,c(7,10,27,31)],list(claims_scaled$clusters),FUN="mean")
claims_profile
#=================================Confusion Matrix Cross Validation================================
modelcomparison=c("cm_log1","cm_test_cart","cm_test_rf","cm_xgb","cm_smote")
modelcomparison
table_model=data.frame(Sensitivity = NA,
                          Specificity = NA,
                          Precision = NA,
                          Recall = NA,
                          F1 = NA)
for (i in seq_along(modelcomparison)) {
  model=get(modelcomparison[i])
  a=data.frame(Sensitivity = model$byClass["Sensitivity"],
               Specificity = model$byClass["Specificity"],
               Precision = model$byClass["Precision"],
               Recall = model$byClass["Recall"],
               F1 = model$byClass["F1"])
  rownames(a)=NULL
  table_model=rbind(table_model,a)
}
table_model=table_model[-1,]
row.names(table_model)=c("LOGISTICS REGRESSION","CART","RANDOM FOREST",
                            "EXTREME GRADIENT BOOSTING","SMOTE")
table_model
#=====================================================================================================