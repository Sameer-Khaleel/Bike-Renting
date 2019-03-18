#clean the enviroment
rm(list=ls())
#set working directory
setwd("E:/study/data")
getwd()
#read the input dataset
df=read.csv("day2.csv")
DB=df

# importing library
x=c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
    "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','knitr','tidyr','xtable','MASS','psych')
lapply(x, require, character.only = TRUE)
rm(x)
library(DMwR)
#Expoloring the data
str(df)
multi.hist(df,main=NA,dcol=c("blue","red"),dlty=c("solid","solid"),bcol="grey")
#Checking for any missing values
table(is.na(df))

#boxplot analysis
#removing instant from dataset
df$instant=NULL
#cnames
numeric_index = sapply(df,is.numeric)
numeric_data = df[,numeric_index]
cnames = colnames(numeric_data)
#box plot for casual riders
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(x = (cnames[i]), y = "casual"), data =subset (df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x=cnames[i],y="casual")+
           ggtitle(paste("Box plot of casual for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
# box plot for registered
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(x = (cnames[i]), y = "registered"), data =subset (df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x=cnames[i],y="registered")+
           ggtitle(paste("Box plot of registered for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
# box plot for total count
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(x = (cnames[i]), y = "count"), data =subset (df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x=cnames[i],y="count")+
           ggtitle(paste("Box plot of total count for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)


#histogram - preprocessing
h1=hist(df$season)
hist(df$weathersit)
hist(df$weekday)
hist(df$holiday)
hist(df$workingday)
hist(df$temp)
hist(df$atemp)
hist(df$hum)
hist(df$windspeed)
hist(df$casual)
hist(df$registered)
#correlation analysis
df$dteday=NULL
numeric_index = sapply(df,is.numeric)
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
sub=data.frame(train$registered,train$casual,train$cnt,train$temp,train$hum,train$atemp,train$windspeed)
cor(sub)
#since atemp is highly corelated we remove atemp from the dataset
df$atemp=NULL
# model 1 - removing outliers from variables
cnames = colnames(df)
#                                   

# #Replace all outliers with NA and impute
# #create NA on "custAge

 for(i in cnames){
   val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
   print(length(val))
   df[,i][df[,i] %in% val] = NA
 }
# 
 
 df$holiday[is.na(df$holiday)] = mean(df$holiday, na.rm = T)
 df$hum[is.na(df$hum)] = mean(df$hum, na.rm = T)
 df$windspeed[is.na(df$windspeed)] = mean(df$windspeed, na.rm = T)
 df$casual[is.na(df$casual)] = mean(df$casual, na.rm = T)
 table(is.na(df))
 
 #Model Development
 #remove dependent variables except the one which is to be targeted
 #df2 =df
 #df=df2
 df$casual=NULL
 df$registered=NULL
 train.index = createDataPartition(df$cnt, p = .80, list = FALSE)
 train = df[ train.index,]
 test  = df[-train.index,]

  #Decision Tree method
 
 
 fit = rpart(cnt ~ ., data = train, method = "anova")
 predictions_DT = predict(fit, test[,-11])
 predictions_DT
 MAPE = function(y, yhat){
   mean(abs((y - yhat)/y)*100)
 }
 regr.eval(test[,11],predictions_DT,stats=c('mse','rmse','mape'))
 MAPE(test[,11],predictions_DT)

 #error rate =22.69%
 #accuracy = 77.30%
 #linear regression
 library(usdm)
 
 vif(df[,-13])
 
 vifcor(df[,-13], th = 0.9)
 
 
 #run regression model
 lm_model = lm(cnt ~., data = train)
 
 #Summary of the model
 summary(lm_model)
 
 
 #Predict
 predictions_LR = predict(lm_model, test[,-11])
 
 #Calculate MAPE
 MAPE(test[,11], predictions_LR)
 regr.eval(test[,11],predictions_DT,stats=c('mse','rmse','mape'))
 #error= 0.18%
 
 #MAE = 635
 ###Random Forest
 RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
 
 #Extract rules fromn random forest
 #transform rf object to an inTrees' format
 treeList = RF2List(RF_model)  
 # 
 # #Extract rules
 exec = extractRules(treeList, train[,-11])  # R-executable conditions
 # 
 # #Visualize some rules
 exec[1:2,]
 # 
 # #Make rules more readable:
 readableRules = presentRules(exec, colnames(train))
 # readableRules[1:2,]
 # 
 # #Get rule metrics
 ruleMetric = getRuleMetric(exec, train[,-11], train$cnt)  # get rule metrics
 # 
 # #evaulate few rules
 # ruleMetric[1:2,]
 
 #Presdict test data using random forest model
 RF_Predictions = predict(RF_model, test[,-11],)
 #error metric
 regr.eval(test[,11],RF_Predictions,stats=c('mse','rmse','mape'))
  #mape error= 12.7%
 MAPE(test[,11],RF_Predictions)

 #Model 3 with outliners
 ##
 df=DB
 df$casual=NULL
 df$registered=NULL
 df$instant=NULL
 df$dteday=NULL
 train.index = createDataPartition(df$cnt, p = .80, list = FALSE)
 train = df[ train.index,]
 test  = df[-train.index,]
 ###Random Forest
 RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
 
 #Extract rules fromn random forest
 #transform rf object to an inTrees' format
 treeList = RF2List(RF_model)  
 # 
 # #Extract rules
 exec = extractRules(treeList, train[,-11])  # R-executable conditions
 # 
 # #Visualize some rules
 exec[1:2,]
 # 
 # #Make rules more readable:
 readableRules = presentRules(exec, colnames(train))
 # readableRules[1:2,]
 # 
 # #Get rule metrics
 ruleMetric = getRuleMetric(exec, train[,-11], train$cnt)  # get rule metrics
 # 
 # #evaulate few rules
 # ruleMetric[1:2,]
 
 #Presdict test data using random forest model
 RF_Predictions = predict(RF_model, test[,-11],)
 #error metric
 regr.eval(test[,11],RF_Predictions,stats=c('mse','rmse','mape'))
 
 MAPE(test[,11],RF_Predictions)
 #Mape error =17.76
 
 ################################################CASE 2 ###################################################################
 df=df2
 
 df=df[-12]
 df=df[-11]
 train.index = createDataPartition(df$cnt, p = .80, list = FALSE)
 train = df[ train.index,]
 test  = df[-train.index,]
 #random forest
 RF_model = randomForest(casual ~ ., train, importance = TRUE, ntree = 500)
 
 #Extract rules fromn random forest
 #transform rf object to an inTrees' format
 treeList = RF2List(RF_model)  
 # 
 # #Extract rules
 exec = extractRules(treeList, train[,-11])  # R-executable conditions
 # 
 # #Visualize some rules
 exec[1:2,]
 # 
 # #Make rules more readable:
 readableRules = presentRules(exec, colnames(train))
 # readableRules[1:2,]
 # 
 # #Get rule metrics
 ruleMetric = getRuleMetric(exec, train[,-11], train$casual)  # get rule metrics
 # 
 # #evaulate few rules
 # ruleMetric[1:2,]
 
 #Presdict test data using random forest model
 RF_Predictions_cas = predict(RF_model, test[,-11],)
 #error metric
 regr.eval(test[,11],RF_Predictions_cas,stats=c('mse','rmse','mape'))
 #mape error= 12.7%
 MAPE(test[,11],RF_Predictions_cas)

 
 
 
 
 
 
 
 