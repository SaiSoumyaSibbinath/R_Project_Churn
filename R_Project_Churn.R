hist(Churn_MV$Day.Calls)
hist(Churn_MV$Day.Charge)

str(Churn_MV)

# Data type transformations
Churn_MV$Intl.Plan = as.factor(Churn_MV$Intl.Plan)
Churn_MV$Area.Code = as.factor(Churn_MV$Area.Code)
Churn_MV$VMail.Plan = as.factor(Churn_MV$VMail.Plan)
Churn_MV$Churn = as.factor(Churn_MV$Churn)

#Check for NA's in each column
summary(Churn_MV)

#Remove all NA columns
#Option 1
Churn_alter = Churn_MV[seq(2, nrow(Churn_MV), 2),]
nrow(Churn_alter)
#Option 2
Churn_new = Churn_MV[!is.na(Churn_MV$Churn), ]
nrow(Churn_new)
#Check if Mean or Median is accurate value
Churn_new$Day.Charge_Mean = ifelse(is.na(Churn_new$Daily.Charges.MV),mean(Churn_new$Day.Charge), Churn_new$Daily.Charges.MV)
Churn_new$Day.Charge_Median = ifelse(is.na(Churn_new$Daily.Charges.MV),median(Churn_new$Day.Charge), Churn_new$Daily.Charges.MV)

meanValue = sqrt(sum((Churn_new$Day.Charge - Churn_new$Day.Charge_Mean)**2))
medianValue = sqrt(sum((Churn_new$Day.Charge - Churn_new$Day.Charge_Median)**2))

meanValue ## Near to actual value
medianValue

#RMSE for Mean
RMSE_Mean = sqrt(mean(((Churn_new$Day.Charge - Churn_new$Day.Charge_Mean) ** 2)))
RMSE_Mean

#RMSE for Median
RMSE_Median = sqrt(mean((Churn_new$Day.Charge - Churn_new$Day.Charge_Median)**2))
RMSE_Median

Churn_new$Daily.Charges.MV = ifelse(is.na(Churn_new$Daily.Charges.MV),mean(Churn_new$Day.Charge), Churn_new$Daily.Charges.MV)


colnames(Churn_new)
library(ggplot2)
#ggplot(Churn_new, aes(x = 1, y = Day.Charge)) + geom_boxplot()

####Outliers 

ggplot(data=Churn_new,aes(x=1,y=Account.Length))+geom_boxplot()
ggplot(data=Churn_new,aes(x=1,y=Day.Mins))+geom_boxplot()
ggplot(data=Churn_new,aes(x=1,y=Eve.Mins))+geom_boxplot()
ggplot(data=Churn_new,aes(x=1,y=Night.Mins))+geom_boxplot()
ggplot(data=Churn_new,aes(x=1,y=Intl.Mins))+geom_boxplot()

## Outliers on Account.Length
Max_Outliers_Fun = function(x){
  
  q3 = quantile(x)[4]
  
  max = q3+(IQR(x)*1.5)
  x = ifelse(x > max, max,x)
  return (x)
}
Min_Outliers_Fun = function(y){
  q1 = quantile(y)[2]
  min = q1-(IQR(y)*1.5)
  y = ifelse(y < min, min,y)
  return (y)
}
## Outliers on Account.Length


max(Churn_new$Account.Length)
min(Churn_new$Account.Length)

Churn_new$Account.Length = Max_Outliers_Fun(Churn_new$Account.Length)

Churn_new$Account.Length = Min_Outliers_Fun(Churn_new$Account.Length)

max(Churn_new$Account.Length)
min(Churn_new$Account.Length)

## Outliers on Eve.Calls

max(Churn_new$Eve.Calls)
min(Churn_new$Eve.Calls)


Churn_new$Eve.Calls = Max_Outliers_Fun(Churn_new$Eve.Calls)
Churn_new$Eve.Calls = Min_Outliers_Fun(Churn_new$Eve.Calls)
max(Churn_new$Eve.Calls)
min(Churn_new$Eve.Calls)

max(Churn_new$Night.Mins)
min(Churn_new$Night.Mins)
Churn_new$Night.Mins = Max_Outliers_Fun(Churn_new$Night.Mins)
Churn_new$Night.Mins = Min_Outliers_Fun(Churn_new$Night.Mins)
max(Churn_new$Night.Mins)
min(Churn_new$Night.Mins)

#ggplot(Churn_new, aes(x = 1, y = y)) + geom_boxplot()

# Scaling Z scoring
?scale
str(Churn_new)
churn_numaricalDF = Churn_new[ ,c ("Account.Length", "VMail.Message", "Day.Mins", "Eve.Mins", "Night.Mins", "Intl.Mins","CustServ.Calls", "Day.Calls", "Day.Charge", "Daily.Charges.MV", "Eve.Calls", "Eve.Charge", "Night.Calls", "Night.Charge", "Intl.Calls", "Intl.Charge")]
churn_catDF = Churn_new[, c("Churn","Intl.Plan","VMail.Plan","State","Area.Code","Phone")]
colnames(churn_numaricalDF)
colnames(churn_catDF)

churn_Scale_df = data.frame(scale(churn_numaricalDF, center = T, scale = T)) # Scale return type is Matrix. so convert it to DF.
churn_Scale_df

##minxmax scaling
  normalized = churn_numaricalDF
  for(y in seq(ncol(normalized))){
    normalized[, y] = (normalized[,y] - min(normalized[, y])) / ((max(normalized[,y]) - min(normalized[,y])))
  }
  
summary(normalized)
#churn_max_min_df = max_min()

install.packages("corrplot")
library(corrplot)
?corrplot
#corp
M
M = cor(churn_Scale_df)
corrplot(M, method = "number")

#churn_Scale_df = churn_Scale_df[, -c(9, 12)] # Index value of column

# Appling Chi Square for Factors
colnames(churn_catDF)

chisq.test(churn_catDF$Churn, churn_catDF$Area.Code, correct = F) #Independent > 0.05
chisq.test(churn_catDF$Churn, churn_catDF$VMail.Plan, correct = F) # Dependent < 0.05
chisq.test(churn_catDF$Churn, churn_catDF$Intl.Plan, correct = F) # Dependent
chisq.test(churn_catDF$Churn, churn_catDF$State, correct = F) # Dependent
chisq.test(churn_catDF$Churn, churn_catDF$Phone, correct = F) # Independent

chisq.test(churn_catDF$Phone, churn_catDF$Intl.Plan, correct = F) # Independent
chisq.test(churn_catDF$Phone, churn_catDF$Area.Code, correct = F) # Independent
chisq.test(churn_catDF$Intl.Plan, churn_catDF$VMail.Plan, correct = F) # Independent

summary(churn_catDF)

churn_combine_minmax = cbind(normalized, churn_catDF)
churn_combine_zscore = cbind(churn_Scale_df, churn_catDF)
colnames(churn_combine)

?aov
traceback()
anova_Vmail = aov(churn_combine_minmax$VMail.Message ~ churn_combine_minmax$VMail.Plan, data = churn_combine_minmax)
anova_Vmail
anova_night = aov(churn_combine_minmax$Night.Calls ~ churn_combine_minmax$Night.Charge, data = churn_combine_minmax)
anova_night

anova_day = aov(churn_combine_minmax$Day.Calls ~ churn_combine_minmax$Day.Charge, data = churn_combine_minmax)
anova_day

anova_night = aov(churn_combine_minmax$Night.Calls ~ churn_combine_minmax$Night.Charge, data = churn_combine_minmax)
anova_night 
# remove : churn, Day.Charge, daily.Charges.MV, Night.Charges, Intl.Charges, State, Area.Code, Phone
churn_combine_minmax$Churn=NULL
churn_combine_minmax$Day.Charge=NULL
churn_combine_minmax$Daily.Charges.MV=NULL
churn_combine_minmax$Night.Charge=NULL
churn_combine_minmax$Intl.Charge=NULL
churn_combine_minmax$State=NULL
churn_combine_minmax$Area.Code=NULL
churn_combine_minmax$Phone=NULL
# target : CustServ.Calls

# Do random and sampling techniques



##EDA
View(churn_combine_minmax)

ggplot(data=churn_combine_minmax,aes(CustServ.Calls))+geom_histogram(bins = 30) ##checking for target variable as ND
ggplot(data=churn_combine_minmax,aes(x= Day.Mins,y=Day.Charge))+geom_point() ##checking for correlation between input variables

##Sampling
set.seed(675)
ids = sample(nrow(churn_combine_minmax),nrow(churn_combine_minmax)*0.8)
Train_DF = churn_combine_minmax[ids,]
Test_DF = churn_combine_minmax[ids,]

colnames(Train_DF)
LR_minmax = lm(CustServ.Calls  ~., data=Train_DF)
summary(LR_minmax)

##prediction on Test data
Test_DF$pred = predict(LR_minmax, newdata=Test_DF )
summary(LR_minmax)






