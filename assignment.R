setwd("/Users/arjunsrivatsava/Downloads/fwdrpracticeexercisedatatrain_csvandtest_csv/R exercise data")
train = read.csv("train.csv")


table(train$Stay_In_Current_City_Years)
#str(train)
train$Stay_In_Current_City_Years1 = train$Stay_In_Current_City_Years
train$Stay_In_Current_City_Years1 = as.character(train$Stay_In_Current_City_Years) 
str(train)

#train$Stay_In_Current_City_Years1 = ifelse(train$Stay_In_Current_City_Years1 == "4+", 4, train$Stay_In_Current_City_Years1)

train[train$Stay_In_Current_City_Year1 == "4+", "Stay_In_Current_City_Years1"] = 4
#train[train$Stay_In_Current_City_Year1 == 4, "Stay_In_Current_City_Years1"] = "4+"

table(train$Stay_In_Current_City_Years)

table(train$Stay_In_Current_City_Years1)

colnames(train)
#3
nrow(train[train$Marital_Status == 0, ] )
nrow(train[(train$Age == "26-35" & train$Marital_Status == 0), ])

# 4
a = train[train$Age == "26-35" & train$Marital_Status == 0, ]
length(unique(a$User_ID))

#5
a = train[,"Age" ]
train$Age
length(unique(a))

length(unique(factor(train$Age)))

#6
length(unique(train$User_ID))

#7

a = data.frame(table(train$Product_ID))
a
b = a[order(a$Freq, decreasing = T), ]
b[1,1]

# Sort will sort on actual value, Order will sort on index

head(order(a$Freq, decreasing = T))

#8
maledf$Purchase = data.frame(ifelse(train$Purchase == "M", train$Purchase, train$Purchase))

colnames(train)
ta = train[,  train$Age]
ta
nrow(ta)
mean(train$Purchase)
table(train$Age)
?table
str(train)
summary(train)
temp = data.frame(train$User_ID)
temp
str(temp)

temp = train[1:3, ]
temp

temp = train[c(1,3,4,7), ]
temp

emp.data = data.frame(
  emp.id = c(1,2,3,4),
  emp.age = c(10,20,30,40),
  emp.address = c("A","B","C","D"),
  emp.salary = c(100.4,200.5,400.2,40.9)
)

emp.data

emp.data$phone = c(123,321,111,333)
emp.data



#8
aggregate(train$Purchase, list(train$Gender), mean)

#What is the average purchase rate of gender = f & gender = m?
colnames(train)

purRate = train[ , "Purchase"]
length(purRate) 
purRateF = train[ train$Gender=="F", "Purchase"]
mean(purRateF)
length(purRateF) 
purRateM = train[ train$Gender=="M", "Purchase"]
length(purRateM) 
mean(purRateM)
?aggregate
aggregate(train$Purchase, by = list(train$Gender), FUN = mean)

aggregate(train$Purchase ~ train$Gender,data = train,FUN = mean )

#What is the average value of purchase when gender = f or age_group = "0-17"

purRate = train[ train$Age == "0-17" | train$Gender == "F", c("Purchase")]
nrow(purRate)

purRateM = purRate[ purRate$Gender=="M", "Purchase"]
length(purRateM)
mean(purRateM)
purRateF = purRate[purRate$Gender=="F", "Purchase"]
length(purRateF)
mean(purRateF)

#What is the average value of purchase within the odd rows of train.csv?

primaryKey = c(1:nrow(train))
primaryKey

completeDF = cbind(primaryKey,train)
#completeDF

oddDF = c()
evenDF = c()

evenDF = completeDF[completeDF$primaryKey %% 2 == 0,  "Purchase"]
oddDF = completeDF[completeDF$primaryKey %% 2 != 0,  "Purchase"]
#ifelse(completeDF$primaryKey %% 2 == 0, evenDF = completeDF[,"Purchase"], oddDF = completeDF[,"Purchase"])
mean(oddDF)
mean(evenDF)

mean(train[seq(2, nrow(train), 2), "Purchase"])

#	create a new dataset (train2) that does not have any row in train.csv that has  missing value

train2 = na.omit(train)
train2

# In which city_category do most of the users within age group "0-17" live?
colnames(train)
which.max(table(train[train$Age == "0-17", "City_Category"]))

## For how many rows is "Product_category_2" missing a value?

missingProducts = train[is.na(train$Product_Category_2), c("User_ID") ]
length(missingProducts) 
missingProducts
sum(is.na(train$Product_Category_2))

# 14. Which value of "Product_category_1" occurs the most whenever product_category_2 value is missing?


train2 = which.max(sort(table(train[is.na(train$Product_Category_2) == T, "Product_Category_1"]), decreasing = T))
train2



#Of all the users that exist in "test.csv", how many of them, also exist in "train.csv"

colnames(train)
UserID_Train = train[, "User_ID"]
UserID_Train

UserID_Test = test[, "User_ID"]
UserID_Test

intersectData = intersect(train$User_ID, test$User_ID)
intersectData

diff = setdiff(train$User_ID, test$User_ID)
diff

union = union(train$User_ID, test$User_ID)
union
?intersect

#Of all the users in "train.csv" how many of them also exist in "test.csv"

diff2 = setdiff(test$User_ID, train$User_ID)
diff2

#What is the average purchase of customers who exist in "train.csv" but not in "test.csv"

PurchaseData = train[train$User_ID %in% diff, "Purchase"]
PurchaseData
mean(PurchaseData)

#How many distinct combinations of "user_id" & "Product_id" are available in train.csv?

userIDdata = unique(train[,"User_ID"])
userIDdata

?unique
?within


#Product_ID
#Among all the variables from "Gender" to "Product_Category_3":
 #A. Calculate the average purchase rate for all the distinct values of each variable

#aggregate(train$Purchase ~ train$Gender,data = train,FUN = mean )

for (i in 1:11) {
  colnames(train[i])
  result = aggregate(train$Purchase,list(train[,i]),mean )
  print(result)
}


#B.identify the variable that has the highest lift in purchase rate

liftResult = NULL

for (i in 1:length(colnames(train[i]))) {
  colnames(train[i])
  result = as.data.frame(aggregate(train$Purchase,list(train[,i]),mean ))
  
  minValue = result[which.min(result$x), "x"]#  min(as.numeric(train[,i]) )
  print(minValue)
  print("Min above")
  maxValue = result[which.max(result$x), "x"] #max(as.numeric(train[,i]))
  print(maxValue)
  print("Max above")
#  print(result) 
  
  resultValue = ((maxValue - minValue)/mean(result$x))
  print(resultValue)
  print("Result Value")
  
  liftResult = rbind(data.frame(cat = c(colnames(train[i]))),
                     resultValue = c(resultValue), liftResult)
}

liftResult

#Write a function that takes variable name as input & gives out the frequency of occurrence table of the distinct values of the variable


frequencyData = function(x){
  print(x)
  print(max(table(train[,x])))
}

for (i in 1:length(colnames(train))) {
  frequencyData(colnames(train[i]))
}

#Write a function that takes variable name & creates dummy variables

a = data.frame(id = rep("", len= length(b)))

dummy = function(y){
  for (level in unique(y)) {
    a[paste("dummy", level, sep = "_" = ifelse(y == level, 1, 0))]
  }
  return(a)
}

install.packages("titan")


