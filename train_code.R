
# Setting the current directory as working directory (Please update the code below if this fails)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load train and test datasets
Ax = read.csv("test.csv", stringsAsFactors = FALSE)
Ax$gender = "Test"

head(Ax)

# Merge train and test to create appropriate columns
At = read.csv("train.csv", stringsAsFactors = FALSE)

head(At)

A = rbind(At, Ax)

library(stringr)
y = str_split(A$ProductList, ";")
max(sapply(y, length))

name_list = paste0('Product_Name',1:43)

library(dplyr)
library(tidyr)

# Creating new variables from original dataset

B = A %>% separate(ProductList, name_list, sep = ';')

C = data.frame(t(B[,4:46])) %>% gather(key = 'Product_Item', value = 'Products')

B$Product_Item = paste0('X',1:dim(B[1]))

B = B %>% select(session_id, startTime, endTime, gender, Product_Item)

D = right_join(B, C, by = "Product_Item")

p = as.data.frame(str_split_fixed(D$Products, '/',4))
p$V4 = str_replace(p$V4,'/','')

D$Category = as.character(p$V1)
D$Sub_Category = as.character(p$V2)
D$Sub_Sub_Category = as.character(p$V3)
D$Product_ID = as.character(p$V4)

# Time related variables
library(lubridate)
D$Year = year(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")))
D$Hour = hour(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")))
D$Month = month(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")))
D$Day =  weekdays(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")))
D$Date = day(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")))
D$Session_Duration = as.integer(as.POSIXct(D$endTime, tryFormats = c("%d/%m/%y %H:%M")) - as.POSIXct(D$startTime, tryFormats = c("%d/%m/%y %H:%M")))

D$Time_of_Day = "Morning"
D$Time_of_Day[D$Hour >= 9 & D$Hour <= 19] = "Office_Hours"
D$Time_of_Day[D$Hour >= 20 & D$Hour <= 23] = "Early Night"
D$Time_of_Day[D$Hour >= 0 & D$Hour <= 6] = "Late Night"

E = D[complete.cases(D),]
Fa = E %>% select(session_id, Category, Sub_Category, Sub_Sub_Category, Hour, Date, Day, Time_of_Day, Month, Year, Session_Duration, gender)

G = Fa %>% select(Category, Sub_Category, Sub_Sub_Category, Hour, Date, Day, Time_of_Day, Month, Year, Session_Duration, gender)

Fa$session_id = as.factor(Fa$session_id)
Fa$gender = as.factor(Fa$gender)
Fa$Day = as.factor(Fa$Day)

### Add new variables

# Category Level interactions (which category/sub-category/sub-sub-category pairs are seen together)
P = Fa %>% select(session_id,Category) %>% group_by(session_id) %>% summarise(Category_Combinations = paste(sort(unique(Category)), collapse = ", "), Category_Count = length(unique(Category)), First_Category = Category[1])
index_P = sort(table(P$Category_Combinations),decreasing = TRUE)

n = max(which(index_P >= 3))
n
others_category_interaction = names(index_P)[(32:length(index_P))]

P$Category_Combinations[which(P$Category_Combinations %in% others_category_interaction)] = "Others"

# Sub Category Level Interatcions
Q = Fa %>% select(session_id,Sub_Category) %>% group_by(session_id) %>% summarise(Sub_Category_Combinations = paste(sort(unique(Sub_Category)), collapse = ", "), Sub_Category_Count = length(unique(Sub_Category)), First_Sub_Category = Sub_Category[1])
index_Q = sort(table(Q$Sub_Category_Combinations),decreasing = TRUE)

n = max(which(index_Q >= 3))
n
others_sub_category_interaction = names(index_Q)[(180:length(index_Q))]

Q$Sub_Category_Combinations[which(Q$Sub_Category_Combinations %in% others_sub_category_interaction)] = "Others"

# Sub Sub Category Level Interatcions
R = Fa %>% select(session_id,Sub_Sub_Category) %>% group_by(session_id) %>% summarise(Sub_Sub_Category_Combinations = paste(sort(unique(Sub_Sub_Category)), collapse = ", "), Sub_Sub_Category_Count = length(unique(Sub_Sub_Category)), First_Sub_Sub_Category = Sub_Sub_Category[1])
index_R = sort(table(R$Sub_Sub_Category_Combinations),decreasing = TRUE)

n = max(which(index_R >= 3))
n
others_sub_sub_category_interaction = names(index_R)[(460:length(index_R))]

R$Sub_Sub_Category_Combinations[which(R$Sub_Sub_Category_Combinations %in% others_sub_sub_category_interaction)] = "Others"

# Converting extra sub category, sub-sub category, which account for only 0.01% data, to Others

# Sub Category
useful_sub_category = cumsum(sort(table(G$Sub_Category),decreasing = TRUE))/dim(Fa)[1]
n = which(useful_sub_category >= 0.991)[1]
n
others_sub_category = names(useful_sub_category)[(49:length(useful_sub_category))]

G$Sub_Category[which(G$Sub_Category %in% others_sub_category)] = "Others"

# Sub Sub Category
useful_sub_sub_category = cumsum(sort(table(G$Sub_Sub_Category),decreasing = TRUE))/dim(Fa)[1]
n = which(useful_sub_sub_category >= 0.99)[1]
n
others_sub_sub_category = names(useful_sub_sub_category)[(229:length(useful_sub_sub_category))]

G$Sub_Sub_Category[which(Fa$Sub_Sub_Category %in% others_sub_sub_category)] = "Others"

# Product ID
# useful_product_id = cumsum(sort(table(G$Product_ID),decreasing = TRUE))/dim(Fa)[1]
# n = which(useful_product_id >= 0.20)[1]
# n
# others_useful_product_id = names(useful_product_id)[(822:length(useful_product_id))]
# 
# G$Product_ID[which(Fa$Product_ID %in% others_useful_product_id)] = "Others"

##########

# Creating dummy columns for new variables

H = fastDummies::dummy_cols(G, select_columns = c("Category", "Sub_Category", "Sub_Sub_Category"))
H$session_id = Fa$session_id
count_of_items = H %>% select(session_id) %>% group_by(session_id) %>% summarise(Product_Count = n())

I = H %>% group_by(session_id, Hour, Date, Day, Time_of_Day, Month, Year, Session_Duration, gender) %>% summarise_if(is.numeric, mean)

perc_zero = sapply(I, function(x){sum(x==0)/length(x)*100})

I$Product_Count = count_of_items$Product_Count

# Count of Products per unit time

I$Session_Duration[I$Session_Duration == 0] = 30
I$Session_Duration[I$Session_Duration >= 300*60] = NA
I$Session_Duration[is.na(I$Session_Duration)] = as.integer(mean(I$Session_Duration, na.rm = TRUE))

I$Products_Per_Minute = I$Product_Count/I$Session_Duration*60

I2 = left_join(I, P, by = "session_id")
I3 = left_join(I2, Q, by = "session_id")
I4 = left_join(I3, R, by = "session_id")

If = fastDummies::dummy_cols(I4, select_columns = c("Category_Combinations", "First_Category", "Sub_Category_Combinations", "First_Sub_Category", "Sub_Sub_Category_Combinations", "First_Sub_Sub_Category"))

drop_cols = c("Category_Combinations", "First_Category", "Sub_Category_Combinations", "First_Sub_Category", "Sub_Sub_Category_Combinations", "First_Sub_Sub_Category", "Category_Combinations_Others", "Sub_Category_Combinations_Others", "Sub_Sub_Category_Combinations_Others")
col_index = which(colnames(If) %in% drop_cols)
If = If[,-col_index]

colnames(If) = make.names(colnames(If))
If = data.frame(If)
# I$gender = as.factor(I$gender)

I_train = If[If$gender!="Test",]

I_test = If[If$gender== "Test",]
drop_cols = c("gender")
col_index = which(colnames(I_test) %in% drop_cols)
I_test = I_test[,-col_index]
colnames(I_test)

##### One-hot encode to run LightGBM in Python

I_train_E = fastDummies::dummy_cols(I_train, select_columns = c("Day"))
I_test_E = fastDummies::dummy_cols(I_test, select_columns = c("Day"))

drop_cols = c("Day")
col_index = which(colnames(I_train_E) %in% drop_cols)
I_train_E = I_train_E[,-col_index]

drop_cols = c("Day")
col_index = which(colnames(I_test_E) %in% drop_cols)
I_test_E = I_test_E[,-col_index]

index_test = data.frame(session_id = Ax$session_id, gender = "Test")
XYZ = left_join(index_test, I_test_E)
head(XYZ)

Dataset_Test = XYZ[,c(1,3:ncol(XYZ))]

index_train = data.frame(session_id = At$session_id, extraCol = "Extra")
PQRS = left_join(index_train, I_train_E)
head(PQRS)

Dataset_Train = PQRS[,c(1,3:ncol(PQRS))]

write.csv(I_train, "TransformedTrain.csv", row.names = FALSE)
write.csv(I_test, "TransformedTest.csv", row.names = FALSE)

###################################

# Modeling
library(caret)

set.seed(12345)

data3 = I_train

colnames(data3)

str(data3)

data3$Day = as.factor(data3$Day)
data3$gender = as.factor(data3$gender)
data3$Time_of_Day = as.factor(data3$Time_of_Day)
intrain = createDataPartition(data3$gender, p = 0.8, list = FALSE)

train = data3[intrain,2:ncol(data3)]
test = data3[-intrain,2:ncol(data3)]

### XGBoost

tune_grid <- expand.grid(
  nrounds = seq(from = 300, to = 400, by = 50),
  eta = c(0.025),
  max_depth = c(5),
  gamma = 0,
  colsample_bytree = c(1),
  min_child_weight = 1,
  subsample = c(1)
)

#Model1 = model_xgb
model_xgb_new_features3 = train(gender ~., data = train, method = "xgbTree", trControl = trainctrl, verbose = T, tuneGrid=tune_grid)
#model_xgb2 = train(gender ~., data = train, method = "xgbTree", verbose = TRUE)
model_xgb_new_features3

pred = predict(model_xgb_new_features3, test)

test$gender = as.factor(test$gender)
confusionMatrix(pred, test$gender)

saveRDS(model_xgb_new_features3, "Models/XGB_Base.rds")

trainctrl <- trainControl(method = "cv", number = 4, verboseIter = TRUE, allowParallel = T, sampling = "smote", classProbs = TRUE)
tune_grid <- expand.grid(
  nrounds = seq(from = 300, to = 400, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = c(0.8,1),
  min_child_weight = 1,
  subsample = c(0.75,1)
)

#Model1 = model_xgb
model_xgb_new_features4 = train(gender ~., data = train, method = "xgbTree", trControl = trainctrl, verbose = T, tuneGrid=tune_grid)
#model_xgb2 = train(gender ~., data = train, method = "xgbTree", verbose = TRUE)
model_xgb_new_features4

pred = predict(model_xgb_new_features4, test)

test$gender = as.factor(test$gender)
confusionMatrix(pred, test$gender)

saveRDS(model_xgb_new_features4, "Models/XGB_Optimized.rds")

### Random Forest

library(randomForest)

model_rf_new_features2 = randomForest(gender ~ ., data = train, do.trace = TRUE, ntree = 50)
#train=train %>% mutate_if(is.character, as.factor)
model_rf_new_features2

pred = predict(model_rf_new_features2, test)

test$gender = as.factor(test$gender)
confusionMatrix(pred, test$gender)

pred_score = data.frame(predict(model_rf_new_features2, test, type = 'prob'))

x = seq(0.05,1,0.05)
acc = seq(0.05,1,0.05)

for (i in 1:length(x))
{
  thresh = x[i]
  
  pred = rep('male', dim(test)[1])
  
  pred[pred_score$female>=thresh] = 'female'
  
  pred = as.factor(pred)
  levels(pred) = c('female', 'male')
  
  #pred = exp(pred) - 1
  library(MLmetrics)
  
  A = confusionMatrix(pred, test$gender)
  
  acc[i] = (A$table[1,1] + A$table[2,2])/dim(test)[1]
}

plot(x,acc)
View(data.frame(x,acc))

saveRDS(model_rf_new_features2, "Models/RF_Base2.rds")

control <- trainControl(method = "cv", number = 4, search="random", classProbs = TRUE, verboseIter = TRUE, sampling = "smote")
tunegrid <- expand.grid(mtry=60, splitrule = c("gini"), min.node.size = 3)
metric = "ROC"
model_rf_new_features <- train(gender~., data=train, method="ranger", metric=metric, tuneGrid = tunegrid, trControl=control, verbose = TRUE)
print(model_rf_new3)

pred_rf = predict(model_rf_new_features, test)
confusionMatrix(pred_rf, test$gender)

saveRDS(model_rf_new_features, "Models/RF_Optimized.rds")

#write.csv(I_train_E, "TrainingDataForLGBM.csv", row.names = FALSE)
#write.csv(I_test_E, "TestDataForLGBM.csv", row.names = FALSE)

#write.csv(Dataset_Train, "TrainingDataForLGBM.csv", row.names = FALSE)
#write.csv(Dataset_Test, "TestDataForLGBM.csv", row.names = FALSE)
