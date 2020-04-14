
# Setting the current directory as working directory (Please update the code below if this fails)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading the test file
I_test = read.csv("TransformedTest.csv", stringsAsFactors = FALSE)

I_test$Day = as.factor(I_test$Day)
I_test$Time_of_Day = as.factor(I_test$Time_of_Day)

### XGB - Best

XGB_Base = readRDS("Models/XGB_Base.rds")

pred_xgb = predict(XGB_Base, I_test)

result = data.frame(session_id = I_test$session_id, gender = pred_xgb)

write.csv(result, "Submissions/submission_xgb.csv", row.names = FALSE)


### RF - Best

RF_Base = readRDS("Models/RF_Optimized.rds")

I_test$Day = as.factor(I_test$Day)
I_test$Time_of_Day = as.factor(I_test$Time_of_Day)

pred_rf = predict(RF_Base, data.frame(I_test))

result = data.frame(session_id = I_test$session_id, gender = pred_rf)

write.csv(result, "Submissions/submission_rf.csv", row.names = FALSE)
