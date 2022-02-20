# Original equation:
#sale_price~1+meta_class+meta_town_code+meta_nbhd+meta_certified_est_bldg+meta_certified_est_land
#+factor(meta_cdu)+ +factor(meta_deed_type)+char_hd_sf+char_age+char_apts+char_roof_cnst+char_rooms+char_beds
#+char_bsmt+char_bsmt_fin+char_oheat+char_frpl+char_fbath+char_hbath+char_tp_plan+char_tp_dsgn+char_cnst_qlty
#+char_site+char_gar1_att+char_bldg_sf+char_use+char_type_resd+char_attic_fnsh+char_renovation+factor(geo_property_city)
#+factor(geo_fips)+factor(geo_municipality)+geo_withinmr100+geo_withinmr101300+factor(geo_school_elem_district)
#+factor(geo_school_hs_district)+econ_tax_rate+econ_midincome+ind_garage+factor(ind_arms_length)


# Final Project - FIN510
# Prof: D. Molitor and X. Gao
# Team: I. Abarca, J. Fu, A. Kassym-Ashim
# Full run takes around 20 minutes (ram memory 32gb, AMD CPU 5800x, windows 10)

### Load packages
library(tidyverse)
library(dplyr)
library(glmnet) 
library(gbm)
library(boot)
library(randomForest)
library(fBasics)

### Load data (2 databases), check the folder if needed

# Database to process (historic_property_data)
data <- read.csv("C:\\Users\\Jason\\Desktop\\Final V2\\historic_property_data1.csv")
head(data)
names(data)

# Database that we need to predict - change file if needed
data_to_predict <- read.csv("C:\\Users\\Jason\\Desktop\\Final V2\\predict_property_data1.csv")
head(data_to_predict)
names(data_to_predict)


### Exploration

data[is.na(data)] <- 0
sum(is.na(data)) # We validate no missing values

# Reducing levels for factor variables

# We group the variables with less than 500 observations in geo_property_city to "Distinct"
geo_prop_city <- data %>%group_by(geo_property_city) %>% summarize(number=n())
# Number of observations of geo_property_city (before = 133)
length(unique(data[["geo_property_city"]]))
change_city<-geo_prop_city$geo_property_city[geo_prop_city$number<500]
data$geo_property_city[data$geo_property_city %in% change_city] <- "Distinct"
# Number of observations of geo_property_city (after = 20)
length(unique(data[["geo_property_city"]]))

# We group the variables with less than 500 observations in geo_property_zip to "Distinct"
geo_prop_zip <- data %>% group_by(geo_property_zip) %>% summarize(number=n())
# Number of observatinons of geo_property_zip (before = 168)
length(unique(data[["geo_property_zip"]]))
change_zip <- geo_prop_zip$geo_property_zip[geo_prop_zip$number<500]
data$geo_property_zip[data$geo_property_zip %in% change_zip] <- "Distinct"
# Number of observatinons of geo_property_zip (after = 28)
length(unique(data[["geo_property_zip"]]))

# Group the variables with less than 500 observations in geo_fips to "Distinct"
geo_prop_fips <- data %>% group_by(geo_fips) %>% summarize(number=n())
change_fips <- geo_prop_fips$geo_fips[geo_prop_fips$number<500]
# Number of observations of geo_fips (before = 128)
length(unique(data[["geo_fips"]]))
data$geo_fips[data$geo_fips %in% change_fips] <- "Distinct"
# Number of observations of geo_fips (before = 21)
length(unique(data[["geo_fips"]]))

# We will explore with all variables to select the significative ones
lm.full1 <- lm(sale_price ~ ., data = data)
summary(lm.full1)

# We will work with only selected variables (significative ones)
variables_selected <- summary(lm.full1)$coefficients[-1,4] < 0.05
variables_selected <- names(summary(lm.full1)$coefficients[-1,4])[variables_selected == TRUE]
variables_selected #list of selected variables

### Prediction
### Prediction 1: Stepwise model

# We run a linear model with all variables

# lm.full2 <- lm(sale_price~1+factor(meta_class)+factor(meta_town_code)+factor(meta_nbhd)
#                +meta_certified_est_bldg+meta_certified_est_land
#                +char_hd_sf+char_age+factor(char_apts)+factor(char_roof_cnst)+char_rooms
#                +char_beds+factor(char_bsmt)+factor(char_bsmt_fin)+factor(char_oheat)+char_frpl
#                +char_fbath+char_hbath+factor(char_tp_plan)+factor(char_tp_dsgn)+factor(char_cnst_qlty)
#                +factor(char_site)+factor(char_gar1_att)+char_bldg_sf+factor(char_use)
#                +factor(char_type_resd)+factor(char_attic_fnsh)+factor(char_renovation)
#                +factor(geo_property_city)+factor(geo_fips)+factor(geo_municipality)
#                +factor(geo_withinmr100)+factor(geo_withinmr101300)+factor(geo_school_elem_district)
#                +factor(geo_school_hs_district)+econ_tax_rate+econ_midincome+factor(ind_garage)
#                +factor(ind_arms_length), data=data)
# 
# summary(lm.full2)
# 
# # row numbers of the training set (90% from GitHub recommendation)
# dim(data)[1]*0.9
# 
# # Create train set
# set.seed(1)
# train.index <- sample(c(1:dim(data)[1]), dim(data)[1]*0.9) 
# train.df <- data[train.index,]
# head(train.df)
# 
# # Create test set 
# test.df <- data[-train.index,]
# head(test.df)
# 
# # use step() to run stepwise regression  
# lm.step.both <- step(lm.full2, direction = "both") # takes time
# 
# # summary table 
# summary(lm.step.both) 
# 
# # make predictions on the test set
# lm.step.pred.both <- predict(lm.step.both, test.df)
# #head(lm.step.pred.both)
# 
# # MSE in the test set 
# stepwise_mse<-mean((test.df$sale_price-lm.step.pred.both)^2)
# stepwise_mse


### Prediction 2: Lasso regression

# set.seed(2023)
# n_rows <- nrow(data)
# train <- sample(n_rows, 0.5*n_rows)
# 
# # We create train and test data
# train_data <- data[train,]
# test_data <- data[-train,]
# 
# x <- model.matrix(sale_price~1+factor(meta_class)+factor(meta_town_code)+factor(meta_nbhd)
#                   +meta_certified_est_bldg+meta_certified_est_land
#                   +char_hd_sf+char_age+factor(char_apts)+factor(char_roof_cnst)+char_rooms
#                   +char_beds+factor(char_bsmt)+factor(char_bsmt_fin)+factor(char_oheat)+char_frpl
#                   +char_fbath+char_hbath+factor(char_tp_plan)+factor(char_tp_dsgn)+factor(char_cnst_qlty)
#                   +factor(char_site)+factor(char_gar1_att)+char_bldg_sf+factor(char_use)
#                   +factor(char_type_resd)+factor(char_attic_fnsh)+factor(char_renovation)
#                   +factor(geo_property_city)+factor(geo_fips)+factor(geo_municipality)
#                   +factor(geo_withinmr100)+factor(geo_withinmr101300)+factor(geo_school_elem_district)
#                   +factor(geo_school_hs_district)+econ_tax_rate+econ_midincome+factor(ind_garage)
#                   +factor(ind_arms_length), data=train_data)
# y <- train_data$sale_price
# 
# lasso_model <- glmnet(x=x, y=y, alpha=1)
# lasso_model$lambda
# plot(lasso_model, xvar="lambda")
# 
# cv.lasso_model <- cv.glmnet(x,y,alpha=1, type.measure="mse", nfold=5)
# coef_lambda_best<- predict(cv.lasso_model,s=cv.lasso_model$lambda.min,type="coefficients")
# 
# model_lasso_test <- model.matrix(sale_price~1+factor(meta_class)+factor(meta_town_code)+factor(meta_nbhd)
#                                  +meta_certified_est_bldg+meta_certified_est_land
#                                  +char_hd_sf+char_age+factor(char_apts)+factor(char_roof_cnst)+char_rooms
#                                  +char_beds+factor(char_bsmt)+factor(char_bsmt_fin)+factor(char_oheat)+char_frpl
#                                  +char_fbath+char_hbath+factor(char_tp_plan)+factor(char_tp_dsgn)+factor(char_cnst_qlty)
#                                  +factor(char_site)+factor(char_gar1_att)+char_bldg_sf+factor(char_use)
#                                  +factor(char_type_resd)+factor(char_attic_fnsh)+factor(char_renovation)
#                                  +factor(geo_property_city)+factor(geo_fips)+factor(geo_municipality)
#                                  +factor(geo_withinmr100)+factor(geo_withinmr101300)+factor(geo_school_elem_district)
#                                  +factor(geo_school_hs_district)+econ_tax_rate+econ_midincome+factor(ind_garage)
#                                  +factor(ind_arms_length), data=test_data)
# 
# pred.lambda.best <- predict(cv.lasso_model,s=cv.lasso_model$lambda.min,newx=model_lasso_test)
# lasso_y_estimate <- predict(lasso_model, newx = model_lasso_test)
# lasso_mse <- mean((test_data$sale_price - lasso_y_estimate)^2)

### Prediction 3: Random Forest

data_temp <- data
set.seed(1)
n_rows <- nrow(data_temp)
train2 <- sample(n_rows, 0.9*n_rows)
train_data2 <- data_temp[train2,]
test_data2 <- data_temp[-train2,]

set.seed(1)
rf_model <- randomForest(sale_price~1+meta_class+meta_town_code+meta_nbhd+meta_certified_est_bldg+meta_certified_est_land
                         +meta_cdu +meta_deed_type+char_hd_sf+char_age+char_apts+char_roof_cnst+char_rooms+char_beds
                         +char_bsmt+char_bsmt_fin+char_oheat+char_frpl+char_fbath+char_hbath+char_tp_plan+char_tp_dsgn+char_cnst_qlty
                         +char_site+char_gar1_att+char_bldg_sf+char_use+char_type_resd+char_attic_fnsh+char_renovation+geo_property_city
                         +geo_fips+geo_municipality+geo_withinmr100+geo_withinmr101300+geo_school_elem_district
                         +geo_school_hs_district+econ_tax_rate+econ_midincome+ind_garage+ind_arms_length, data = train_data2, mtry=8, importance=TRUE, ntree=250) #medium tree (ntree = 250)
rf_y_estimate <- predict(rf_model,newdata = test_data2)
rf_mse <- mean((test_data2$sale_price - rf_y_estimate)^2)


### Prediction 4: Boosting

set.seed(1)
boosting_model <- gbm(sale_price~1+factor(meta_class)+factor(meta_town_code)+factor(meta_nbhd)
                      +meta_certified_est_bldg+meta_certified_est_land
                      +char_hd_sf+char_age+factor(char_apts)+factor(char_roof_cnst)+char_rooms
                      +char_beds+factor(char_bsmt)+factor(char_bsmt_fin)+factor(char_oheat)+char_frpl
                      +char_fbath+char_hbath+factor(char_tp_plan)+factor(char_tp_dsgn)+factor(char_cnst_qlty)
                      +factor(char_site)+factor(char_gar1_att)+char_bldg_sf+factor(char_use)
                      +factor(char_type_resd)+factor(char_attic_fnsh)+factor(char_renovation)
                      +factor(geo_property_city)+factor(geo_fips)+factor(geo_municipality)
                      +factor(geo_withinmr100)+factor(geo_withinmr101300)+factor(geo_school_elem_district)
                      +factor(geo_school_hs_district)+econ_tax_rate+econ_midincome+factor(ind_garage)
                      +factor(ind_arms_length), data=train_data, distribution ="gaussian", interaction.depth = 4, n.trees = 100) #medium tree (n.trees = 100)
boosting_y_estimate <- predict(boosting_model,newdata=test_data)
boosting_mse <- mean((test_data$sale_price - boosting_y_estimate)^2)

# We will compare all the MSE from the previous 4 models and pick the smallest
stepwise_mse
lasso_mse
rf_mse
boosting_mse

### We pick the model: rf

# The final model is trained in all the sample in data
rf_model_2 <- randomForest(sale_price~1+meta_class+meta_town_code+meta_nbhd+meta_certified_est_bldg+meta_certified_est_land
                           +meta_cdu +meta_deed_type+char_hd_sf+char_age+char_apts+char_roof_cnst+char_rooms+char_beds
                           +char_bsmt+char_bsmt_fin+char_oheat+char_frpl+char_fbath+char_hbath+char_tp_plan+char_tp_dsgn+char_cnst_qlty
                           +char_site+char_gar1_att+char_bldg_sf+char_use+char_type_resd+char_attic_fnsh+char_renovation+geo_property_city
                           +geo_fips+geo_municipality+geo_withinmr100+geo_withinmr101300+geo_school_elem_district
                           +geo_school_hs_district+econ_tax_rate+econ_midincome+ind_garage+ind_arms_length, data = data, mtry=8, importance=TRUE,ntree=250)




# Now we work with  data_to_predict 

# We change all the NA value to 0
data_to_predict[is.na(data_to_predict)] <- 0

# We will create the levels of factor variables of predict_property_data to be the same as that we used for training the model
for (j in (1:length(data_to_predict[,1]))){
  if (any(data_to_predict["geo_property_city"][j,]==data$geo_property_city)==TRUE){
    data_to_predict["geo_property_city"][j,] <-  "Distinct"
  }
}

for (j in (1:length(data_to_predict[,1]))){
  if (any(data_to_predict["geo_property_zip"][j,]==data$geo_property_zip)==TRUE){
    data_to_predict["geo_property_zip"][j,] <-  "Distinct"
  }
}

for (j in (1:length(data_to_predict[,1]))){
  if (any(data_to_predict["geo_fips"][j,]==data$geo_fips)==TRUE){
    data_to_predict["geo_fips"][j,] <-  "Distinct"
  }
}

# Predict the outcome
pred <- predict(rf_model_2, data_to_predict)
head(pred)

# Reports on the random forest
round(importance(rf_model_2),1)

# Create the output the result to csv file
pred_result <- data.frame(pid=data_to_predict$pid, assessed_value=pred)
head(pred_result)

# Export predicting results
write.csv(pred_result, "C:\\Users\\Jason\\Desktop\\Final V2\\assessed_value2.csv", row.names = FALSE)

# Basic stats
options(scipen = 100)
round(basicStats(pred_result$assessed_value),1)
plot(pred_result$assessed_value/1000000,xlab="pid",ylab="Market value (USD million)", main="Prediction (USD million)")
hist(pred_result$assessed_value/1000000,xlab="pid",ylab="Market value (USD million)", main="Prediction (USD million)", breaks=60)