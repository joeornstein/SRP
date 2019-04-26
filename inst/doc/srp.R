## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tidyverse)
library(magrittr)

## ----installation, message = FALSE---------------------------------------
devtools::install_github('joeornstein/SRP')
library(SRP)

## ----data, message=FALSE-------------------------------------------------
trainset <- SRP::vignetteData

trainset

## ----disag, message=FALSE------------------------------------------------
disag_estimates <- trainset %>% 
  group_by(unit) %>% 
  summarise(disag_estimate = mean(y),
            num = n())

disag_estimates

## ----multilevel regression, message=FALSE--------------------------------
library(lme4)

model1 <- lmer(y ~ (1|x1) + (1|x2) + unit_covariate + (1|unit), data = trainset)

## ----PSFrame, message=FALSE----------------------------------------------
PSFrame <- SRP::vignettePSFrame

PSFrame

## ----poststratification1, message=FALSE----------------------------------
pred <- predict(model1, PSFrame, allow.new.levels = T)

mrp_estimates <- poststratify(pred, PSFrame)

mrp_estimates

## ----machine learning, message=FALSE-------------------------------------
library(glmnet)
library(ranger)
library(kknn)
library(xgboost)
library(caret)


#Estimate HLM
hlmFormula <- y ~ (1|x1) +  (1|x2) + unit_covariate + (1|unit) 
hlmModel <- lmer(hlmFormula, data = trainset)

#Tune LASSO
lasso_vars <- c("x1","x2","unit","unit_covariate")
lasso_factors <- c('x1', 'x2', 'unit') #which variables to convert to factors
trainset_lasso <- cleanDataLASSO(trainset, lasso_vars, lasso_factors)$trainset

lassoModel <- cv.glmnet(trainset_lasso, trainset$y, 
                        type.measure = "mse") 

#Tune KNN
knnFormula <- y ~ x1 + x2 + latitude + longitude + unit_covariate
knn_train <- train.kknn(knnFormula, data=trainset, kmax = 201) #Find best k (LOOCV)
k_best <- knn_train$best.parameters$k

#Tune Random Forest
forestFormula <- y ~ x1 + x2 + latitude + longitude + unit_covariate
forestModel <- ranger(forestFormula, data = trainset)

#Tune GBM
gbm_vars <- c("x1","x2","latitude","longitude","unit_covariate")
trainset_gbm <- cleanDataGBM(trainset=trainset, gbm_vars=gbm_vars)$trainset
#Create a custom 'xgb.DMatrix'. Faster computation
dtrain <- xgb.DMatrix(trainset_gbm, label = trainset$y)

#5-fold cross-validation; pick nrounds that minimizes RMSE
xgb.tune <- xgb.cv(data = dtrain, 
                   booster = "gbtree",
                   objective = "reg:linear",
                   eval_metric = "rmse",
                   eta = 0.02,
                   nrounds = 50 / 0.02, #Lower eta -> more trees
                   nfold = 5, 
                   verbose = F,
                   early_stopping_rounds = 20)

gbmModel <- xgboost(data = dtrain, 
                    booster = "gbtree",
                    objective = "reg:linear",
                    eval_metric = "rmse",
                    eta = 0.02,
                    verbose = F,
                    nrounds = xgb.tune$best_iteration)

## ----stacking, message=FALSE---------------------------------------------
stackWeights <- getStackWeights(trainset = trainset,
                                hlmFormula = hlmFormula,
                                lasso_vars = lasso_vars,
                                lasso_factors = lasso_factors,
                                forestFormula = forestFormula,
                                knnFormula = knnFormula, k_best = k_best,
                                gbm_vars = gbm_vars, gbm_factors = NULL, 
                                gbm_params = list(eta = 0.02), gbm_tune = xgb.tune, 
                                nfolds = 5)

stackWeights %>% round(3)

## ----poststratification2, message=FALSE----------------------------------
PSFrame_lasso <- cleanDataLASSO(PSFrame, lasso_vars = lasso_vars, lasso_factors = lasso_factors,
                                new_vars_lasso = colnames(trainset_lasso))$trainset
PSFrame_gbm <- cleanDataGBM(PSFrame, gbm_vars = gbm_vars)$trainset

M1 <- predict(hlmModel, PSFrame, allow.new.levels = T)
M2 <- predict(lassoModel, newx = PSFrame_lasso, s = lassoModel$lambda.min)
M3 <- kknn(knnFormula, train = trainset, test = PSFrame, k = k_best)$fitted.values
M4 <- predict(forestModel, PSFrame)$predictions
M5 <- predict(gbmModel, PSFrame_gbm)
M <- cbind(M1,M2,M3,M4,M5) %>% as.matrix

pred <- M %*% stackWeights

#Poststratify
srp_estimates <- poststratify(pred, PSFrame)

head(srp_estimates)

## ----plots, echo=FALSE, message=FALSE------------------------------------
true_means <- SRP::vignetteTruth

mrp_estimates %<>% set_colnames(c('unit','mrp_estimate'))
srp_estimates %<>% set_colnames(c('unit','srp_estimate'))

results <- true_means %>%
  left_join(disag_estimates, by = 'unit') %>%
  left_join(mrp_estimates, by = 'unit') %>%
  left_join(srp_estimates, by = 'unit')

ggplot(results) + geom_point(aes(x=disag_estimate, y=true_mean)) + 
  xlab('Disaggregated Estimate') + ylab('True Mean') + 
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

ggplot(results) + geom_point(aes(x=mrp_estimate, y=true_mean)) + 
  xlab('MRP Estimate') + ylab('True Mean') + 
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

ggplot(results) + geom_point(aes(x=srp_estimate, y=true_mean)) + 
  xlab('SRP Estimate') + ylab('True Mean') + 
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()


## ----PSFrames, echo=TRUE, message=FALSE----------------------------------
PSFrame1 <- SRP::race
PSFrame2 <- SRP::education

PSFrame1
PSFrame2

## ----getSynthetic, echo=TRUE, message=FALSE------------------------------
PSFrame <- getSyntheticPSFrame(PSFrame1, PSFrame2)

PSFrame

## ----getSynthetic2, echo=TRUE, message=FALSE-----------------------------
PSFrame3 <- SRP::sex

PSFrame3

PSFrame <- getSyntheticPSFrame(PSFrame, PSFrame3)

PSFrame

