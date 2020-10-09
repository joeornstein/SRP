#Functions to estimate stacked regression models

#Version 1.1
#Last Updated by Joe Ornstein (October 9, 2020)


#' Generate Stack Weights
#' @description  Main Stacking Procedure: Construct k folds and generate predictions from base models
#'
#' @param trainset The training dataset
#' @param binaryDepVar A boolean denoting whether the outcome variable is binary or continuous. Used to specify tuning parameters for machine learning methods.
#' @param hlmFormula A formula object for the hierarchical linear model
#' @param lasso_vars The predictor variables to include in the LASSO model
#' @param lasso_factors Which predictor variables need to be converted to factors before estimating the LASSO model
#' @param forestFormula A formula object for the random forest model
#' @param mns_best Optional min node size parameter for random forest model
#' @param knnFormula A formula object for the KNN model
#' @param k_best Optimal k parameter for KNN model
#' @param gbm_vars The predictor variables to include in GBM model
#' @param gbm_factors Which predictor variables need to be converted to factors before estimating the GBM
#' @param gbm_params List of parameters for the GBM model
#' @param gbm_tune xgb.tune object, containing the $best.iteration
#' @param nfolds Number of cross-validation folds
#' @return The optimal stacking weights (HLM, LASSO, KNN, Random Forest, GBM)
#' @note trainset must include a variable called 'y', denoting the outcome
getStackWeights <- function(trainset, binaryDepVar = F,
                     hlmFormula,
                     lasso_vars, lasso_factors = NULL,
                     forestFormula, mns_best = NULL,
                     knnFormula, k_best,
                     gbm_vars, gbm_factors = NULL, gbm_params, gbm_tune,
                     nfolds = 5){

  #1. Partition the data into n folds
  trainset$fold <- createFolds(trainset[,1] %>% unlist, k=nfolds, list=F, returnTrain = F)

  #2. Create train_meta dataframe to hold OOF predictions
  train_meta <- trainset[0,]

  #3. For each fold:
  for (i in 1:nfolds){

    #Split data
    trainfold <- trainset[trainset$fold != i,]
    testfold <- trainset[trainset$fold == i,]

    #Clean LASSO variables
    if(sum(is.na(lasso_vars))==0){

      cleaned_lasso <- cleanDataLASSO(trainset = trainfold,
                                      testset = testfold,
                                      lasso_vars = lasso_vars,
                                      lasso_factors = lasso_factors)

      trainfold_lasso <- cleaned_lasso$trainset
      testfold_lasso <- cleaned_lasso$testset
    }

    #Clean GBM variables
    if(sum(is.na(gbm_vars))==0){
      cleaned_gbm <- cleanDataGBM(trainset = trainfold,
                                  testset = testfold,
                                  gbm_vars = gbm_vars,
                                  gbm_factors = gbm_factors)

      trainfold_gbm <- cleaned_gbm$trainset
      testfold_gbm <- cleaned_gbm$testset
    }


    #3a. Fit base models to training fold

    #HLM
    hlmFold <- lmer(hlmFormula, data = trainfold)

    #LASSO
    if(binaryDepVar){
      lassoCVFold <- cv.glmnet(trainfold_lasso, trainfold$y,
                               family = "binomial", type.measure = "deviance")
    }else{
      lassoCVFold <- cv.glmnet(trainfold_lasso, trainfold$y, type.measure = "mse")
    }

    #Random Forest
    forestFold <- ranger(forestFormula, data = trainfold,
                         min.node.size = mns_best)

    #GBM
    dtrain <-xgb.DMatrix(trainfold_gbm, label = trainfold$y) #Create a custom 'xgb.DMatrix'. Faster computation
    objective <- 'reg:linear'
    eval_metric <- 'rmse'
    if(binaryDepVar){
      objective <- 'binary:logistic'
      eval_metric <- 'logloss'
    }

    gbmFold <- xgboost(params = gbm_params,
                       data = dtrain,
                       booster = "gbtree",
                       objective = objective,
                       eval_metric = eval_metric,
                       nrounds = gbm_tune$best_iteration,
                       verbose = F)


    #3b. Make predictions on test fold
    if(binaryDepVar){
      testfold$M1 <- predict(hlmFold, testfold, type = "response", allow.new.levels = T)
      testfold$M2 <- predict(lassoCVFold, newx = testfold_lasso,
                             s = lassoCVFold$lambda.min, type = "response")[,1]
      testfold$M3 <- kknn(knnFormula, train = trainfold, test = testfold, k = k_best)$fitted.values
      testfold$M4 <- predict(forestFold, testfold)$predictions
      testfold$M5 <- predict(gbmFold, testfold_gbm)
    }else{
      testfold$M1 <- predict(hlmFold, testfold, allow.new.levels = T)
      testfold$M2 <- predict(lassoCVFold, newx = testfold_lasso, s = lassoCVFold$lambda.min)
      testfold$M3 <- kknn(knnFormula, train = trainfold, test = testfold, k = k_best)$fitted.values
      testfold$M4 <- predict(forestFold, testfold)$predictions
      testfold$M5 <- predict(gbmFold, testfold_gbm)
    }

    #3c. Store in train_meta
    train_meta %<>% bind_rows(testfold)
  }

  #4. Use quadratic programming algorithm (continuous depvar) or hill climbing (binary depvar)
  #   to generate stack weights
  M <- train_meta %>% dplyr::select(M1,M2,M3,M4,M5) %>% as.matrix
  if(binaryDepVar){
    stackWeights <- getHillClimbWeights(Y = train_meta$y, M = M)
  }else{
    stackWeights <- regress.func(Y = train_meta$y, preds.var = M)
  }

  return(stackWeights)
}



#' Quadratic Programming Algorithm
#' @description   Quadratic programming solution to stacking problem from Grimmer, Messing & Westwood (2017)
#'
#' @param Y The outcome variable
#' @param preds.var An n x k matrix of out-of-fold predictions of Y, where n is the length of Y, and k is th number of models
#' @return The optimal stacking weights
regress.func <- function(Y, preds.var){

  # need to smartly figure out which columns are not NA
  orgcols <- length(preds.var[1,])
  notNA <- which(!is.na(preds.var[1,]))
  predX <- preds.var[,notNA ]

  library(quadprog)
  d.mat <- solve(chol(t(predX)%*%predX))
  a.mat <- cbind(rep(1, ncol(predX)), diag(ncol(predX)))
  b.vec <- c(1, rep(0, ncol(predX)))
  d.vec <- t(Y) %*% predX
  out<- solve.QP(Dmat = d.mat, factorized =TRUE, dvec = d.vec, Amat = a.mat, bvec = b.vec, meq = 1)
  coefs <- rep(NA, orgcols)
  notDel <- c(1:orgcols)[notNA]#[notCor]
  coefs[notDel] <- out$solution
  return(coefs)
}

#' Hill Climbing Algorithm
#'
#' @description Hill Climbing solution to stacking problem from Caruana et al (2004); use for minimizing LogLoss
#'
#' @param Y The outcome variable
#' @param M An n x k matrix of out-of-fold predictions of Y, where n is the length of Y, and k is th number of models
#' @param verbose
#' @return The optimal stacking weights
getHillClimbWeights <- function(Y, M, verbose = T){

  hillClimbingWeights <- rep(1,ncol(M))

  hillClimbingPredictions <- M %*% hillClimbingWeights / sum(hillClimbingWeights)
  currentLogLoss <- LogLossBinary(actual = Y, predicted = hillClimbingPredictions)
  bestLogLoss <- currentLogLoss
  keepGoing <- T

  while(keepGoing){
    #Keep track of which model would be the best 'greedy' addition to the ensemble
    bestAddition <- 0
    for (j in 1:length(hillClimbingWeights)){

      hillClimbingWeights[j] <- hillClimbingWeights[j] + 1
      newPredictions <- M %*% hillClimbingWeights / sum(hillClimbingWeights)
      newLogLoss <- LogLossBinary(actual = Y, predicted = newPredictions)
      if (newLogLoss < bestLogLoss){
        bestLogLoss <- newLogLoss
        bestAddition <- j
      }
      hillClimbingWeights[j] <- hillClimbingWeights[j] - 1
    }
    #If we found an improvement (and we're below the cutoff number of iterations), then add the best model
    if(sum(hillClimbingWeights) < 1000 & bestLogLoss < currentLogLoss){
      hillClimbingWeights[bestAddition] <- hillClimbingWeights[bestAddition] + 1
      currentLogLoss <- bestLogLoss
    } else{
      #If not, break
      keepGoing <- F
    }
    if(verbose){
      print(paste0("Iteration: ",
                   sum(hillClimbingWeights) - 6,
                   ", LogLoss = ",
                   round(currentLogLoss, 4)))
    }
  }

  #Normalize the weights and return
  hillClimbingWeights %>%
    divide_by(sum(hillClimbingWeights)) %>%
    return
}

#LASSO and GBM need cleaned versions of the dataset


#' Clean LASSO data
#'
#' @description Cleans the training data for glmnet, which requires a model matrix instead of a formula
#'
#' @param trainset The training dataset
#' @param lasso_vars Predictor variables to use in the LASSO model
#' @param lasso_factors Which predictor variables need to be converted to factors
#' @param testset Optional test set; included so that both cleaned versions will have the same dimensions
#' @param new_vars_lasso Optional variable names to include as input
#' @return A list, containing cleaned versions of the trainset and testset
cleanDataLASSO <- function(trainset, lasso_vars, lasso_factors = NULL, testset = NULL, new_vars_lasso = NULL){

  #NOTE: takes trainset and testset to ensure that both matrices have the same dimensions.
  #If no testset is provided, defaults to outputting duplicate matrices
  if(is.null(testset)){
    testset <- trainset
  }


  #Convert categorical vars to factors
  if(!is.null(lasso_factors)){
    for(i in lasso_factors){
      trainset[,i] <- trainset[,i] %>% unlist %>% factor
      testset[,i] <- testset[,i] %>% unlist %>% factor
    }
  }

  treatplan_lasso <- vtreat::designTreatmentsZ(trainset, lasso_vars, verbose = FALSE)

  #Option to provide new variable names as input; otherwise generate them from treatplan_lasso
  if(is.null(new_vars_lasso)){
    new_vars_lasso <- treatplan_lasso$scoreFrame %>%
      filter(code %in% c("clean", "lev")) %>%
      pull(varName)
  }


  trainset %<>%
    vtreat::prepare(treatplan_lasso, ., varRestriction = new_vars_lasso) %>%
    as.matrix

  testset %<>%
    vtreat::prepare(treatplan_lasso, ., varRestriction = new_vars_lasso) %>%
    as.matrix

  return(list(trainset=trainset, testset=testset))
}


#' Clean GBM Data
#' @description Cleans the training data for xgboost, which requires a model matrix instead of a formula
#'
#' @param trainset The training dataset
#' @param gbm_vars Predictor variables to use in the GBM model
#' @param gbm_factors Which predictor variables need to be converted to factors
#' @param testset Optional test set; included so that both cleaned versions will have the same dimensions
#' @param new_vars_gbm Optional variable names to include as input
#' @return A list, containing cleaned versions of the trainset and testset
cleanDataGBM <- function(trainset, gbm_vars, gbm_factors = NULL, testset = NULL, new_vars_gbm = NULL){

  #NOTE: takes trainset and testset to ensure that both matrices have the same dimensions.
  #If no testset is provided, defaults to outputting duplicate matrices
  if(is.null(testset)){
    testset <- trainset
  }

  #Convert categorical vars to factors
  if(!is.null(gbm_factors)){
    for(i in gbm_factors){
      trainset[,i] <- trainset[,i] %>% unlist %>% factor
      testset[,i] <- testset[,i] %>% unlist %>% factor
    }
  }


  treatplan_gbm <- vtreat::designTreatmentsZ(trainset, gbm_vars, verbose = FALSE)

  #Option to provide new variable names as input; otherwise generate them from treatplan_gbm
  if(is.null(new_vars_gbm)){
    new_vars_gbm <- treatplan_gbm$scoreFrame %>%
      filter(code %in% c("clean", "lev")) %>%
      pull(varName)
  }

  #Prepare the training trainseta
  trainset <- vtreat::prepare(treatplan_gbm, trainset, varRestriction = new_vars_gbm) %>%
    as.matrix

  testset <- vtreat::prepare(treatplan_gbm, testset, varRestriction = new_vars_gbm) %>%
    as.matrix

  return(list(trainset=trainset, testset=testset))
}

#' Compute Log Loss
#' @description Computes the log loss measure, given predicted and actual values
#'
#' @param actual A vector of actual values
#' @param predicted A vector of predicted values
#' @param eps A very small number
#' @return Log Loss
LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}
