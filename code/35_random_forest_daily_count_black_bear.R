# ============================================================
# 35_random_forest_daily_count_black_bear.R
#
# RANDOM FOREST DAILY-COUNT MODEL - BLACK BEAR
#
# PURPOSE:
#
# Model the number of independent black bear detections
# recorded during each camera-day.
#
# All complete camera-days are included, including days with
# zero detections.
#
# The model uses environmental, habitat, camera, and wildfire
# PM2.5 predictors.
#
#
# MODELING STRATEGY:
#
# 1. Split the data by camera site for predictive validation.
#
# 2. Fit a Random Forest count model to training sites.
#
# 3. Evaluate predictions on completely unseen camera sites.
#
# 4. Fit a second model using the complete dataset for
#    interpretation of predictor relationships.
#
# 5. Include camera site in the full interpretation model so
#    persistent differences among camera locations can be
#    represented.
#
#
# PM2.5 response interpretation will be performed later using
# ALE and permutation testing.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(ranger)



# ------------------------------------------------------------
# 2. Load cleaned black bear dataset
# ------------------------------------------------------------

bear <- read.csv(
  "data-split/black_bear_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

bear$Road_Type <- as.factor(
  bear$Road_Type
)

bear$Habitat_Type <- as.factor(
  bear$Habitat_Type
)

bear$Land_Mgnt <- as.factor(
  bear$Land_Mgnt
)

bear$Cell_ID <- as.factor(
  bear$Cell_ID
)



# ============================================================
# SELECT MODEL VARIABLES
# ============================================================



# ------------------------------------------------------------
# 4. Keep variables required for analysis
# ------------------------------------------------------------

bear.rf.data <- bear[
  ,
  c(
    "Cell_ID",
    "Daily_Count",
    "scale.smoke",
    "julian",
    "scale.temp",
    "Road_Type",
    "Cam_Height",
    "Distance_to_Road",
    "Habitat_Type",
    "Land_Mgnt",
    "elev",
    "wadnr"
  )
]



# ------------------------------------------------------------
# 5. Remove any remaining incomplete observations
# ------------------------------------------------------------

bear.rf.data <- bear.rf.data[
  complete.cases(
    bear.rf.data
  ),
]



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR DAILY-COUNT RANDOM FOREST\n"
)

cat(
  "====================================================\n"
)


cat(
  "Camera-days:",
  nrow(
    bear.rf.data
  ),
  "\n"
)


cat(
  "Camera sites:",
  length(
    unique(
      bear.rf.data$Cell_ID
    )
  ),
  "\n"
)


cat(
  "Zero-detection days:",
  sum(
    bear.rf.data$Daily_Count == 0
  ),
  "\n"
)


cat(
  "Positive-detection days:",
  sum(
    bear.rf.data$Daily_Count > 0
  ),
  "\n"
)


cat(
  "Total detections:",
  sum(
    bear.rf.data$Daily_Count
  ),
  "\n"
)


cat(
  "Mean daily count:",
  round(
    mean(
      bear.rf.data$Daily_Count
    ),
    6
  ),
  "\n"
)



# ============================================================
# GROUPED TRAIN / TEST SPLIT
# ============================================================



# ------------------------------------------------------------
# 6. Identify unique camera sites
# ------------------------------------------------------------

unique.sites <- unique(
  bear.rf.data$Cell_ID
)



# ------------------------------------------------------------
# 7. Create reproducible 80/20 site split
# ------------------------------------------------------------

set.seed(42)


training.sites <- sample(
  unique.sites,
  size = floor(
    0.80 *
      length(
        unique.sites
      )
  )
)


testing.sites <- setdiff(
  unique.sites,
  training.sites
)



# ------------------------------------------------------------
# 8. Create training and testing datasets
# ------------------------------------------------------------

train <- bear.rf.data[
  bear.rf.data$Cell_ID %in%
    training.sites,
]


test <- bear.rf.data[
  bear.rf.data$Cell_ID %in%
    testing.sites,
]



# ------------------------------------------------------------
# 9. Print grouped split information
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "GROUPED TRAIN / TEST SPLIT\n"
)

cat(
  "====================================================\n"
)


cat(
  "Training sites:",
  length(
    training.sites
  ),
  "\n"
)


cat(
  "Testing sites:",
  length(
    testing.sites
  ),
  "\n"
)


cat(
  "Training camera-days:",
  nrow(
    train
  ),
  "\n"
)


cat(
  "Testing camera-days:",
  nrow(
    test
  ),
  "\n"
)



# ============================================================
# PREDICTIVE VALIDATION MODEL
# ============================================================



# ------------------------------------------------------------
# 10. Prepare validation data
# ------------------------------------------------------------

train.model <- train

test.model <- test


train.model$Cell_ID <- NULL

test.model$Cell_ID <- NULL



# ------------------------------------------------------------
# 11. Fit daily-count Random Forest
# ------------------------------------------------------------

bear.daily.rf <- ranger(
  
  Daily_Count ~ .,
  
  data =
    train.model,
  
  num.trees =
    500,
  
  min.node.size =
    20,
  
  splitrule =
    "poisson",
  
  poisson.tau =
    1,
  
  importance =
    "permutation",
  
  respect.unordered.factors =
    "order",
  
  seed =
    42
)



# ------------------------------------------------------------
# 12. Print fitted validation model
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR DAILY-COUNT VALIDATION MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  bear.daily.rf
)



# ============================================================
# TEST-SET PREDICTIONS
# ============================================================



# ------------------------------------------------------------
# 13. Predict daily counts for unseen camera sites
# ------------------------------------------------------------

test.predictions <- predict(
  
  bear.daily.rf,
  
  data =
    test.model
  
)$predictions



# ------------------------------------------------------------
# 14. Store observed values
# ------------------------------------------------------------

test.observed <-
  test.model$Daily_Count



# ============================================================
# PREDICTIVE PERFORMANCE
# ============================================================



# ------------------------------------------------------------
# 15. Mean Absolute Error
# ------------------------------------------------------------

test.mae <- mean(
  abs(
    test.predictions -
      test.observed
  )
)



# ------------------------------------------------------------
# 16. Root Mean Squared Error
# ------------------------------------------------------------

test.rmse <- sqrt(
  mean(
    (
      test.predictions -
        test.observed
    )^2
  )
)



# ------------------------------------------------------------
# 17. Test R-squared
# ------------------------------------------------------------

test.sse <- sum(
  (
    test.observed -
      test.predictions
  )^2
)


test.sst <- sum(
  (
    test.observed -
      mean(
        test.observed
      )
  )^2
)


test.r2 <- 1 - (
  test.sse /
    test.sst
)



# ============================================================
# POISSON DEVIANCE
# ============================================================



# ------------------------------------------------------------
# 18. Create safe positive predictions
# ------------------------------------------------------------

epsilon <- 1e-10


test.predictions.safe <- pmax(
  test.predictions,
  epsilon
)



# ------------------------------------------------------------
# 19. Calculate observation-level Poisson deviance
# ------------------------------------------------------------

poisson.deviance <- ifelse(
  
  test.observed == 0,
  
  2 *
    test.predictions.safe,
  
  2 *
    (
      test.observed *
        log(
          test.observed /
            test.predictions.safe
        ) -
        (
          test.observed -
            test.predictions.safe
        )
    )
)



# ------------------------------------------------------------
# 20. Mean test Poisson deviance
# ------------------------------------------------------------

mean.poisson.deviance <- mean(
  poisson.deviance
)



# ------------------------------------------------------------
# 21. Create mean-count baseline
# ------------------------------------------------------------

baseline.prediction <- mean(
  train.model$Daily_Count
)


baseline.predictions <- rep(
  baseline.prediction,
  length(
    test.observed
  )
)


baseline.predictions.safe <- pmax(
  baseline.predictions,
  epsilon
)



# ------------------------------------------------------------
# 22. Calculate baseline Poisson deviance
# ------------------------------------------------------------

baseline.poisson.deviance <- ifelse(
  
  test.observed == 0,
  
  2 *
    baseline.predictions.safe,
  
  2 *
    (
      test.observed *
        log(
          test.observed /
            baseline.predictions.safe
        ) -
        (
          test.observed -
            baseline.predictions.safe
        )
    )
)


mean.baseline.deviance <- mean(
  baseline.poisson.deviance
)



# ------------------------------------------------------------
# 23. Print validation metrics
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "TEST-SET PERFORMANCE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Observed mean daily count:",
  round(
    mean(
      test.observed
    ),
    6
  ),
  "\n"
)


cat(
  "Predicted mean daily count:",
  round(
    mean(
      test.predictions
    ),
    6
  ),
  "\n"
)


cat(
  "MAE:",
  round(
    test.mae,
    6
  ),
  "\n"
)


cat(
  "RMSE:",
  round(
    test.rmse,
    6
  ),
  "\n"
)


cat(
  "Test R-squared:",
  round(
    test.r2,
    6
  ),
  "\n"
)


cat(
  "Mean Poisson deviance:",
  round(
    mean.poisson.deviance,
    6
  ),
  "\n"
)


cat(
  "Mean baseline Poisson deviance:",
  round(
    mean.baseline.deviance,
    6
  ),
  "\n"
)



# ============================================================
# VALIDATION-MODEL VARIABLE IMPORTANCE
# ============================================================



# ------------------------------------------------------------
# 24. Sort predictor importance
# ------------------------------------------------------------

validation.importance <- sort(
  
  bear.daily.rf$variable.importance,
  
  decreasing =
    TRUE
)



# ------------------------------------------------------------
# 25. Print importance
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "VALIDATION MODEL PERMUTATION IMPORTANCE\n"
)

cat(
  "====================================================\n"
)


print(
  validation.importance
)



# ------------------------------------------------------------
# 26. Print PM2.5 importance rank
# ------------------------------------------------------------

validation.smoke.rank <- which(
  names(
    validation.importance
  ) ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank:",
  validation.smoke.rank,
  "of",
  length(
    validation.importance
  ),
  "\n"
)



# ============================================================
# FULL-DATA INTERPRETATION MODEL
# ============================================================



# ------------------------------------------------------------
# 27. Prepare full interpretation data
# ------------------------------------------------------------

full.model.data <- bear.rf.data



# ------------------------------------------------------------
# 28. Fit full-data daily-count Random Forest
# ------------------------------------------------------------

bear.daily.rf.full <- ranger(
  
  Daily_Count ~ .,
  
  data =
    full.model.data,
  
  num.trees =
    500,
  
  min.node.size =
    20,
  
  splitrule =
    "poisson",
  
  poisson.tau =
    1,
  
  importance =
    "permutation",
  
  respect.unordered.factors =
    "order",
  
  seed =
    42
)



# ------------------------------------------------------------
# 29. Print full model
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL-DATA BLACK BEAR DAILY-COUNT MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  bear.daily.rf.full
)



# ============================================================
# FULL-DATA VARIABLE IMPORTANCE
# ============================================================



# ------------------------------------------------------------
# 30. Sort full-model importance
# ------------------------------------------------------------

full.importance <- sort(
  
  bear.daily.rf.full$variable.importance,
  
  decreasing =
    TRUE
)



# ------------------------------------------------------------
# 31. Print all predictor importance
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL MODEL PERMUTATION IMPORTANCE\n"
)

cat(
  "====================================================\n"
)


print(
  full.importance
)



# ------------------------------------------------------------
# 32. Calculate ecological-predictor importance
# ------------------------------------------------------------

ecological.importance <-
  full.importance[
    names(
      full.importance
    ) !=
      "Cell_ID"
  ]



# ------------------------------------------------------------
# 33. Print ecological-predictor importance
# ------------------------------------------------------------

cat(
  "\nEcological/camera predictor importance:\n"
)


print(
  ecological.importance
)



# ------------------------------------------------------------
# 34. Calculate PM2.5 importance rank
# ------------------------------------------------------------

full.smoke.rank <- which(
  names(
    ecological.importance
  ) ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank among ecological/camera predictors:",
  full.smoke.rank,
  "of",
  length(
    ecological.importance
  ),
  "\n"
)



# ============================================================
# SAVE VALIDATION RESULTS
# ============================================================



# ------------------------------------------------------------
# 35. Create results folder
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)



# ------------------------------------------------------------
# 36. Create performance results table
# ------------------------------------------------------------

performance.results <- data.frame(
  
  Species =
    "Black Bear",
  
  Training_Sites =
    length(
      training.sites
    ),
  
  Testing_Sites =
    length(
      testing.sites
    ),
  
  Training_Camera_Days =
    nrow(
      train
    ),
  
  Testing_Camera_Days =
    nrow(
      test
    ),
  
  Test_Mean_Observed =
    mean(
      test.observed
    ),
  
  Test_Mean_Predicted =
    mean(
      test.predictions
    ),
  
  MAE =
    test.mae,
  
  RMSE =
    test.rmse,
  
  Test_R2 =
    test.r2,
  
  Mean_Poisson_Deviance =
    mean.poisson.deviance,
  
  Baseline_Poisson_Deviance =
    mean.baseline.deviance,
  
  PM25_Validation_Importance_Rank =
    validation.smoke.rank,
  
  PM25_Full_Model_Importance_Rank =
    full.smoke.rank
)



# ------------------------------------------------------------
# 37. Save performance table
# ------------------------------------------------------------

write.csv(
  
  performance.results,
  
  "results/Black_Bear_Daily_Count_RF_Performance.csv",
  
  row.names =
    FALSE
)



# ------------------------------------------------------------
# 38. Save validation importance
# ------------------------------------------------------------

write.csv(
  
  data.frame(
    
    Predictor =
      names(
        validation.importance
      ),
    
    Permutation_Importance =
      as.numeric(
        validation.importance
      )
  ),
  
  "results/Black_Bear_Daily_Count_RF_Validation_Importance.csv",
  
  row.names =
    FALSE
)



# ------------------------------------------------------------
# 39. Save full-model ecological importance
# ------------------------------------------------------------

write.csv(
  
  data.frame(
    
    Predictor =
      names(
        ecological.importance
      ),
    
    Permutation_Importance =
      as.numeric(
        ecological.importance
      )
  ),
  
  "results/Black_Bear_Daily_Count_RF_Full_Model_Importance.csv",
  
  row.names =
    FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR DAILY-COUNT RANDOM FOREST SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "All camera-days modeled:",
  nrow(
    bear.rf.data
  ),
  "\n"
)


cat(
  "Test MAE:",
  round(
    test.mae,
    6
  ),
  "\n"
)


cat(
  "Test RMSE:",
  round(
    test.rmse,
    6
  ),
  "\n"
)


cat(
  "Test R-squared:",
  round(
    test.r2,
    6
  ),
  "\n"
)


cat(
  "RF mean Poisson deviance:",
  round(
    mean.poisson.deviance,
    6
  ),
  "\n"
)


cat(
  "Baseline mean Poisson deviance:",
  round(
    mean.baseline.deviance,
    6
  ),
  "\n"
)


cat(
  "PM2.5 validation importance rank:",
  validation.smoke.rank,
  "of",
  length(
    validation.importance
  ),
  "\n"
)


cat(
  "PM2.5 full-model importance rank:",
  full.smoke.rank,
  "of",
  length(
    ecological.importance
  ),
  "\n"
)


cat(
  "====================================================\n"
)