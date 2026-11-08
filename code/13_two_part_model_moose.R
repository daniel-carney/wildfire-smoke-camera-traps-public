# ============================================================
# 13_two_part_model_moose.R
#
# VERSION 2 MODELING PIPELINE - MOOSE
#
# PURPOSE:
#
# Build a TWO-PART Random Forest model for moose.
#
# We use the same modeling framework that was finalized for
# mule deer so results can be compared consistently across
# species.
#
#
# PART 1:
#
#   Model whether moose are detected at all.
#
#       0 = no moose detections
#       1 = at least one moose detection
#
#   This is a Random Forest CLASSIFICATION model.
#
#
# PART 2:
#
#   Among camera-days where moose WERE detected,
#   model the number of detections.
#
#   This is a Random Forest REGRESSION model.
#
#
# WHY USE TWO MODELS?
#
# Moose camera-trap data contain a very large number of
# zero-detection days.
#
# Instead of making one model explain both:
#
#     whether moose are detected
#
# AND
#
#     how many detections occur when moose are detected
#
# we model those two processes separately.
#
#
# IMPORTANT:
#
# This script evaluates whether the two Random Forest models
# work correctly.
#
# PM2.5 interpretation using refined ALE and permutation
# testing comes in later scripts.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(ranger)



# ------------------------------------------------------------
# 2. Load cleaned moose dataset
# ------------------------------------------------------------
#
# This dataset should already have been created by the moose
# preprocessing script.

moose <- read.csv(
  "data-split/moose_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# Factor information is lost when data are saved to CSV,
# so we restore it after loading.

moose$Year <- as.factor(
  moose$Year
)

moose$landcover <- as.factor(
  moose$landcover
)

moose$Habitat_Type <- as.factor(
  moose$Habitat_Type
)

moose$Road_Type <- as.factor(
  moose$Road_Type
)

moose$Land_Mgnt <- as.factor(
  moose$Land_Mgnt
)

moose$conifer <- as.factor(
  moose$conifer
)



# ------------------------------------------------------------
# 4. Re-create binary Detection variable
# ------------------------------------------------------------

moose$Detection <- ifelse(
  moose$Daily_Count > 0,
  "Yes",
  "No"
)

moose$Detection <- factor(
  moose$Detection,
  levels = c(
    "No",
    "Yes"
  )
)



# ------------------------------------------------------------
# 5. Print basic detection information
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Total camera-days:",
  nrow(moose),
  "\n"
)

cat(
  "Zero-detection days:",
  sum(
    moose$Daily_Count == 0
  ),
  "\n"
)

cat(
  "Positive-detection days:",
  sum(
    moose$Daily_Count > 0
  ),
  "\n"
)

cat(
  "Overall detection prevalence:",
  round(
    mean(
      moose$Daily_Count > 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



# ============================================================
# GROUPED TRAIN / TEST SPLIT
# ============================================================
#
# We split by Cell_ID instead of randomly splitting individual
# camera-days.
#
# This prevents observations from the same physical camera site
# from appearing in both training and testing data.
# ============================================================



# ------------------------------------------------------------
# 6. Identify unique sites
# ------------------------------------------------------------

unique.sites <- unique(
  moose$Cell_ID
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

train <- moose[
  moose$Cell_ID %in%
    training.sites,
]

test <- moose[
  moose$Cell_ID %in%
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
  nrow(train),
  "\n"
)

cat(
  "Testing camera-days:",
  nrow(test),
  "\n"
)



# ============================================================
# PART 1:
# DETECTION-PROBABILITY RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 10. Prepare detection-model training data
# ------------------------------------------------------------
#
# Remove Cell_ID because it is an identifier.
#
# Remove Daily_Count because Detection is directly derived from
# Daily_Count. Keeping it would cause data leakage.

detection.train <- train

detection.test <- test


detection.train$Cell_ID <- NULL

detection.test$Cell_ID <- NULL


detection.train$Daily_Count <- NULL

detection.test$Daily_Count <- NULL



# ------------------------------------------------------------
# 11. Fit detection probability Random Forest
# ------------------------------------------------------------

moose.detection.rf <- ranger(
  Detection ~ .,
  data = detection.train,
  num.trees = 500,
  min.node.size = 20,
  probability = TRUE,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 12. Print model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE DETECTION MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  moose.detection.rf
)



# ------------------------------------------------------------
# 13. Predict probability of detection on test sites
# ------------------------------------------------------------

detection.predictions <- predict(
  moose.detection.rf,
  data = detection.test
)$predictions[, "Yes"]



# ------------------------------------------------------------
# 14. Calculate observed detection values
# ------------------------------------------------------------

detection.observed <- ifelse(
  detection.test$Detection ==
    "Yes",
  1,
  0
)



# ------------------------------------------------------------
# 15. Compare observed and predicted prevalence
# ------------------------------------------------------------

observed.prevalence <- mean(
  detection.observed
)

predicted.prevalence <- mean(
  detection.predictions
)


cat(
  "\nObserved test detection prevalence:",
  round(
    observed.prevalence * 100,
    2
  ),
  "%",
  "\n"
)

cat(
  "Mean predicted detection probability:",
  round(
    predicted.prevalence * 100,
    2
  ),
  "%",
  "\n"
)



# ------------------------------------------------------------
# 16. Calculate Brier score
# ------------------------------------------------------------
#
# The Brier score measures the average squared difference
# between predicted probabilities and observed outcomes.
#
# Lower values are better.
#
# A perfect model would have:
#
#     Brier score = 0

brier.score <- mean(
  (
    detection.predictions -
      detection.observed
  )^2
)


cat(
  "Brier score:",
  round(
    brier.score,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 17. 0.50 classification threshold
# ------------------------------------------------------------
#
# This is included as a diagnostic.
#
# Because moose detections are extremely rare, the model may
# classify almost every observation as "No" at a 0.50
# threshold.
#
# That does NOT automatically mean the probability model is
# useless.
#
# The continuous predicted probabilities are more informative
# for our analysis.

predicted.class <- ifelse(
  detection.predictions >= 0.50,
  "Yes",
  "No"
)


predicted.class <- factor(
  predicted.class,
  levels = c(
    "No",
    "Yes"
  )
)



# ------------------------------------------------------------
# 18. Confusion matrix
# ------------------------------------------------------------

cat(
  "\nConfusion matrix using 0.50 threshold:\n"
)

print(
  table(
    Observed =
      detection.test$Detection,
    Predicted =
      predicted.class
  )
)



# ------------------------------------------------------------
# 19. Detection-model permutation importance
# ------------------------------------------------------------

detection.importance <- sort(
  moose.detection.rf$variable.importance,
  decreasing = TRUE
)


cat(
  "\nDetection model permutation importance:\n"
)

print(
  detection.importance
)



# ------------------------------------------------------------
# 20. Print PM2.5 importance rank
# ------------------------------------------------------------

detection.smoke.rank <- which(
  names(
    detection.importance
  ) ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank in detection model:",
  detection.smoke.rank,
  "of",
  length(
    detection.importance
  ),
  "\n"
)



# ============================================================
# PART 2:
# POSITIVE-COUNT RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 21. Keep only days with moose detections
# ------------------------------------------------------------

positive.train <- train[
  train$Daily_Count > 0,
]

positive.test <- test[
  test$Daily_Count > 0,
]



# ------------------------------------------------------------
# 22. Print positive-count sample sizes
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE POSITIVE-COUNT MODEL\n"
)

cat(
  "====================================================\n"
)


cat(
  "Positive training camera-days:",
  nrow(
    positive.train
  ),
  "\n"
)

cat(
  "Positive testing camera-days:",
  nrow(
    positive.test
  ),
  "\n"
)



# ------------------------------------------------------------
# 23. Prepare positive-count model data
# ------------------------------------------------------------
#
# Remove Cell_ID because it is an identifier.
#
# Remove Detection because every observation in this dataset is
# already Detection = Yes.

positive.train.model <-
  positive.train

positive.test.model <-
  positive.test


positive.train.model$Cell_ID <- NULL

positive.test.model$Cell_ID <- NULL


positive.train.model$Detection <- NULL

positive.test.model$Detection <- NULL



# ------------------------------------------------------------
# 24. Fit positive-count Random Forest
# ------------------------------------------------------------

moose.positive.rf <- ranger(
  Daily_Count ~ .,
  data = positive.train.model,
  num.trees = 500,
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 25. Print positive-count model
# ------------------------------------------------------------

print(
  moose.positive.rf
)



# ------------------------------------------------------------
# 26. Predict test positive counts
# ------------------------------------------------------------

positive.predictions <- predict(
  moose.positive.rf,
  data = positive.test.model
)$predictions


positive.observed <-
  positive.test.model$Daily_Count



# ------------------------------------------------------------
# 27. Mean Absolute Error
# ------------------------------------------------------------

positive.mae <- mean(
  abs(
    positive.predictions -
      positive.observed
  )
)



# ------------------------------------------------------------
# 28. Root Mean Squared Error
# ------------------------------------------------------------

positive.rmse <- sqrt(
  mean(
    (
      positive.predictions -
        positive.observed
    )^2
  )
)



# ------------------------------------------------------------
# 29. Test R-squared
# ------------------------------------------------------------
#
# R-squared compares the model's squared prediction error with
# the error from simply predicting the test-set mean.
#
# Positive values mean the model performs better than the mean
# baseline.
#
# Negative values mean the model performs worse than the mean
# baseline.

positive.sse <- sum(
  (
    positive.observed -
      positive.predictions
  )^2
)


positive.sst <- sum(
  (
    positive.observed -
      mean(
        positive.observed
      )
  )^2
)


positive.r2 <- 1 - (
  positive.sse /
    positive.sst
)



# ------------------------------------------------------------
# 30. Print positive-count performance
# ------------------------------------------------------------

cat(
  "\nPositive-count test MAE:",
  round(
    positive.mae,
    4
  ),
  "\n"
)

cat(
  "Positive-count test RMSE:",
  round(
    positive.rmse,
    4
  ),
  "\n"
)

cat(
  "Positive-count test R-squared:",
  round(
    positive.r2,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 31. Positive-count permutation importance
# ------------------------------------------------------------

positive.importance <- sort(
  moose.positive.rf$variable.importance,
  decreasing = TRUE
)


cat(
  "\nPositive-count model permutation importance:\n"
)

print(
  positive.importance
)



# ------------------------------------------------------------
# 32. Print PM2.5 importance rank
# ------------------------------------------------------------

positive.smoke.rank <- which(
  names(
    positive.importance
  ) ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank in positive-count model:",
  positive.smoke.rank,
  "of",
  length(
    positive.importance
  ),
  "\n"
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE VERSION 2 MODEL SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "\n--- DETECTION MODEL ---\n"
)

cat(
  "Observed test prevalence:",
  round(
    observed.prevalence * 100,
    2
  ),
  "%",
  "\n"
)

cat(
  "Mean predicted probability:",
  round(
    predicted.prevalence * 100,
    2
  ),
  "%",
  "\n"
)

cat(
  "Brier score:",
  round(
    brier.score,
    4
  ),
  "\n"
)

cat(
  "PM2.5 importance rank:",
  detection.smoke.rank,
  "of",
  length(
    detection.importance
  ),
  "\n"
)


cat(
  "\n--- POSITIVE-COUNT MODEL ---\n"
)

cat(
  "Positive training days:",
  nrow(
    positive.train
  ),
  "\n"
)

cat(
  "Positive testing days:",
  nrow(
    positive.test
  ),
  "\n"
)

cat(
  "MAE:",
  round(
    positive.mae,
    4
  ),
  "\n"
)

cat(
  "RMSE:",
  round(
    positive.rmse,
    4
  ),
  "\n"
)

cat(
  "Test R-squared:",
  round(
    positive.r2,
    4
  ),
  "\n"
)

cat(
  "PM2.5 importance rank:",
  positive.smoke.rank,
  "of",
  length(
    positive.importance
  ),
  "\n"
)

cat(
  "====================================================\n"
)