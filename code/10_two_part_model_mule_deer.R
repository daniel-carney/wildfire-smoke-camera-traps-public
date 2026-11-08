# ============================================================
# 10_two_part_model_mule_deer.R
#
# VERSION 2 MODELING PIPELINE
#
# PURPOSE:
#
# Build a TWO-PART Random Forest model for mule deer.
#
# We are changing the modeling strategy because our original
# single Random Forest + partial-dependence approach failed an
# important shuffle test.
#
# When wildfire PM2.5 was randomly shuffled for bobcat, the old
# method still produced positive PM2.5 response curves.
#
# That means we should not continue interpreting the old PDP
# curves as evidence of a real smoke relationship.
#
#
# THE NEW TWO-PART APPROACH:
#
# PART 1:
#   Model whether mule deer are detected at all.
#
#       0 = no mule deer detections
#       1 = at least one mule deer detection
#
#   This is a Random Forest CLASSIFICATION model.
#
#
# PART 2:
#   Among camera-days where mule deer WERE detected, model the
#   number of detections.
#
#   This is a Random Forest REGRESSION model.
#
#
# WHY DO THIS?
#
# Wildlife camera-trap data contain many zero-detection days.
#
# For mule deer, roughly 90% of usable camera-days contain zero
# detections.
#
# Instead of asking one model to simultaneously explain:
#
#     whether an animal appears
#
# AND
#
#     how many detections occur when it appears
#
# we separate those into two easier questions.
#
#
# IMPORTANT:
#
# This script DOES NOT yet calculate ALE or interpret PM2.5.
#
# First, we want to confirm that the two models work correctly.
#
# ALE and the new PM2.5 diagnostics will come in the next
# scripts.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------
#
# ranger is used to fit both:
#
#   Random Forest classification
#
# and
#
#   Random Forest regression

library(ranger)



# ------------------------------------------------------------
# 2. Load the cleaned mule deer dataset
# ------------------------------------------------------------
#
# This dataset was created earlier by:
#
#     01_preprocessing_mule_deer.R
#
# We DO NOT need to repeat the original preprocessing.

mule <- read.csv(
  "data-split/mule_deer_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# When an R data frame is written to a CSV file, factor
# information is not preserved.
#
# Therefore, after reading the CSV back into R, we convert the
# categorical variables back into factors.

mule$Year <- as.factor(
  mule$Year
)

mule$landcover <- as.factor(
  mule$landcover
)

mule$Habitat_Type <- as.factor(
  mule$Habitat_Type
)

mule$Road_Type <- as.factor(
  mule$Road_Type
)

mule$Land_Mgnt <- as.factor(
  mule$Land_Mgnt
)

mule$conifer <- as.factor(
  mule$conifer
)



# ------------------------------------------------------------
# 4. Verify the cleaned dataset
# ------------------------------------------------------------
#
# These checks confirm that we loaded the expected data before
# building the new model.

dim(
  mule
)


str(
  mule
)


colSums(
  is.na(mule)
)



# ============================================================
# CREATE THE BINARY DETECTION TARGET
# ============================================================



# ------------------------------------------------------------
# 5. Create a NEW detection variable
# ------------------------------------------------------------
#
# Our cleaned dataset intentionally removed the original
# "Detected" column because it was target leakage for our old
# Daily_Count regression model.
#
# For this NEW two-part approach, however, whether an animal was
# detected is now the actual target of Part 1.
#
# Therefore, we create a NEW response variable directly from
# Daily_Count.
#
#
# If:
#
#     Daily_Count == 0
#
# then:
#
#     Detection = "No"
#
#
# If:
#
#     Daily_Count > 0
#
# then:
#
#     Detection = "Yes"

mule$Detection <- ifelse(
  mule$Daily_Count > 0,
  "Yes",
  "No"
)



# ------------------------------------------------------------
# 6. Convert Detection into a factor
# ------------------------------------------------------------
#
# A classification Random Forest requires a categorical target.
#
# We explicitly order the levels as:
#
#     No
#     Yes

mule$Detection <- factor(
  mule$Detection,
  levels = c(
    "No",
    "Yes"
  )
)



# ------------------------------------------------------------
# 7. Inspect the detection classes
# ------------------------------------------------------------
#
# This shows how many camera-days contain:
#
#     No detection
#
# versus
#
#     At least one detection

table(
  mule$Detection
)



# ------------------------------------------------------------
# 8. Calculate observed detection prevalence
# ------------------------------------------------------------
#
# Detection prevalence is simply the percentage of camera-days
# where at least one mule deer detection occurred.

detection.prevalence <- mean(
  mule$Detection == "Yes"
)


cat(
  "\nObserved mule deer detection prevalence:",
  round(
    detection.prevalence * 100,
    2
  ),
  "%\n"
)



# ============================================================
# GROUPED TRAIN / TEST SPLIT
# ============================================================



# ------------------------------------------------------------
# 9. Identify physical camera sites
# ------------------------------------------------------------
#
# We continue using Cell_ID for grouped validation.
#
# This is one part of the original methodology that was working
# correctly and should NOT be changed.
#
# Every observation from one physical camera site must belong
# entirely to either:
#
#     training data
#
# OR
#
#     testing data
#
# but never both.

all.sites <- unique(
  mule$Cell_ID
)


number.sites <- length(
  all.sites
)


cat(
  "\nTotal physical camera sites:",
  number.sites,
  "\n"
)



# ------------------------------------------------------------
# 10. Create the same 80% / 20% grouped site split
# ------------------------------------------------------------
#
# set.seed(42) makes the split reproducible.

set.seed(42)


number.train.sites <- floor(
  0.80 * number.sites
)


train.sites <- sample(
  all.sites,
  size = number.train.sites,
  replace = FALSE
)


test.sites <- setdiff(
  all.sites,
  train.sites
)



# ------------------------------------------------------------
# 11. Create training and testing datasets
# ------------------------------------------------------------

train.data <- mule[
  mule$Cell_ID %in% train.sites,
]


test.data <- mule[
  mule$Cell_ID %in% test.sites,
]



# ------------------------------------------------------------
# 12. Verify the grouped split
# ------------------------------------------------------------

cat(
  "\nTraining rows:",
  nrow(train.data),
  "\n"
)


cat(
  "Testing rows:",
  nrow(test.data),
  "\n"
)


cat(
  "Training sites:",
  length(
    unique(train.data$Cell_ID)
  ),
  "\n"
)


cat(
  "Testing sites:",
  length(
    unique(test.data$Cell_ID)
  ),
  "\n"
)



# ------------------------------------------------------------
# 13. Confirm there is NO site overlap
# ------------------------------------------------------------
#
# Expected result:
#
#     character(0)

shared.sites <- intersect(
  unique(train.data$Cell_ID),
  unique(test.data$Cell_ID)
)


cat(
  "\nSites appearing in BOTH train and test:\n"
)


print(
  shared.sites
)



# ============================================================
# PART 1:
# RANDOM FOREST DETECTION MODEL
# ============================================================
#
# Question:
#
# What is the probability that mule deer are detected on a
# particular camera-day?
#
# ============================================================



# ------------------------------------------------------------
# 14. Prepare data for the detection model
# ------------------------------------------------------------
#
# We create copies because we do not want to alter train.data or
# test.data.
#
# We REMOVE:
#
#     Cell_ID
#
# because it is only an identifier.
#
#
# We ALSO REMOVE:
#
#     Daily_Count
#
# from this classification model.
#
# This is extremely important.
#
# Detection was created directly from Daily_Count.
#
# If Daily_Count remained as a predictor, the model would
# already know whether a detection occurred.
#
# That would be target leakage.

detection.train <- train.data

detection.test <- test.data


detection.train$Cell_ID <- NULL

detection.test$Cell_ID <- NULL


detection.train$Daily_Count <- NULL

detection.test$Daily_Count <- NULL



# ------------------------------------------------------------
# 15. Inspect class balance in training data
# ------------------------------------------------------------

cat(
  "\nTraining detection classes:\n"
)


print(
  table(
    detection.train$Detection
  )
)



# ------------------------------------------------------------
# 16. Fit Random Forest CLASSIFICATION model
# ------------------------------------------------------------
#
# Detection is a factor, so ranger knows this is a
# classification problem.
#
#
# probability = TRUE
#
# This is important.
#
# Instead of only predicting:
#
#     No
#
# or
#
#     Yes
#
# the Random Forest estimates a probability for each class.
#
# Our main future interpretation will focus on:
#
#     Probability of Detection = "Yes"
#
#
# num.trees = 500
#
# We continue using 500 trees for stability.
#
#
# min.node.size = 20
#
# Version 2 is a new modeling design.
#
# We use a smaller minimum node size than the original count
# regression because the classification model needs enough
# flexibility to learn patterns associated with the relatively
# uncommon detection days.
#
# We will keep this setting fixed when the Version 2 pipeline is
# eventually applied across species.

mule.detection.rf <- ranger(
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
# 17. Print detection-model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "PART 1: MULE DEER DETECTION MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  mule.detection.rf
)



# ------------------------------------------------------------
# 18. Predict detection probabilities at unseen sites
# ------------------------------------------------------------
#
# Because probability = TRUE, predict() returns probabilities
# for:
#
#     No
#
# and
#
#     Yes
#
# We specifically store the probability of:
#
#     Detection = "Yes"

detection.predictions <- predict(
  mule.detection.rf,
  data = detection.test
)$predictions


head(
  detection.predictions
)



# ------------------------------------------------------------
# 19. Store probability of detection
# ------------------------------------------------------------

probability.yes <-
  detection.predictions[, "Yes"]



# ------------------------------------------------------------
# 20. Compare average predicted probability to reality
# ------------------------------------------------------------
#
# This provides a simple calibration check.
#
# If the test data contain about 10% detection days, we would
# hope that the model's average predicted probability is in the
# same general range.

observed.test.prevalence <- mean(
  detection.test$Detection == "Yes"
)


mean.predicted.probability <- mean(
  probability.yes
)


cat(
  "\nObserved test-set detection prevalence:",
  round(
    observed.test.prevalence * 100,
    2
  ),
  "%\n"
)


cat(
  "Average predicted detection probability:",
  round(
    mean.predicted.probability * 100,
    2
  ),
  "%\n"
)



# ------------------------------------------------------------
# 21. Calculate Brier score
# ------------------------------------------------------------
#
# The Brier score evaluates probability predictions.
#
# It compares:
#
#     predicted probability
#
# against
#
#     actual outcome (0 or 1)
#
# Smaller values are better.
#
# A perfect model would have a Brier score of 0.

actual.binary <- ifelse(
  detection.test$Detection == "Yes",
  1,
  0
)


brier.score <- mean(
  (
    probability.yes -
      actual.binary
  )^2
)


cat(
  "Detection-model Brier score:",
  round(
    brier.score,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 22. Create simple 0.50-threshold classifications
# ------------------------------------------------------------
#
# This is NOT our main model output.
#
# The probabilities themselves are more useful.
#
# We calculate this only as an easy-to-understand diagnostic.
#
# If predicted probability >= 0.50:
#
#     predict Yes
#
# otherwise:
#
#     predict No

predicted.class <- ifelse(
  probability.yes >= 0.50,
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
# 23. Display confusion matrix
# ------------------------------------------------------------
#
# Rows represent actual outcome.
#
# Columns represent predicted outcome.
#
# Because detections are uncommon, it would not be surprising
# if a 0.50 cutoff predicts relatively few "Yes" observations.

confusion.matrix <- table(
  Actual = detection.test$Detection,
  Predicted = predicted.class
)


cat(
  "\nDetection-model confusion matrix:\n"
)


print(
  confusion.matrix
)



# ------------------------------------------------------------
# 24. Calculate permutation importance for detection model
# ------------------------------------------------------------

detection.importance <-
  mule.detection.rf$variable.importance


detection.importance.table <- data.frame(
  Variable = names(
    detection.importance
  ),
  Importance = as.numeric(
    detection.importance
  )
)


detection.importance.table <-
  detection.importance.table[
    order(
      detection.importance.table$Importance,
      decreasing = TRUE
    ),
  ]


rownames(
  detection.importance.table
) <- NULL


cat(
  "\nDetection-model variable importance:\n"
)


print(
  detection.importance.table
)



# ------------------------------------------------------------
# 25. Find PM2.5 importance rank in detection model
# ------------------------------------------------------------

detection.smoke.rank <- which(
  detection.importance.table$Variable ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank in DETECTION model:",
  detection.smoke.rank,
  "of",
  nrow(detection.importance.table),
  "\n"
)



# ============================================================
# PART 2:
# POSITIVE-COUNT RANDOM FOREST
# ============================================================
#
# Question:
#
# WHEN mule deer are detected, how many detections occur?
#
#
# IMPORTANT:
#
# This model completely removes zero-detection days.
#
# It is NOT trying to determine whether an animal appears.
#
# Part 1 already handles that question.
#
# ============================================================



# ------------------------------------------------------------
# 26. Keep only POSITIVE detection days
# ------------------------------------------------------------
#
# Daily_Count must be greater than zero.

positive.train <- train.data[
  train.data$Daily_Count > 0,
]


positive.test <- test.data[
  test.data$Daily_Count > 0,
]



# ------------------------------------------------------------
# 27. Show positive-data sample sizes
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "PART 2: POSITIVE-COUNT MODEL\n"
)

cat(
  "====================================================\n"
)


cat(
  "Positive training camera-days:",
  nrow(positive.train),
  "\n"
)


cat(
  "Positive testing camera-days:",
  nrow(positive.test),
  "\n"
)



# ------------------------------------------------------------
# 28. Remove variables not used as predictors
# ------------------------------------------------------------
#
# Cell_ID is only an identifier.
#
# Detection is also unnecessary here because every observation
# in this dataset has:
#
#     Detection = Yes
#
# It therefore contains no useful variation.

positive.train.model <- positive.train

positive.test.model <- positive.test


positive.train.model$Cell_ID <- NULL

positive.test.model$Cell_ID <- NULL


positive.train.model$Detection <- NULL

positive.test.model$Detection <- NULL



# ------------------------------------------------------------
# 29. Inspect positive Daily_Count distribution
# ------------------------------------------------------------
#
# This tells us what the regression model is trying to predict
# once zero days have been removed.

cat(
  "\nPositive Daily_Count summary:\n"
)


print(
  summary(
    positive.train.model$Daily_Count
  )
)


cat(
  "\nPositive Daily_Count frequencies:\n"
)


print(
  table(
    positive.train.model$Daily_Count
  )
)



# ------------------------------------------------------------
# 30. Fit positive-count Random Forest regression
# ------------------------------------------------------------
#
# This is a regression model because Daily_Count is numerical.
#
#
# num.trees = 500
#
# Again, we use 500 trees.
#
#
# min.node.size = 20
#
# We use 20 rather than the old value of 80 because this model
# contains far fewer observations after zero-detection days are
# removed.
#
# A node size of 80 would make the forest unnecessarily rigid.
#
# We will keep this Version 2 setting consistent across species
# unless diagnostics later show a strong reason not to.

mule.positive.rf <- ranger(
  Daily_Count ~ .,
  data = positive.train.model,
  num.trees = 500,
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 31. Print positive-count model summary
# ------------------------------------------------------------

print(
  mule.positive.rf
)



# ------------------------------------------------------------
# 32. Predict counts for positive test observations
# ------------------------------------------------------------
#
# Remember:
#
# These test observations are ONLY days where at least one mule
# deer was actually detected.

positive.predictions <- predict(
  mule.positive.rf,
  data = positive.test.model
)$predictions



# ------------------------------------------------------------
# 33. Calculate positive-count MAE
# ------------------------------------------------------------

positive.mae <- mean(
  abs(
    positive.test.model$Daily_Count -
      positive.predictions
  )
)


cat(
  "\nPositive-count Test MAE:",
  round(
    positive.mae,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 34. Calculate positive-count RMSE
# ------------------------------------------------------------

positive.rmse <- sqrt(
  mean(
    (
      positive.test.model$Daily_Count -
        positive.predictions
    )^2
  )
)


cat(
  "Positive-count Test RMSE:",
  round(
    positive.rmse,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 35. Calculate positive-count R-squared
# ------------------------------------------------------------

positive.ss.residual <- sum(
  (
    positive.test.model$Daily_Count -
      positive.predictions
  )^2
)


positive.ss.total <- sum(
  (
    positive.test.model$Daily_Count -
      mean(
        positive.test.model$Daily_Count
      )
  )^2
)


positive.r2 <- 1 - (
  positive.ss.residual /
    positive.ss.total
)


cat(
  "Positive-count Test R-squared:",
  round(
    positive.r2,
    4
  ),
  "\n"
)



# ------------------------------------------------------------
# 36. Calculate variable importance for positive-count model
# ------------------------------------------------------------

positive.importance <-
  mule.positive.rf$variable.importance


positive.importance.table <- data.frame(
  Variable = names(
    positive.importance
  ),
  Importance = as.numeric(
    positive.importance
  )
)


positive.importance.table <-
  positive.importance.table[
    order(
      positive.importance.table$Importance,
      decreasing = TRUE
    ),
  ]


rownames(
  positive.importance.table
) <- NULL


cat(
  "\nPositive-count variable importance:\n"
)


print(
  positive.importance.table
)



# ------------------------------------------------------------
# 37. Find PM2.5 importance rank in positive-count model
# ------------------------------------------------------------

positive.smoke.rank <- which(
  positive.importance.table$Variable ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank in POSITIVE-COUNT model:",
  positive.smoke.rank,
  "of",
  nrow(positive.importance.table),
  "\n"
)



# ============================================================
# SAVE RESULTS
# ============================================================



# ------------------------------------------------------------
# 38. Create results folder if needed
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 39. Save detection-model importance
# ------------------------------------------------------------

write.csv(
  detection.importance.table,
  "results/mule_deer_v2_detection_importance.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 40. Save positive-count model importance
# ------------------------------------------------------------

write.csv(
  positive.importance.table,
  "results/mule_deer_v2_positive_count_importance.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 41. Save detection probabilities for test data
# ------------------------------------------------------------

detection.results <- data.frame(
  Actual = detection.test$Detection,
  Probability_Yes = probability.yes,
  Predicted_Class = predicted.class
)


write.csv(
  detection.results,
  "results/mule_deer_v2_detection_predictions.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 42. Save positive-count predictions
# ------------------------------------------------------------

positive.results <- data.frame(
  Actual_Count =
    positive.test.model$Daily_Count,
  Predicted_Count =
    positive.predictions
)


write.csv(
  positive.results,
  "results/mule_deer_v2_positive_count_predictions.csv",
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================
#
# This gives us the main diagnostic information in one place.
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER VERSION 2 MODEL SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Total observations:",
  nrow(mule),
  "\n"
)


cat(
  "Physical camera sites:",
  number.sites,
  "\n"
)


cat(
  "Training sites:",
  length(train.sites),
  "\n"
)


cat(
  "Testing sites:",
  length(test.sites),
  "\n"
)


cat(
  "\n--- PART 1: DETECTION MODEL ---\n"
)


cat(
  "Observed test detection prevalence:",
  round(
    observed.test.prevalence * 100,
    2
  ),
  "%\n"
)


cat(
  "Mean predicted detection probability:",
  round(
    mean.predicted.probability * 100,
    2
  ),
  "%\n"
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
  "PM2.5 detection-model importance rank:",
  detection.smoke.rank,
  "of",
  nrow(detection.importance.table),
  "\n"
)


cat(
  "\n--- PART 2: POSITIVE-COUNT MODEL ---\n"
)


cat(
  "Positive training days:",
  nrow(positive.train),
  "\n"
)


cat(
  "Positive testing days:",
  nrow(positive.test),
  "\n"
)


cat(
  "Positive-count MAE:",
  round(
    positive.mae,
    4
  ),
  "\n"
)


cat(
  "Positive-count RMSE:",
  round(
    positive.rmse,
    4
  ),
  "\n"
)


cat(
  "Positive-count R-squared:",
  round(
    positive.r2,
    4
  ),
  "\n"
)


cat(
  "PM2.5 positive-count importance rank:",
  positive.smoke.rank,
  "of",
  nrow(positive.importance.table),
  "\n"
)


cat(
  "====================================================\n"
)