# ============================================================
# 08_random_forest_bobcat.R
#
# PURPOSE:
# Fit a Random Forest regression model for daily bobcat
# detections and examine the relationship between wildfire-
# attributable PM2.5 and predicted bobcat detections.
#
#
# MAIN RESEARCH QUESTION:
#
# How is wildfire-attributable PM2.5 associated with the
# number of daily bobcat detections?
#
#
# WHY THIS SPECIES MATTERS:
#
# The parent paper reported that bobcat detections decreased
# as wildfire PM2.5 increased.
#
# Our previous Random Forest results were:
#
#   Mule deer -> positive PM2.5 relationship
#   Moose     -> positive PM2.5 relationship
#
# Bobcat therefore provides an important test of whether the
# SAME Random Forest pipeline can learn a negative relationship.
#
#
# IMPORTANT:
#
# We are NOT changing the modeling method to try to reproduce
# the parent paper.
#
# We intentionally use the SAME settings used for the previous
# species:
#
#   - grouped train/test split by Cell_ID
#   - 500 trees
#   - min.node.size = 80
#   - permutation importance
#   - PM2.5 partial dependence
#   - 1st to 99th percentile PM2.5 range
#
# This keeps comparisons across species fair.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load the cleaned bobcat dataset
# ------------------------------------------------------------
#
# This file was created by:
#
#     07_preprocessing_bobcat.R

bobcat <- read.csv(
  "data-split/bobcat_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# CSV files do not store R factor information.
#
# Therefore, we convert the categorical predictors back into
# factors before fitting the Random Forest.

bobcat$Year <- as.factor(
  bobcat$Year
)

bobcat$landcover <- as.factor(
  bobcat$landcover
)

bobcat$Habitat_Type <- as.factor(
  bobcat$Habitat_Type
)

bobcat$Road_Type <- as.factor(
  bobcat$Road_Type
)

bobcat$Land_Mgnt <- as.factor(
  bobcat$Land_Mgnt
)

bobcat$conifer <- as.factor(
  bobcat$conifer
)



# ------------------------------------------------------------
# 4. Verify the cleaned dataset
# ------------------------------------------------------------

dim(
  bobcat
)

str(
  bobcat
)

colSums(
  is.na(bobcat)
)



# ============================================================
# GROUPED TRAIN / TEST SPLIT
# ============================================================



# ------------------------------------------------------------
# 5. Identify physical camera sites
# ------------------------------------------------------------
#
# Cell_ID represents the physical camera site.
#
# We split by SITE instead of by individual daily observations.
#
# This prevents observations from the same physical site from
# appearing in both training and testing data.

all.sites <- unique(
  bobcat$Cell_ID
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
# 6. Create an 80% training / 20% testing split by site
# ------------------------------------------------------------
#
# We use the same seed as the previous species so the process is
# reproducible.

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
# 7. Create training and testing datasets
# ------------------------------------------------------------

train.data <- bobcat[
  bobcat$Cell_ID %in% train.sites,
]


test.data <- bobcat[
  bobcat$Cell_ID %in% test.sites,
]



# ------------------------------------------------------------
# 8. Check the grouped split
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
  length(unique(train.data$Cell_ID)),
  "\n"
)

cat(
  "Testing sites:",
  length(unique(test.data$Cell_ID)),
  "\n"
)



# ------------------------------------------------------------
# 9. Verify that no sites appear in both groups
# ------------------------------------------------------------
#
# The expected result is:
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



# ------------------------------------------------------------
# 10. Remove Cell_ID before modeling
# ------------------------------------------------------------
#
# Cell_ID was required for grouped validation.
#
# It should NOT be used as a predictor because we do not want
# the model to memorize site identities.

train.model <- train.data

test.model <- test.data


train.model$Cell_ID <- NULL

test.model$Cell_ID <- NULL



# ============================================================
# FIT THE RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 11. Fit Random Forest regression
# ------------------------------------------------------------
#
# TARGET:
#
#     Daily_Count
#
# This is the number of independent bobcat detections recorded
# on a camera-day.
#
#
# num.trees = 500
#
# We average predictions across 500 decision trees.
#
#
# min.node.size = 80
#
# This limits how small terminal nodes can become.
#
# Bobcat detections are extremely sparse, so this helps prevent
# trees from becoming too specialized to a handful of rare
# detection observations.
#
#
# importance = "permutation"
#
# This allows us to later determine how much the model relies on
# each predictor.

bobcat.rf <- ranger(
  Daily_Count ~ .,
  data = train.model,
  num.trees = 500,
  min.node.size = 80,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 12. Print Random Forest summary
# ------------------------------------------------------------

print(
  bobcat.rf
)



# ============================================================
# TEST-SET PREDICTION
# ============================================================



# ------------------------------------------------------------
# 13. Predict observations from unseen camera sites
# ------------------------------------------------------------

test.predictions <- predict(
  bobcat.rf,
  data = test.model
)$predictions



# ------------------------------------------------------------
# 14. Combine actual and predicted values
# ------------------------------------------------------------

prediction.results <- data.frame(
  Actual = test.model$Daily_Count,
  Predicted = test.predictions
)


head(
  prediction.results
)



# ------------------------------------------------------------
# 15. Calculate Mean Absolute Error
# ------------------------------------------------------------
#
# MAE tells us the average absolute prediction error.
#
# Prediction accuracy is NOT the main goal of this project.
#
# We mainly calculate these metrics as a model sanity check.

mae <- mean(
  abs(
    prediction.results$Actual -
      prediction.results$Predicted
  )
)


cat(
  "\nTest MAE:",
  round(mae, 4),
  "\n"
)



# ------------------------------------------------------------
# 16. Calculate Root Mean Squared Error
# ------------------------------------------------------------

rmse <- sqrt(
  mean(
    (
      prediction.results$Actual -
        prediction.results$Predicted
    )^2
  )
)


cat(
  "Test RMSE:",
  round(rmse, 4),
  "\n"
)



# ------------------------------------------------------------
# 17. Calculate test-set R-squared
# ------------------------------------------------------------
#
# Because bobcat detections are extremely rare, R-squared may
# be close to zero or negative.
#
# That would mean the model is weak at predicting exact daily
# counts at unseen sites.
#
# It does NOT automatically tell us that the PM2.5 relationship
# is meaningless.

ss.residual <- sum(
  (
    prediction.results$Actual -
      prediction.results$Predicted
  )^2
)


ss.total <- sum(
  (
    prediction.results$Actual -
      mean(prediction.results$Actual)
  )^2
)


test.r2 <- 1 - (
  ss.residual /
    ss.total
)


cat(
  "Test R-squared:",
  round(test.r2, 4),
  "\n"
)



# ============================================================
# VARIABLE IMPORTANCE
# ============================================================



# ------------------------------------------------------------
# 18. Calculate permutation importance
# ------------------------------------------------------------
#
# Higher positive values indicate that the Random Forest relied
# more strongly on that predictor.
#
# Our main variable of interest is:
#
#     scale.smoke
#
# However, smoke does NOT need to be the most important variable
# for the project to be useful.

importance.values <- bobcat.rf$variable.importance


importance.table <- data.frame(
  Variable = names(importance.values),
  Importance = as.numeric(importance.values)
)


importance.table <- importance.table[
  order(
    importance.table$Importance,
    decreasing = TRUE
  ),
]


rownames(
  importance.table
) <- NULL


cat(
  "\nPermutation Variable Importance:\n"
)


print(
  importance.table
)



# ------------------------------------------------------------
# 19. Find the PM2.5 importance rank
# ------------------------------------------------------------

smoke.rank <- which(
  importance.table$Variable ==
    "scale.smoke"
)


cat(
  "\nscale.smoke importance rank:",
  smoke.rank,
  "out of",
  nrow(importance.table),
  "predictors\n"
)



# ------------------------------------------------------------
# 20. Create results folder if needed
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 21. Save variable importance table
# ------------------------------------------------------------

write.csv(
  importance.table,
  "results/bobcat_variable_importance.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 22. Plot variable importance
# ------------------------------------------------------------

importance.plot <- ggplot(
  importance.table,
  aes(
    x = reorder(
      Variable,
      Importance
    ),
    y = Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance - Bobcat",
    x = "Predictor",
    y = "Permutation Importance"
  ) +
  theme_minimal()


print(
  importance.plot
)



# ------------------------------------------------------------
# 23. Save variable importance plot
# ------------------------------------------------------------

ggsave(
  filename = "results/bobcat_variable_importance.png",
  plot = importance.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL MODEL USING ALL CLEANED OBSERVATIONS
# ============================================================



# ------------------------------------------------------------
# 24. Create full modeling dataset
# ------------------------------------------------------------
#
# Cell_ID is removed because it is only an identifier.

bobcat.full.model <- bobcat


bobcat.full.model$Cell_ID <- NULL



# ------------------------------------------------------------
# 25. Fit Random Forest using all observations
# ------------------------------------------------------------
#
# We use the exact same model settings used above.

bobcat.rf.full <- ranger(
  Daily_Count ~ .,
  data = bobcat.full.model,
  num.trees = 500,
  min.node.size = 80,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)


cat(
  "\nFinal Random Forest using all cleaned bobcat data:\n"
)


print(
  bobcat.rf.full
)



# ============================================================
# PM2.5 PARTIAL DEPENDENCE
# ============================================================
#
# This is the main relationship we want to interpret.
#
#
# QUESTION:
#
# How do predicted bobcat detections change when wildfire PM2.5
# changes?
#
#
# For every PM2.5 value:
#
#   1. Give all observations that PM2.5 value
#   2. Leave every other predictor unchanged
#   3. Ask the Random Forest to predict Daily_Count
#   4. Average those predictions
#
#
# IMPORTANT:
#
# This is a model association.
#
# It does NOT prove that smoke causes bobcats to behave in a
# particular way.
# ============================================================



# ------------------------------------------------------------
# 26. Define the PM2.5 range
# ------------------------------------------------------------
#
# We use the 1st through 99th percentile.
#
# This prevents a very small number of extreme-smoke
# observations from dominating the response curve.

smoke.lower <- as.numeric(
  quantile(
    bobcat$scale.smoke,
    probs = 0.01
  )
)


smoke.upper <- as.numeric(
  quantile(
    bobcat$scale.smoke,
    probs = 0.99
  )
)


cat(
  "\nPM2.5 partial-dependence range:\n"
)

cat(
  "Lower value:",
  smoke.lower,
  "\n"
)

cat(
  "Upper value:",
  smoke.upper,
  "\n"
)



# ------------------------------------------------------------
# 27. Create 50 PM2.5 values across the modeled range
# ------------------------------------------------------------

smoke.grid <- seq(
  from = smoke.lower,
  to = smoke.upper,
  length.out = 50
)



# ------------------------------------------------------------
# 28. Create storage for partial-dependence predictions
# ------------------------------------------------------------

smoke.pd <- numeric(
  length(smoke.grid)
)



# ------------------------------------------------------------
# 29. Calculate PM2.5 partial dependence
# ------------------------------------------------------------

for (i in seq_along(smoke.grid)) {
  
  
  # Make a temporary copy of the full predictor dataset.
  
  temporary.data <- bobcat.full.model
  
  
  # Give every observation the selected smoke value.
  
  temporary.data$scale.smoke <-
    smoke.grid[i]
  
  
  # Generate Random Forest predictions.
  
  temporary.predictions <- predict(
    bobcat.rf.full,
    data = temporary.data
  )$predictions
  
  
  # Store the average prediction.
  
  smoke.pd[i] <- mean(
    temporary.predictions
  )
}



# ------------------------------------------------------------
# 30. Create PM2.5 response table
# ------------------------------------------------------------

smoke.response <- data.frame(
  scale.smoke = smoke.grid,
  predicted.daily.detections = smoke.pd
)


cat(
  "\nPM2.5 Partial Dependence Results:\n"
)


print(
  smoke.response
)



# ------------------------------------------------------------
# 31. Calculate overall low-to-high PM2.5 change
# ------------------------------------------------------------
#
# This is:
#
#     prediction at high smoke
#
# minus
#
#     prediction at low smoke
#
#
# Positive:
#     predicted detections increased
#
# Negative:
#     predicted detections decreased
#
# Near zero:
#     little overall change
#
#
# This is especially important for bobcat because the parent
# paper reported a negative relationship.

low.smoke.prediction <-
  smoke.response$predicted.daily.detections[1]


high.smoke.prediction <-
  smoke.response$predicted.daily.detections[
    nrow(smoke.response)
  ]


smoke.response.change <-
  high.smoke.prediction -
  low.smoke.prediction



cat(
  "\nPredicted detections at LOW modeled PM2.5:",
  round(
    low.smoke.prediction,
    6
  ),
  "\n"
)


cat(
  "Predicted detections at HIGH modeled PM2.5:",
  round(
    high.smoke.prediction,
    6
  ),
  "\n"
)


cat(
  "Overall low-to-high PM2.5 change:",
  round(
    smoke.response.change,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 32. Calculate relative percent change
# ------------------------------------------------------------
#
# Because bobcat predictions are likely to be very small,
# percentage changes can appear large even when the absolute
# difference is tiny.
#
# Therefore, ALWAYS interpret this together with the absolute
# low-to-high difference.

relative.change.percent <- (
  smoke.response.change /
    low.smoke.prediction
) * 100


cat(
  "Relative low-to-high change:",
  round(
    relative.change.percent,
    2
  ),
  "%\n"
)



# ------------------------------------------------------------
# 33. Plot the PM2.5 relationship
# ------------------------------------------------------------

smoke.plot <- ggplot(
  smoke.response,
  aes(
    x = scale.smoke,
    y = predicted.daily.detections
  )
) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Bobcat Response to Wildfire PM2.5",
    subtitle = "Random Forest Partial Dependence",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "Predicted Daily Bobcat Detections"
  ) +
  theme_minimal()


print(
  smoke.plot
)



# ------------------------------------------------------------
# 34. Save PM2.5 response data
# ------------------------------------------------------------

write.csv(
  smoke.response,
  "results/bobcat_pm25_partial_dependence.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 35. Save PM2.5 response plot
# ------------------------------------------------------------

ggsave(
  filename = "results/bobcat_pm25_partial_dependence.png",
  plot = smoke.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL SUMMARY
# ============================================================



# ------------------------------------------------------------
# 36. Print the most important results together
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "BOBCAT RANDOM FOREST SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Cleaned observations:",
  nrow(bobcat),
  "\n"
)

cat(
  "Physical camera sites:",
  length(
    unique(bobcat$Cell_ID)
  ),
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
  "Test MAE:",
  round(mae, 4),
  "\n"
)

cat(
  "Test RMSE:",
  round(rmse, 4),
  "\n"
)

cat(
  "Test R-squared:",
  round(test.r2, 4),
  "\n"
)

cat(
  "PM2.5 importance rank:",
  smoke.rank,
  "of",
  nrow(importance.table),
  "\n"
)

cat(
  "Low-smoke predicted detections:",
  round(
    low.smoke.prediction,
    6
  ),
  "\n"
)

cat(
  "High-smoke predicted detections:",
  round(
    high.smoke.prediction,
    6
  ),
  "\n"
)

cat(
  "Low-to-high PM2.5 change:",
  round(
    smoke.response.change,
    6
  ),
  "\n"
)

cat(
  "Relative change:",
  round(
    relative.change.percent,
    2
  ),
  "%\n"
)

cat(
  "====================================================\n"
)