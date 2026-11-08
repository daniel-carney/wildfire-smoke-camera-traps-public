# ============================================================
# 05_random_forest_moose.R
#
# PURPOSE:
# Fit a Random Forest regression model for daily moose
# detections and examine the relationship between wildfire-
# attributable PM2.5 and predicted moose detections.
#
#
# MAIN RESEARCH QUESTION:
#
# How is wildfire-attributable PM2.5 associated with the
# number of daily moose detections?
#
#
# IMPORTANT:
#
# We are intentionally using the SAME general modeling
# procedure that we used for mule deer.
#
# We do not want to change the method simply because the parent
# paper found a negative smoke relationship for moose.
#
# Keeping the methodology consistent allows us to make a fairer
# comparison between species.
#
#
# THIS SCRIPT WILL:
#
#   1. Load the cleaned moose data
#   2. Restore categorical variables as factors
#   3. Split the data by physical camera site
#   4. Fit a Random Forest to the training data
#   5. Evaluate predictions on unseen camera sites
#   6. Calculate permutation variable importance
#   7. Refit the model using all available sites
#   8. Calculate the PM2.5 partial-dependence relationship
#   9. Save useful results and figures
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------
#
# ranger:
#   Fits the Random Forest regression model.
#
# ggplot2:
#   Creates plots for variable importance and PM2.5 response.
#
# If ranger or ggplot2 are not installed, run:
#
# install.packages("ranger")
# install.packages("ggplot2")

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load the CLEANED moose dataset
# ------------------------------------------------------------
#
# This file was created by:
#
#     04_preprocessing_moose.R
#
# We use the cleaned file instead of repeating preprocessing
# inside the modeling script.

moose <- read.csv(
  "data-split/moose_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# IMPORTANT:
#
# When R writes factors to a CSV file, the CSV does not preserve
# the fact that those variables were factors.
#
# When we read the CSV back into R, some categorical variables
# may therefore appear as numbers or character strings.
#
# We convert them back to factors before fitting the model.

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
# 4. Verify the data before modeling
# ------------------------------------------------------------
#
# These checks confirm that the correct dataset was loaded.

dim(moose)

str(moose)

colSums(
  is.na(moose)
)



# ------------------------------------------------------------
# 5. Identify all physical camera sites
# ------------------------------------------------------------
#
# Cell_ID represents a physical camera site.
#
# We will divide CAMERA SITES into training and testing groups
# instead of randomly dividing individual rows.
#
# This is important because each site contributes many daily
# observations.
#
# If we randomly split individual rows, observations from the
# same site could appear in BOTH the training and test data.
#
# The model could then partially memorize characteristics of
# that site.
#
# By splitting on Cell_ID, the test set contains completely
# unseen physical camera locations.

all.sites <- unique(
  moose$Cell_ID
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
# 6. Create an 80% training / 20% testing split BY SITE
# ------------------------------------------------------------
#
# set.seed() makes the random split reproducible.
#
# This means that if we rerun this script, we will get the same
# training and testing sites.
#
# We use seed 42 because that is the same seed used in the
# mule-deer development model.

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
#
# Every observation belonging to a training site goes into
# train.data.
#
# Every observation belonging to a testing site goes into
# test.data.

train.data <- moose[
  moose$Cell_ID %in% train.sites,
]


test.data <- moose[
  moose$Cell_ID %in% test.sites,
]



# ------------------------------------------------------------
# 8. Check the grouped split
# ------------------------------------------------------------
#
# We print the number of observations and sites in each group.

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
# 9. Verify that NO camera site appears in both groups
# ------------------------------------------------------------
#
# intersect() looks for Cell_ID values that occur in both the
# training and testing datasets.
#
# The result should be:
#
#     character(0)
#
# This confirms that our grouped validation worked.

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
# 10. Remove Cell_ID before fitting the model
# ------------------------------------------------------------
#
# We needed Cell_ID to create the grouped split.
#
# However, Cell_ID should NOT be used as a predictor.
#
# Otherwise, the Random Forest could learn that particular
# sites tend to have more or fewer moose detections.
#
# We want the model to learn relationships with environmental
# and habitat variables instead.

train.model <- train.data

test.model <- test.data


train.model$Cell_ID <- NULL

test.model$Cell_ID <- NULL



# ------------------------------------------------------------
# 11. Fit the Random Forest regression model
# ------------------------------------------------------------
#
# Our target is:
#
#     Daily_Count
#
# This is the number of independent moose detections recorded
# at a camera on a particular day.
#
#
# RANDOM FOREST:
#
# A Random Forest fits many decision trees.
#
# Each tree sees a slightly different sample of the data and
# considers different subsets of predictors.
#
# The predictions from all trees are then averaged together.
#
# This makes the model more stable than relying on one single
# decision tree.


moose.rf <- ranger(
  
  # Predict Daily_Count using all other columns.
  
  Daily_Count ~ .,
  
  
  # Training dataset.
  
  data = train.model,
  
  
  # Fit 500 individual decision trees.
  #
  # This is the same number used for mule deer.
  
  num.trees = 500,
  
  
  # Minimum number of observations allowed in a terminal node.
  #
  # A relatively large value helps prevent the trees from
  # becoming extremely specific to rare detection events.
  #
  # This is especially important for moose because almost 99%
  # of camera-days contain zero detections.
  #
  # We keep this at 80 because we used the same setting for
  # mule deer and want a consistent species comparison.
  
  min.node.size = 80,
  
  
  # Calculate permutation importance.
  #
  # Permutation importance measures how much predictive
  # performance becomes worse when one variable is randomly
  # shuffled.
  #
  # A larger positive value suggests that the model relied more
  # heavily on that predictor.
  
  importance = "permutation",
  
  
  # Tell ranger how to handle unordered categorical factors.
  
  respect.unordered.factors = "order",
  
  
  # Make the Random Forest itself reproducible.
  
  seed = 42
)



# ------------------------------------------------------------
# 12. Print the Random Forest model summary
# ------------------------------------------------------------
#
# The output includes:
#
#   number of trees
#   number of predictors
#   mtry
#   minimum node size
#   OOB prediction error
#   OOB R-squared
#
#
# OOB = Out Of Bag
#
# Each tree is trained using only part of the available
# training data.
#
# The observations not used for a particular tree can be used
# to estimate prediction error.

print(
  moose.rf
)



# ------------------------------------------------------------
# 13. Predict DAILY COUNTS for the unseen testing sites
# ------------------------------------------------------------
#
# These sites were not used to train the Random Forest.
#
# This gives us a more realistic idea of how well the model
# predicts at new physical camera locations.

test.predictions <- predict(
  moose.rf,
  data = test.model
)$predictions



# ------------------------------------------------------------
# 14. Save actual and predicted values
# ------------------------------------------------------------
#
# This creates a small table where:
#
# Actual    = observed Daily_Count
# Predicted = Random Forest prediction

prediction.results <- data.frame(
  Actual = test.model$Daily_Count,
  Predicted = test.predictions
)


head(
  prediction.results
)



# ------------------------------------------------------------
# 15. Calculate Mean Absolute Error (MAE)
# ------------------------------------------------------------
#
# MAE measures the average absolute difference between the
# observed count and predicted count.
#
# Smaller values indicate predictions closer to the observed
# values.
#
# IMPORTANT:
#
# Prediction accuracy is NOT the main scientific goal of this
# project.
#
# We calculate these metrics mainly as a sanity check that the
# model is behaving reasonably.

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
# 16. Calculate Root Mean Squared Error (RMSE)
# ------------------------------------------------------------
#
# RMSE also measures prediction error, but larger errors receive
# more weight because the differences are squared.

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
# R-squared compares our prediction errors to simply predicting
# the average Daily_Count for every observation.
#
# An R-squared close to:
#
#   1 = strong predictions
#   0 = little improvement over predicting the mean
#
# A negative R-squared can occur when the model performs worse
# than simply predicting the test-set mean.
#
# Moose detections are extremely rare, so weak or even negative
# test R-squared would not automatically mean that the PM2.5
# analysis is useless.

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
  ss.residual / ss.total
)


cat(
  "Test R-squared:",
  round(test.r2, 4),
  "\n"
)



# ------------------------------------------------------------
# 18. Calculate permutation variable importance
# ------------------------------------------------------------
#
# This tells us which predictors the Random Forest relied on
# most.
#
# Our main variable of interest is:
#
#     scale.smoke
#
# However, smoke does NOT need to be the most important
# predictor for the project to be meaningful.
#
# Wildlife detections are influenced by many factors such as:
#
#   season
#   temperature
#   habitat
#   elevation
#   road distance
#
# We mainly want to know:
#
#   1. Does the model use smoke at all?
#
#   2. What relationship does the model learn between smoke
#      and predicted detections?

importance.values <- moose.rf$variable.importance


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
# 19. Find the importance rank of wildfire PM2.5
# ------------------------------------------------------------
#
# This makes it easy to report something such as:
#
# "PM2.5 ranked 6th out of 13 predictors."
#
# We are NOT assuming what its rank should be.

smoke.rank <- which(
  importance.table$Variable == "scale.smoke"
)


cat(
  "\nscale.smoke importance rank:",
  smoke.rank,
  "out of",
  nrow(importance.table),
  "predictors\n"
)



# ------------------------------------------------------------
# 20. Create the results folder if necessary
# ------------------------------------------------------------
#
# recursive = TRUE allows R to create the directory structure
# if needed.
#
# showWarnings = FALSE prevents a warning if the folder already
# exists.

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 21. Save the variable importance results
# ------------------------------------------------------------

write.csv(
  importance.table,
  "results/moose_variable_importance.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 22. Plot permutation variable importance
# ------------------------------------------------------------
#
# reorder() places the variables in order of importance.
#
# coord_flip() makes the variable names easier to read.

importance.plot <- ggplot(
  importance.table,
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance - Moose",
    x = "Predictor",
    y = "Permutation Importance"
  ) +
  theme_minimal()


print(
  importance.plot
)



# ------------------------------------------------------------
# 23. Save the importance plot
# ------------------------------------------------------------

ggsave(
  filename = "results/moose_variable_importance.png",
  plot = importance.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PART 2:
# Fit a final model using ALL cleaned moose observations
# ============================================================
#
# The grouped train/test model above was used to evaluate how
# well the model generalizes to unseen camera sites.
#
# For our final interpretation of the PM2.5 relationship, we now
# refit the Random Forest using all available cleaned data.
#
# This gives the model access to the greatest amount of
# information when estimating the overall PM2.5 response.
#
# Cell_ID is again removed because it is only an identifier.
# ============================================================



# ------------------------------------------------------------
# 24. Create the full modeling dataset
# ------------------------------------------------------------

moose.full.model <- moose


moose.full.model$Cell_ID <- NULL



# ------------------------------------------------------------
# 25. Fit the final Random Forest using ALL observations
# ------------------------------------------------------------
#
# We intentionally keep the SAME model settings:
#
#   500 trees
#   min.node.size = 80
#   permutation importance
#
# This keeps our methodology consistent.

moose.rf.full <- ranger(
  Daily_Count ~ .,
  data = moose.full.model,
  num.trees = 500,
  min.node.size = 80,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)


cat(
  "\nFinal Random Forest using all cleaned moose data:\n"
)


print(
  moose.rf.full
)



# ============================================================
# PART 3:
# PM2.5 Partial Dependence
# ============================================================
#
# The partial-dependence curve is one of the most important
# outputs in this project.
#
#
# QUESTION:
#
# How do the model's predicted moose detections change as
# wildfire-attributable PM2.5 changes?
#
#
# BASIC IDEA:
#
# For each PM2.5 value:
#
#   1. Set scale.smoke to that value for EVERY observation.
#
#   2. Keep all other variables at their actual observed values.
#
#   3. Ask the Random Forest to predict Daily_Count.
#
#   4. Average those predictions.
#
#
# The resulting curve shows the relationship the Random Forest
# learned between wildfire PM2.5 and predicted daily moose
# detections while averaging over the other observed variables.
#
#
# IMPORTANT:
#
# Partial dependence shows a MODEL ASSOCIATION.
#
# It does NOT prove that smoke causes moose behavior to change.
# ============================================================



# ------------------------------------------------------------
# 26. Define the PM2.5 range used for partial dependence
# ------------------------------------------------------------
#
# The wildfire-smoke distribution is extremely right-skewed.
#
# Most camera-days have very little wildfire smoke, while a
# small number have extremely high values.
#
# To prevent a few extreme observations from dominating the
# plot, we use the:
#
#     1st percentile through 99th percentile
#
# This is the same approach we used for mule deer.

smoke.lower <- as.numeric(
  quantile(
    moose$scale.smoke,
    probs = 0.01
  )
)


smoke.upper <- as.numeric(
  quantile(
    moose$scale.smoke,
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
# 27. Create a sequence of PM2.5 values
# ------------------------------------------------------------
#
# We evaluate the model at 50 evenly spaced PM2.5 values
# between the 1st and 99th percentile.

smoke.grid <- seq(
  from = smoke.lower,
  to = smoke.upper,
  length.out = 50
)



# ------------------------------------------------------------
# 28. Create storage for partial-dependence predictions
# ------------------------------------------------------------
#
# This vector will eventually contain the average predicted
# Daily_Count at every PM2.5 value.

smoke.pd <- numeric(
  length(smoke.grid)
)



# ------------------------------------------------------------
# 29. Calculate partial dependence manually
# ------------------------------------------------------------
#
# For every value in smoke.grid:
#
#   - make a temporary copy of the data
#   - replace scale.smoke with the selected value
#   - predict Daily_Count
#   - average the predictions
#
# This gives us the model's average predicted moose detection
# count at each level of wildfire PM2.5.

for (i in seq_along(smoke.grid)) {
  
  
  # Create a temporary copy of the full predictor dataset.
  
  temporary.data <- moose.full.model
  
  
  # Set wildfire PM2.5 to one specific value for every
  # observation.
  
  temporary.data$scale.smoke <- smoke.grid[i]
  
  
  # Generate Random Forest predictions.
  
  temporary.predictions <- predict(
    moose.rf.full,
    data = temporary.data
  )$predictions
  
  
  # Store the average prediction.
  
  smoke.pd[i] <- mean(
    temporary.predictions
  )
}



# ------------------------------------------------------------
# 30. Put PM2.5 values and predictions into a table
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
# 31. Calculate the overall low-to-high PM2.5 response
# ------------------------------------------------------------
#
# We subtract the prediction at the LOW end of the modeled
# smoke range from the prediction at the HIGH end.
#
#
# If the result is:
#
#   positive -> predicted detections increased overall
#
#   negative -> predicted detections decreased overall
#
#   near zero -> little overall change
#
#
# The parent paper reported a NEGATIVE relationship for moose,
# but we do NOT force the Random Forest to reproduce that result.

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
  round(low.smoke.prediction, 6),
  "\n"
)


cat(
  "Predicted detections at HIGH modeled PM2.5:",
  round(high.smoke.prediction, 6),
  "\n"
)


cat(
  "Overall low-to-high PM2.5 change:",
  round(smoke.response.change, 6),
  "\n"
)



# ------------------------------------------------------------
# 32. Calculate relative percent change
# ------------------------------------------------------------
#
# This provides another way to describe the model's change.
#
# IMPORTANT:
#
# Moose detections are very rare, so the baseline predicted
# count may be extremely small.
#
# A large percentage change can therefore correspond to a very
# small absolute change in detections.
#
# For that reason, we should always report the ABSOLUTE change
# alongside the percentage change.

relative.change.percent <- (
  smoke.response.change /
    low.smoke.prediction
) * 100


cat(
  "Relative low-to-high change:",
  round(relative.change.percent, 2),
  "%\n"
)



# ------------------------------------------------------------
# 33. Plot the PM2.5 partial-dependence relationship
# ------------------------------------------------------------
#
# The x-axis shows wildfire-attributable PM2.5.
#
# The y-axis shows the Random Forest's average predicted daily
# moose detections.
#
# We are especially interested in whether the curve is:
#
#   increasing
#   decreasing
#   mostly flat
#   nonlinear

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
    title = "Moose Response to Wildfire PM2.5",
    subtitle = "Random Forest Partial Dependence",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "Predicted Daily Moose Detections"
  ) +
  theme_minimal()


print(
  smoke.plot
)



# ------------------------------------------------------------
# 34. Save the PM2.5 response data
# ------------------------------------------------------------

write.csv(
  smoke.response,
  "results/moose_pm25_partial_dependence.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 35. Save the PM2.5 response plot
# ------------------------------------------------------------

ggsave(
  filename = "results/moose_pm25_partial_dependence.png",
  plot = smoke.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ------------------------------------------------------------
# 36. Print a final summary
# ------------------------------------------------------------
#
# This gathers the most important values from the script into
# one place so they are easy to copy into our project notes.

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE RANDOM FOREST SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Cleaned observations:",
  nrow(moose),
  "\n"
)

cat(
  "Physical camera sites:",
  length(unique(moose$Cell_ID)),
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
  round(low.smoke.prediction, 6),
  "\n"
)

cat(
  "High-smoke predicted detections:",
  round(high.smoke.prediction, 6),
  "\n"
)

cat(
  "Low-to-high PM2.5 change:",
  round(smoke.response.change, 6),
  "\n"
)

cat(
  "Relative change:",
  round(relative.change.percent, 2),
  "%\n"
)

cat(
  "====================================================\n"
)