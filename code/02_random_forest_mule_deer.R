# ============================================================
# 02_random_forest_mule_deer.R
#
# Random Forest analysis of mule deer daily detections
# and wildfire-attributable PM2.5.
# ============================================================


# ------------------------------------------------------------
# 1. Install/load required packages
# ------------------------------------------------------------

if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ranger)
library(ggplot2)


# ------------------------------------------------------------
# 2. Load cleaned modeling data
# ------------------------------------------------------------

mule.model <- read.csv(
  "data-split/mule_deer_model_data.csv"
)


# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------

mule.model$Year <- as.factor(
  mule.model$Year
)

mule.model$landcover <- as.factor(
  mule.model$landcover
)

mule.model$Habitat_Type <- as.factor(
  mule.model$Habitat_Type
)

mule.model$Road_Type <- as.factor(
  mule.model$Road_Type
)

mule.model$Land_Mgnt <- as.factor(
  mule.model$Land_Mgnt
)

mule.model$conifer <- as.factor(
  mule.model$conifer
)


# ------------------------------------------------------------
# 4. Verify loaded dataset
# ------------------------------------------------------------

dim(mule.model)

names(mule.model)

str(mule.model)

colSums(is.na(mule.model))


# ------------------------------------------------------------
# 5. Create grouped train/test split
# ------------------------------------------------------------

set.seed(42)

sites <- unique(
  mule.model$Cell_ID
)

train.sites <- sample(
  sites,
  size = floor(0.80 * length(sites)),
  replace = FALSE
)


train <- mule.model[
  mule.model$Cell_ID %in% train.sites,
]

test <- mule.model[
  !(mule.model$Cell_ID %in% train.sites),
]


# ------------------------------------------------------------
# 6. Check train/test split
# ------------------------------------------------------------

cat(
  "Training rows:",
  nrow(train),
  "\n"
)

cat(
  "Testing rows:",
  nrow(test),
  "\n"
)

cat(
  "Training sites:",
  length(unique(train$Cell_ID)),
  "\n"
)

cat(
  "Testing sites:",
  length(unique(test$Cell_ID)),
  "\n"
)


shared.sites <- intersect(
  unique(train$Cell_ID),
  unique(test$Cell_ID)
)

shared.sites


# ------------------------------------------------------------
# 7. Remove Cell_ID from model predictors
# ------------------------------------------------------------

train.model <- train[
  , names(train) != "Cell_ID"
]

test.model <- test[
  , names(test) != "Cell_ID"
]


# ------------------------------------------------------------
# 8. Fit Random Forest regression model
# ------------------------------------------------------------

set.seed(42)

rf.mule <- ranger(
  Daily_Count ~ .,
  data = train.model,
  num.trees = 500,
  min.node.size = 80,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)


rf.mule


# ------------------------------------------------------------
# 9. Predict testing data
# ------------------------------------------------------------

test.predictions <- predict(
  rf.mule,
  data = test.model
)$predictions


head(test.predictions)


# ------------------------------------------------------------
# 10. Calculate basic model performance
# ------------------------------------------------------------

actual <- test.model$Daily_Count


MAE <- mean(
  abs(
    actual - test.predictions
  )
)


RMSE <- sqrt(
  mean(
    (actual - test.predictions)^2
  )
)


R2 <- 1 -
  (
    sum(
      (actual - test.predictions)^2
    )
    /
      sum(
        (actual - mean(actual))^2
      )
  )


cat(
  "\nMODEL PERFORMANCE\n"
)

cat(
  "-------------------------\n"
)

cat(
  "MAE:",
  round(MAE, 4),
  "\n"
)

cat(
  "RMSE:",
  round(RMSE, 4),
  "\n"
)

cat(
  "R-squared:",
  round(R2, 4),
  "\n"
)


# ------------------------------------------------------------
# 11. Examine permutation variable importance
# ------------------------------------------------------------
# This shows how much the fitted model relies on each predictor.
#
# PM2.5 is represented by:
# scale.smoke
#
# Importance alone does NOT tell us whether the relationship is
# positive or negative. It only tells us whether changing the
# variable disrupts model predictions.

importance.values <- rf.mule$variable.importance

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


print(importance.table)


# Specifically examine wildfire PM2.5

importance.table[
  importance.table$Variable == "scale.smoke",
]


# Plot feature importance

ggplot(
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
    title = "Random Forest Permutation Importance",
    subtitle = "Mule Deer Daily Detection Model",
    x = "Predictor",
    y = "Permutation Importance"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 12. Fit final Random Forest using all cleaned data
# ------------------------------------------------------------
# The train/test model above was used to validate the approach.
#
# For interpretation of the PM2.5 relationship, we now fit the
# same model using all available cleaned mule-deer observations.
#
# Cell_ID is still excluded because it is an identifier.

final.model.data <- mule.model[
  , names(mule.model) != "Cell_ID"
]


set.seed(42)

rf.mule.final <- ranger(
  Daily_Count ~ .,
  data = final.model.data,
  num.trees = 500,
  min.node.size = 80,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)


rf.mule.final


# ------------------------------------------------------------
# 13. Create PM2.5 range for interpretation
# ------------------------------------------------------------
# We use the 1st through 99th percentiles rather than the
# absolute minimum and maximum.
#
# This reduces the chance that a very small number of extreme
# smoke observations dominates the response graph.

smoke.low <- as.numeric(
  quantile(
    final.model.data$scale.smoke,
    0.01
  )
)

smoke.high <- as.numeric(
  quantile(
    final.model.data$scale.smoke,
    0.99
  )
)


smoke.grid <- seq(
  from = smoke.low,
  to = smoke.high,
  length.out = 75
)


# ------------------------------------------------------------
# 14. Calculate PM2.5 partial dependence
# ------------------------------------------------------------
#
# For every PM2.5 level:
#
# 1. Keep all other observed variables unchanged.
# 2. Set scale.smoke to the same value for every observation.
# 3. Predict Daily_Count.
# 4. Average those predictions.
#
# This shows the average relationship learned by the Random
# Forest between wildfire PM2.5 and mule-deer detections.

smoke.predictions <- numeric(
  length(smoke.grid)
)


for (i in seq_along(smoke.grid)) {
  
  temp.data <- final.model.data
  
  temp.data$scale.smoke <- smoke.grid[i]
  
  
  predictions <- predict(
    rf.mule.final,
    data = temp.data
  )$predictions
  
  
  smoke.predictions[i] <- mean(
    predictions
  )
}


pm25.response <- data.frame(
  scale.smoke = smoke.grid,
  predicted.detections = smoke.predictions
)


print(
  pm25.response
)


# ------------------------------------------------------------
# 15. Plot learned PM2.5 relationship
# ------------------------------------------------------------

ggplot(
  pm25.response,
  aes(
    x = scale.smoke,
    y = predicted.detections
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  labs(
    title = "Mule Deer Response to Wildfire PM2.5",
    subtitle = "Random Forest Partial Dependence",
    x = "Scaled Wildfire PM2.5",
    y = "Predicted Daily Mule Deer Detections"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 16. Plot distribution of actual wildfire PM2.5 observations
# ------------------------------------------------------------
# This tells us where most of the actual data occur.
#
# We should be cautious about interpreting portions of the
# response curve where very few observations exist.

ggplot(
  final.model.data,
  aes(
    x = scale.smoke
  )
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Wildfire PM2.5",
    subtitle = "Mule Deer Camera-Days",
    x = "Scaled Wildfire PM2.5",
    y = "Number of Camera-Days"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 17. Helpful numerical summary of PM2.5 response
# ------------------------------------------------------------

cat(
  "\nPM2.5 RESPONSE SUMMARY\n"
)

cat(
  "-------------------------\n"
)

cat(
  "Lowest modeled smoke:",
  round(
    min(pm25.response$scale.smoke),
    4
  ),
  "\n"
)

cat(
  "Predicted detections at low smoke:",
  round(
    pm25.response$predicted.detections[1],
    4
  ),
  "\n"
)

cat(
  "Highest modeled smoke:",
  round(
    max(pm25.response$scale.smoke),
    4
  ),
  "\n"
)

cat(
  "Predicted detections at high smoke:",
  round(
    pm25.response$predicted.detections[
      nrow(pm25.response)
    ],
    4
  ),
  "\n"
)

cat(
  "Overall predicted change:",
  round(
    pm25.response$predicted.detections[
      nrow(pm25.response)
    ] -
      pm25.response$predicted.detections[1],
    4
  ),
  "\n"
)