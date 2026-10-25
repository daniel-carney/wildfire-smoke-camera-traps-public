# ============================================================
# 03_robustness_mule_deer.R
#
# Robustness analysis of the Random Forest relationship between
# wildfire-attributable PM2.5 and mule deer daily detections.
#
# The goal is to determine whether the positive PM2.5 response
# found in the original model is stable across different
# samples of physical camera sites.
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
# 2. Load cleaned mule deer data
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
# 4. Basic verification
# ------------------------------------------------------------

dim(mule.model)

length(
  unique(mule.model$Cell_ID)
)


# ------------------------------------------------------------
# 5. Create common PM2.5 range
# ------------------------------------------------------------
# We use the same 1st to 99th percentile range used in the main
# Random Forest analysis.
#
# Every resampled model will be evaluated over this same range.

smoke.low <- as.numeric(
  quantile(
    mule.model$scale.smoke,
    0.01
  )
)

smoke.high <- as.numeric(
  quantile(
    mule.model$scale.smoke,
    0.99
  )
)


smoke.grid <- seq(
  from = smoke.low,
  to = smoke.high,
  length.out = 75
)


# ------------------------------------------------------------
# 6. Set robustness-analysis parameters
# ------------------------------------------------------------

n.repeats <- 30

all.sites <- unique(
  mule.model$Cell_ID
)

n.train.sites <- floor(
  0.80 * length(all.sites)
)


# Matrix for storing PM2.5 response curves
#
# Rows = PM2.5 values
# Columns = Random Forest repetitions

response.matrix <- matrix(
  NA,
  nrow = length(smoke.grid),
  ncol = n.repeats
)


# Store overall low-to-high PM2.5 change from each model

response.change <- numeric(
  n.repeats
)


# ------------------------------------------------------------
# 7. Repeated grouped Random Forest fitting
# ------------------------------------------------------------
#
# Each repetition:
#
# 1. Randomly selects 80% of physical camera sites.
# 2. Fits a Random Forest using those sites.
# 3. Calculates the PM2.5 partial-dependence curve.
# 4. Saves the curve and overall change.
#
# This tells us whether the PM2.5 response remains similar when
# different camera sites are used to fit the model.

for (r in 1:n.repeats) {
  
  cat(
    "Running repetition",
    r,
    "of",
    n.repeats,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Select 80% of physical sites
  # ----------------------------------------------------------
  
  set.seed(
    100 + r
  )
  
  selected.sites <- sample(
    all.sites,
    size = n.train.sites,
    replace = FALSE
  )
  
  
  sample.data <- mule.model[
    mule.model$Cell_ID %in% selected.sites,
  ]
  
  
  # ----------------------------------------------------------
  # Remove Cell_ID from model predictors
  # ----------------------------------------------------------
  
  sample.model.data <- sample.data[
    , names(sample.data) != "Cell_ID"
  ]
  
  
  # ----------------------------------------------------------
  # Fit Random Forest
  # ----------------------------------------------------------
  
  rf.temp <- ranger(
    Daily_Count ~ .,
    data = sample.model.data,
    num.trees = 500,
    min.node.size = 80,
    importance = "none",
    respect.unordered.factors = "order",
    seed = 100 + r
  )
  
  
  # ----------------------------------------------------------
  # Calculate PM2.5 partial dependence
  # ----------------------------------------------------------
  
  temp.response <- numeric(
    length(smoke.grid)
  )
  
  
  for (i in seq_along(smoke.grid)) {
    
    prediction.data <- sample.model.data
    
    prediction.data$scale.smoke <- smoke.grid[i]
    
    
    temp.predictions <- predict(
      rf.temp,
      data = prediction.data
    )$predictions
    
    
    temp.response[i] <- mean(
      temp.predictions
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Save this model's response curve
  # ----------------------------------------------------------
  
  response.matrix[
    ,
    r
  ] <- temp.response
  
  
  # ----------------------------------------------------------
  # Save low-to-high PM2.5 change
  # ----------------------------------------------------------
  
  response.change[r] <-
    temp.response[
      length(temp.response)
    ] -
    temp.response[1]
  
}


# ------------------------------------------------------------
# 8. Summarize the 30 PM2.5 response curves
# ------------------------------------------------------------

response.mean <- apply(
  response.matrix,
  1,
  mean
)

response.lower <- apply(
  response.matrix,
  1,
  quantile,
  probs = 0.025
)

response.upper <- apply(
  response.matrix,
  1,
  quantile,
  probs = 0.975
)


robustness.response <- data.frame(
  scale.smoke = smoke.grid,
  mean.prediction = response.mean,
  lower = response.lower,
  upper = response.upper
)


print(
  robustness.response
)


# ------------------------------------------------------------
# 9. Plot robustness PM2.5 response
# ------------------------------------------------------------
#
# The line is the average relationship across the 30 forests.
#
# The shaded region shows the range containing approximately
# 95% of the fitted response curves.

ggplot(
  robustness.response,
  aes(
    x = scale.smoke,
    y = mean.prediction
  )
) +
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.25
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  labs(
    title = "Robustness of Mule Deer PM2.5 Response",
    subtitle = "30 Random Forest Fits Using Different Camera-Site Samples",
    x = "Scaled Wildfire PM2.5",
    y = "Predicted Daily Mule Deer Detections"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 10. Examine low-to-high change across repetitions
# ------------------------------------------------------------

response.change


summary(
  response.change
)


# ------------------------------------------------------------
# 11. Count positive and negative responses
# ------------------------------------------------------------

positive.responses <- sum(
  response.change > 0
)

negative.responses <- sum(
  response.change < 0
)

zero.responses <- sum(
  response.change == 0
)


cat(
  "\nROBUSTNESS SUMMARY\n"
)

cat(
  "-------------------------\n"
)

cat(
  "Number of repetitions:",
  n.repeats,
  "\n"
)

cat(
  "Positive PM2.5 relationships:",
  positive.responses,
  "\n"
)

cat(
  "Negative PM2.5 relationships:",
  negative.responses,
  "\n"
)

cat(
  "Exactly flat relationships:",
  zero.responses,
  "\n"
)


cat(
  "Percent positive:",
  round(
    positive.responses /
      n.repeats *
      100,
    1
  ),
  "%\n"
)


cat(
  "Mean low-to-high change:",
  round(
    mean(response.change),
    4
  ),
  "\n"
)


cat(
  "Median low-to-high change:",
  round(
    median(response.change),
    4
  ),
  "\n"
)


cat(
  "Minimum low-to-high change:",
  round(
    min(response.change),
    4
  ),
  "\n"
)


cat(
  "Maximum low-to-high change:",
  round(
    max(response.change),
    4
  ),
  "\n"
)


# ------------------------------------------------------------
# 12. Plot distribution of response changes
# ------------------------------------------------------------

response.change.df <- data.frame(
  change = response.change
)


ggplot(
  response.change.df,
  aes(
    x = change
  )
) +
  geom_histogram(
    bins = 10
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of PM2.5 Response Across Random Forest Fits",
    subtitle = "Mule Deer",
    x = "Change in Predicted Daily Detections: Low to High PM2.5",
    y = "Number of Random Forest Fits"
  ) +
  theme_minimal()