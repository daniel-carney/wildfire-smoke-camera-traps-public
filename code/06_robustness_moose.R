# ============================================================
# 06_robustness_moose.R
#
# PURPOSE:
#
# Test whether the relationship between wildfire-attributable
# PM2.5 and predicted daily moose detections is stable across
# different samples of physical camera sites.
#
#
# WHY THIS MATTERS:
#
# In 05_random_forest_moose.R, the Random Forest learned a
# generally POSITIVE PM2.5 relationship.
#
# This differs from the parent study, which reported a negative
# relationship for moose.
#
# Before interpreting this difference, we need to determine
# whether our Random Forest result is stable.
#
# A relationship that appears in only one particular sample of
# camera sites would not be very convincing.
#
#
# ROBUSTNESS STRATEGY:
#
# We will repeat the following process 30 times:
#
#   1. Randomly select 80% of physical camera sites
#   2. Keep all observations belonging to those sites
#   3. Fit the same Random Forest model
#   4. Calculate the PM2.5 partial-dependence curve
#   5. Record the overall low-to-high PM2.5 response
#
#
# IMPORTANT:
#
# Each repetition fits 500 trees.
#
# Therefore:
#
#     30 Random Forests x 500 trees
#
# equals 15,000 individual decision trees in this robustness
# analysis.
#
#
# We intentionally use the SAME model structure as before:
#
#     500 trees
#     min.node.size = 80
#     same predictors
#     same PM2.5 range
#
# We change only which camera sites are included.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load the cleaned moose dataset
# ------------------------------------------------------------
#
# This dataset was created by:
#
#     04_preprocessing_moose.R

moose <- read.csv(
  "data-split/moose_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# CSV files do not preserve R factor information.
#
# Therefore, we convert the categorical variables back into
# factors before fitting any Random Forest models.

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
# 4. Identify all physical camera sites
# ------------------------------------------------------------
#
# Cell_ID represents a physical camera site.
#
# Robustness sampling will happen at the SITE level rather than
# at the individual-row level.

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
# 5. Decide how many sites to include in each repetition
# ------------------------------------------------------------
#
# Each Random Forest will use 80% of all physical sites.
#
# This allows the exact combination of camera locations to
# change from one model to another.

sites.per.run <- floor(
  0.80 * number.sites
)


cat(
  "Physical camera sites used per run:",
  sites.per.run,
  "\n"
)



# ------------------------------------------------------------
# 6. Define the common PM2.5 range
# ------------------------------------------------------------
#
# We want every one of the 30 Random Forest models to be
# evaluated over the SAME smoke range.
#
# Otherwise, the response curves would be difficult to compare.
#
# As in the main model, we use the 1st through 99th percentile
# of the full moose smoke distribution.

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


smoke.grid <- seq(
  from = smoke.lower,
  to = smoke.upper,
  length.out = 50
)


cat(
  "\nCommon PM2.5 range:\n"
)

cat(
  "Lower:",
  smoke.lower,
  "\n"
)

cat(
  "Upper:",
  smoke.upper,
  "\n"
)



# ------------------------------------------------------------
# 7. Set the number of robustness repetitions
# ------------------------------------------------------------
#
# Thirty repetitions give us many different samples of camera
# sites while still keeping computation manageable.

number.runs <- 30



# ------------------------------------------------------------
# 8. Create storage for results
# ------------------------------------------------------------
#
# curve.results will hold the full PM2.5 response curve for
# every Random Forest.
#
# response.changes will hold only the overall low-to-high
# change for each model.

curve.results <- data.frame()


response.changes <- numeric(
  number.runs
)



# ============================================================
# 9. START THE 30-RUN ROBUSTNESS LOOP
# ============================================================

for (r in 1:number.runs) {
  
  
  # ----------------------------------------------------------
  # 9A. Set a different reproducible seed for each repetition
  # ----------------------------------------------------------
  #
  # Using 100 + r means:
  #
  # run 1 uses seed 101
  # run 2 uses seed 102
  # ...
  # run 30 uses seed 130
  #
  # This makes every run reproducible while still producing
  # different samples of camera sites.
  
  set.seed(
    100 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 9B. Randomly select 80% of physical camera sites
  # ----------------------------------------------------------
  
  selected.sites <- sample(
    all.sites,
    size = sites.per.run,
    replace = FALSE
  )
  
  
  
  # ----------------------------------------------------------
  # 9C. Keep observations from those sites
  # ----------------------------------------------------------
  
  run.data <- moose[
    moose$Cell_ID %in% selected.sites,
  ]
  
  
  
  # ----------------------------------------------------------
  # 9D. Remove Cell_ID before fitting the model
  # ----------------------------------------------------------
  #
  # Cell_ID is used to sample physical sites but should never be
  # used as a predictor.
  
  run.model.data <- run.data
  
  run.model.data$Cell_ID <- NULL
  
  
  
  # ----------------------------------------------------------
  # 9E. Fit the Random Forest
  # ----------------------------------------------------------
  #
  # These settings are intentionally identical to the main
  # moose Random Forest.
  
  run.rf <- ranger(
    Daily_Count ~ .,
    data = run.model.data,
    num.trees = 500,
    min.node.size = 80,
    respect.unordered.factors = "order",
    seed = 200 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 9F. Create storage for this run's partial dependence
  # ----------------------------------------------------------
  
  run.pd <- numeric(
    length(smoke.grid)
  )
  
  
  
  # ----------------------------------------------------------
  # 9G. Calculate PM2.5 partial dependence
  # ----------------------------------------------------------
  #
  # At each smoke value:
  #
  #   1. Replace scale.smoke with that value
  #   2. Keep all other variables unchanged
  #   3. Predict Daily_Count
  #   4. Average the predictions
  
  for (i in seq_along(smoke.grid)) {
    
    
    temporary.data <- run.model.data
    
    
    temporary.data$scale.smoke <- smoke.grid[i]
    
    
    temporary.predictions <- predict(
      run.rf,
      data = temporary.data
    )$predictions
    
    
    run.pd[i] <- mean(
      temporary.predictions
    )
  }
  
  
  
  # ----------------------------------------------------------
  # 9H. Calculate the overall PM2.5 response
  # ----------------------------------------------------------
  #
  # We measure:
  #
  # predicted detections at HIGH smoke
  #
  # minus
  #
  # predicted detections at LOW smoke
  #
  #
  # Positive = overall increase
  #
  # Negative = overall decrease
  
  run.response.change <-
    run.pd[length(run.pd)] -
    run.pd[1]
  
  
  
  # Save the response for this run.
  
  response.changes[r] <-
    run.response.change
  
  
  
  # ----------------------------------------------------------
  # 9I. Save the entire response curve
  # ----------------------------------------------------------
  
  run.curve <- data.frame(
    Run = r,
    scale.smoke = smoke.grid,
    predicted.daily.detections = run.pd
  )
  
  
  curve.results <- rbind(
    curve.results,
    run.curve
  )
  
  
  
  # ----------------------------------------------------------
  # 9J. Print progress
  # ----------------------------------------------------------
  #
  # This lets us see that the loop is working.
  
  cat(
    "Completed run",
    r,
    "of",
    number.runs,
    "- PM2.5 response:",
    round(run.response.change, 6),
    "\n"
  )
}



# ============================================================
# 10. SUMMARIZE THE 30 RESPONSE CURVES
# ============================================================
#
# At every PM2.5 value, we calculate:
#
#     mean predicted detections
#     2.5th percentile
#     97.5th percentile
#
#
# IMPORTANT:
#
# These percentiles describe variation across our different
# camera-site samples.
#
# They should be described as a:
#
#     resampling stability band
#
# rather than a formal statistical confidence interval.
# ============================================================


mean.curve <- aggregate(
  predicted.daily.detections ~ scale.smoke,
  data = curve.results,
  FUN = mean
)


names(mean.curve)[2] <-
  "mean.prediction"



lower.curve <- aggregate(
  predicted.daily.detections ~ scale.smoke,
  data = curve.results,
  FUN = function(x) {
    quantile(
      x,
      probs = 0.025
    )
  }
)


names(lower.curve)[2] <-
  "lower"



upper.curve <- aggregate(
  predicted.daily.detections ~ scale.smoke,
  data = curve.results,
  FUN = function(x) {
    quantile(
      x,
      probs = 0.975
    )
  }
)


names(upper.curve)[2] <-
  "upper"



# ------------------------------------------------------------
# 11. Combine the summary curves
# ------------------------------------------------------------

robustness.summary <- merge(
  mean.curve,
  lower.curve,
  by = "scale.smoke"
)


robustness.summary <- merge(
  robustness.summary,
  upper.curve,
  by = "scale.smoke"
)



cat(
  "\nRobustness Curve Summary:\n"
)


print(
  robustness.summary
)



# ============================================================
# 12. SUMMARIZE THE LOW-TO-HIGH RESPONSES
# ============================================================

cat(
  "\nPM2.5 response from each Random Forest:\n"
)


print(
  response.changes
)



cat(
  "\nSummary of low-to-high PM2.5 responses:\n"
)


print(
  summary(response.changes)
)



# ------------------------------------------------------------
# 13. Count positive, negative, and approximately flat responses
# ------------------------------------------------------------
#
# Because numerical calculations rarely equal exactly zero,
# we define a very small tolerance.
#
# Anything between -0.000001 and +0.000001 is considered
# approximately flat.

tolerance <- 0.000001


positive.runs <- sum(
  response.changes > tolerance
)


negative.runs <- sum(
  response.changes < -tolerance
)


flat.runs <- sum(
  abs(response.changes) <= tolerance
)



cat(
  "\nPositive PM2.5 responses:",
  positive.runs,
  "of",
  number.runs,
  "\n"
)


cat(
  "Negative PM2.5 responses:",
  negative.runs,
  "of",
  number.runs,
  "\n"
)


cat(
  "Approximately flat responses:",
  flat.runs,
  "of",
  number.runs,
  "\n"
)



# ------------------------------------------------------------
# 14. Calculate percentage of runs in each direction
# ------------------------------------------------------------

positive.percent <- (
  positive.runs /
    number.runs
) * 100


negative.percent <- (
  negative.runs /
    number.runs
) * 100


cat(
  "\nPercent positive:",
  round(positive.percent, 1),
  "%\n"
)


cat(
  "Percent negative:",
  round(negative.percent, 1),
  "%\n"
)



# ------------------------------------------------------------
# 15. Create results folder if necessary
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 16. Save all individual PM2.5 curves
# ------------------------------------------------------------

write.csv(
  curve.results,
  "results/moose_robustness_all_curves.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 17. Save the mean curve and stability band
# ------------------------------------------------------------

write.csv(
  robustness.summary,
  "results/moose_robustness_summary.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 18. Save the 30 response changes
# ------------------------------------------------------------

response.table <- data.frame(
  Run = 1:number.runs,
  PM25_Response_Change = response.changes
)


write.csv(
  response.table,
  "results/moose_robustness_response_changes.csv",
  row.names = FALSE
)



# ============================================================
# 19. PLOT THE ROBUSTNESS CURVE
# ============================================================
#
# Black line:
#
#     Average PM2.5 partial-dependence curve across all 30
#     Random Forest models.
#
# Gray area:
#
#     2.5th to 97.5th percentile across site samples.
#
# This shows both:
#
#     direction
#
# and
#
#     stability
#
# of the model's PM2.5 relationship.
# ============================================================

robustness.plot <- ggplot(
  robustness.summary,
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
    linewidth = 1
  ) +
  
  labs(
    title = "Robustness of Moose PM2.5 Response",
    subtitle = "30 Random Forest Fits Using Different Camera-Site Samples",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "Predicted Daily Moose Detections"
  ) +
  
  theme_minimal()


print(
  robustness.plot
)



# ------------------------------------------------------------
# 20. Save the robustness plot
# ------------------------------------------------------------

ggsave(
  filename = "results/moose_pm25_robustness.png",
  plot = robustness.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# 21. PLOT DISTRIBUTION OF THE 30 RESPONSE CHANGES
# ============================================================
#
# This histogram provides another way to see how consistent
# the PM2.5 relationship is.
#
# Values greater than zero represent positive relationships.
#
# Values less than zero represent negative relationships.
#
# The dashed vertical line at zero separates the two.
# ============================================================

response.plot <- ggplot(
  response.table,
  aes(
    x = PM25_Response_Change
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
    title = "Distribution of PM2.5 Response Across Random Forest Fits - Moose",
    subtitle = "30 Different Camera-Site Samples",
    x = "High-Smoke Prediction Minus Low-Smoke Prediction",
    y = "Number of Random Forest Fits"
  ) +
  
  theme_minimal()


print(
  response.plot
)



# ------------------------------------------------------------
# 22. Save the response distribution plot
# ------------------------------------------------------------

ggsave(
  filename = "results/moose_pm25_response_distribution.png",
  plot = response.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# 23. PRINT FINAL ROBUSTNESS SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE PM2.5 ROBUSTNESS SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Random Forest models:",
  number.runs,
  "\n"
)

cat(
  "Trees per model: 500\n"
)

cat(
  "Sites per model:",
  sites.per.run,
  "\n"
)

cat(
  "Positive responses:",
  positive.runs,
  "of",
  number.runs,
  "\n"
)

cat(
  "Negative responses:",
  negative.runs,
  "of",
  number.runs,
  "\n"
)

cat(
  "Flat responses:",
  flat.runs,
  "of",
  number.runs,
  "\n"
)

cat(
  "Mean PM2.5 response:",
  round(
    mean(response.changes),
    6
  ),
  "\n"
)

cat(
  "Median PM2.5 response:",
  round(
    median(response.changes),
    6
  ),
  "\n"
)

cat(
  "Minimum PM2.5 response:",
  round(
    min(response.changes),
    6
  ),
  "\n"
)

cat(
  "Maximum PM2.5 response:",
  round(
    max(response.changes),
    6
  ),
  "\n"
)

cat(
  "====================================================\n"
)