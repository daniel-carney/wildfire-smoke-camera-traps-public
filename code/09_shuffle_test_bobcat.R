# ============================================================
# 09_shuffle_test_bobcat.R
#
# PURPOSE:
#
# Test whether our Random Forest + partial-dependence pipeline
# has a built-in tendency to create a positive PM2.5 response.
#
#
# WHY WE ARE DOING THIS:
#
# So far, our Random Forest results have been:
#
#   Mule deer -> positive PM2.5 relationship
#   Moose     -> positive PM2.5 relationship
#   Bobcat    -> positive PM2.5 relationship
#
# The parent paper reported:
#
#   Mule deer -> positive
#   Moose     -> negative
#   Bobcat    -> negative
#
# This raises an important question:
#
# Is our Random Forest pipeline somehow producing positive
# smoke relationships even when no real relationship exists?
#
#
# SHUFFLE TEST IDEA:
#
# We randomly shuffle the scale.smoke column.
#
# Shuffling destroys the real connection between:
#
#     wildfire PM2.5
#
# and
#
#     bobcat detections / environmental context
#
# while leaving the distribution of smoke values unchanged.
#
#
# If the pipeline is unbiased:
#
#     shuffled PM2.5 responses should usually be close to zero
#     and should include BOTH positive and negative directions.
#
#
# If the pipeline itself strongly favors positive responses:
#
#     the shuffled models may continue producing mostly
#     positive PM2.5 curves.
#
#
# We will repeat the shuffle test 30 times.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load cleaned bobcat data
# ------------------------------------------------------------
#
# This dataset was created by:
#
#     07_preprocessing_bobcat.R

bobcat <- read.csv(
  "data-split/bobcat_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------
#
# CSV files do not preserve factor information, so we restore
# the same categorical variables used in the main model.

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
# 4. Remove Cell_ID before modeling
# ------------------------------------------------------------
#
# Cell_ID is only an identifier.
#
# We do not want the Random Forest to use site identity as a
# predictor.

bobcat.model <- bobcat

bobcat.model$Cell_ID <- NULL



# ------------------------------------------------------------
# 5. Define the SAME PM2.5 range used in the real analysis
# ------------------------------------------------------------
#
# We use the same 1st through 99th percentile range.
#
# This keeps the shuffle test directly comparable with the
# original bobcat partial-dependence result.

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


smoke.grid <- seq(
  from = smoke.lower,
  to = smoke.upper,
  length.out = 50
)


cat(
  "\nPM2.5 range used for shuffle test:\n"
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
# 6. Store the ORIGINAL bobcat PM2.5 response
# ------------------------------------------------------------
#
# From our real bobcat model:
#
# low-to-high PM2.5 change = approximately +0.004513
#
# We save that value so we can compare the shuffled models
# directly against the real result.

observed.response <- 0.004513



# ------------------------------------------------------------
# 7. Set number of shuffle repetitions
# ------------------------------------------------------------
#
# We will fit 30 Random Forests.
#
# Each one receives a different shuffled version of scale.smoke.

number.runs <- 30



# ------------------------------------------------------------
# 8. Create storage for results
# ------------------------------------------------------------
#
# response.changes:
#     stores the overall low-to-high PM2.5 change for every
#     shuffled model.
#
# curve.results:
#     stores the entire partial-dependence curve for every run.

response.changes <- numeric(
  number.runs
)


curve.results <- data.frame()



# ============================================================
# 9. START SHUFFLE TEST
# ============================================================

for (r in 1:number.runs) {
  
  
  # ----------------------------------------------------------
  # 9A. Set a reproducible seed
  # ----------------------------------------------------------
  #
  # Each repetition gets a different seed so we generate a
  # different shuffled smoke column.
  
  set.seed(
    500 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 9B. Create a copy of the dataset
  # ----------------------------------------------------------
  
  shuffled.data <- bobcat.model
  
  
  
  # ----------------------------------------------------------
  # 9C. SHUFFLE scale.smoke
  # ----------------------------------------------------------
  #
  # sample() randomly rearranges the existing PM2.5 values.
  #
  # IMPORTANT:
  #
  # We are NOT creating new smoke values.
  #
  # The distribution remains exactly the same.
  #
  # We are only breaking the connection between the smoke value
  # and the camera-day where it originally occurred.
  
  shuffled.data$scale.smoke <- sample(
    shuffled.data$scale.smoke,
    replace = FALSE
  )
  
  
  
  # ----------------------------------------------------------
  # 9D. Fit Random Forest to shuffled data
  # ----------------------------------------------------------
  #
  # We intentionally keep the same settings:
  #
  #     500 trees
  #     min.node.size = 80
  #     same predictors
  #
  # The only difference is that PM2.5 has been randomized.
  
  shuffled.rf <- ranger(
    Daily_Count ~ .,
    data = shuffled.data,
    num.trees = 500,
    min.node.size = 80,
    respect.unordered.factors = "order",
    seed = 700 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 9E. Create storage for this run's PDP
  # ----------------------------------------------------------
  
  run.pd <- numeric(
    length(smoke.grid)
  )
  
  
  
  # ----------------------------------------------------------
  # 9F. Calculate partial dependence
  # ----------------------------------------------------------
  #
  # This is deliberately identical to the original analysis.
  
  for (i in seq_along(smoke.grid)) {
    
    
    temporary.data <- shuffled.data
    
    
    # Set all observations to the same PM2.5 value.
    
    temporary.data$scale.smoke <-
      smoke.grid[i]
    
    
    # Generate predictions.
    
    temporary.predictions <- predict(
      shuffled.rf,
      data = temporary.data
    )$predictions
    
    
    # Average the predicted Daily_Count values.
    
    run.pd[i] <- mean(
      temporary.predictions
    )
  }
  
  
  
  # ----------------------------------------------------------
  # 9G. Calculate low-to-high smoke response
  # ----------------------------------------------------------
  #
  # Same calculation used in our real models.
  
  run.response.change <-
    run.pd[length(run.pd)] -
    run.pd[1]
  
  
  response.changes[r] <-
    run.response.change
  
  
  
  # ----------------------------------------------------------
  # 9H. Save entire curve
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
  # 9I. Print progress
  # ----------------------------------------------------------
  
  cat(
    "Completed shuffle run",
    r,
    "of",
    number.runs,
    "- PM2.5 response:",
    round(
      run.response.change,
      6
    ),
    "\n"
  )
}



# ============================================================
# 10. SUMMARIZE SHUFFLED RESPONSES
# ============================================================

cat(
  "\nShuffled PM2.5 responses:\n"
)


print(
  response.changes
)


cat(
  "\nSummary of shuffled responses:\n"
)


print(
  summary(response.changes)
)



# ------------------------------------------------------------
# 11. Count positive and negative shuffled responses
# ------------------------------------------------------------
#
# We use a tiny tolerance around zero.

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
  "\nPositive shuffled responses:",
  positive.runs,
  "of",
  number.runs,
  "\n"
)


cat(
  "Negative shuffled responses:",
  negative.runs,
  "of",
  number.runs,
  "\n"
)


cat(
  "Approximately flat shuffled responses:",
  flat.runs,
  "of",
  number.runs,
  "\n"
)



# ------------------------------------------------------------
# 12. Calculate mean shuffled response
# ------------------------------------------------------------
#
# If the shuffle test behaves properly, this should generally be
# close to zero.

mean.shuffled.response <- mean(
  response.changes
)


cat(
  "\nMean shuffled PM2.5 response:",
  round(
    mean.shuffled.response,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 13. Compare real bobcat result with shuffled results
# ------------------------------------------------------------
#
# The real bobcat model produced:
#
#     +0.004513
#
# We now ask how many shuffled models produced a response as
# large or larger than the observed one.

as.large.as.observed <- sum(
  response.changes >= observed.response
)


cat(
  "\nShuffled responses >= observed bobcat response:",
  as.large.as.observed,
  "of",
  number.runs,
  "\n"
)



# ------------------------------------------------------------
# 14. Calculate simple empirical proportion
# ------------------------------------------------------------
#
# This is NOT being treated as a formal hypothesis-test p-value.
#
# It is simply a useful descriptive comparison showing how often
# the shuffled models produced a response as large as the real
# bobcat model.

empirical.proportion <-
  as.large.as.observed /
  number.runs


cat(
  "Proportion of shuffled responses >= observed:",
  round(
    empirical.proportion,
    3
  ),
  "\n"
)



# ============================================================
# 15. SUMMARIZE THE SHUFFLED PARTIAL-DEPENDENCE CURVES
# ============================================================



# Mean prediction at every PM2.5 level.

mean.curve <- aggregate(
  predicted.daily.detections ~ scale.smoke,
  data = curve.results,
  FUN = mean
)


names(mean.curve)[2] <-
  "mean.prediction"



# Lower 2.5th percentile across shuffled models.

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



# Upper 97.5th percentile.

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



# Combine summaries.

shuffle.summary <- merge(
  mean.curve,
  lower.curve,
  by = "scale.smoke"
)


shuffle.summary <- merge(
  shuffle.summary,
  upper.curve,
  by = "scale.smoke"
)



# ------------------------------------------------------------
# 16. Create results folder if necessary
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 17. Save response values
# ------------------------------------------------------------

shuffle.response.table <- data.frame(
  Run = 1:number.runs,
  Shuffled_PM25_Response = response.changes
)


write.csv(
  shuffle.response.table,
  "results/bobcat_shuffle_responses.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 18. Save shuffled curve summary
# ------------------------------------------------------------

write.csv(
  shuffle.summary,
  "results/bobcat_shuffle_curve_summary.csv",
  row.names = FALSE
)



# ============================================================
# 19. PLOT THE DISTRIBUTION OF SHUFFLED RESPONSES
# ============================================================
#
# This is probably the MOST IMPORTANT shuffle-test plot.
#
#
# Dashed line at 0:
#
#     no PM2.5 response
#
#
# Solid vertical line:
#
#     real bobcat PM2.5 response (+0.004513)
#
#
# If the shuffle results cluster around zero and the real value
# sits far to the right, that suggests the positive result is
# not simply produced automatically by the pipeline.
# ============================================================

shuffle.histogram <- ggplot(
  shuffle.response.table,
  aes(
    x = Shuffled_PM25_Response
  )
) +
  
  geom_histogram(
    bins = 10
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = observed.response,
    linewidth = 1
  ) +
  
  labs(
    title = "Bobcat PM2.5 Shuffle Test",
    subtitle = "30 Random Forest Fits After Randomizing PM2.5",
    x = "High-Smoke Prediction Minus Low-Smoke Prediction",
    y = "Number of Random Forest Fits"
  ) +
  
  theme_minimal()


print(
  shuffle.histogram
)



# ------------------------------------------------------------
# 20. Save shuffle histogram
# ------------------------------------------------------------

ggsave(
  filename = "results/bobcat_pm25_shuffle_histogram.png",
  plot = shuffle.histogram,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# 21. PLOT THE AVERAGE SHUFFLED PDP
# ============================================================
#
# If the pipeline itself creates positive PM2.5 curves, the mean
# shuffled response may still visibly increase.
#
# Ideally, this curve should be comparatively flat.
# ============================================================

shuffle.curve.plot <- ggplot(
  shuffle.summary,
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
    title = "Bobcat PM2.5 Response After Shuffling Smoke",
    subtitle = "Mean of 30 Random Forest Fits",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "Predicted Daily Bobcat Detections"
  ) +
  
  theme_minimal()


print(
  shuffle.curve.plot
)



# ------------------------------------------------------------
# 22. Save mean shuffled curve
# ------------------------------------------------------------

ggsave(
  filename = "results/bobcat_pm25_shuffle_curve.png",
  plot = shuffle.curve.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# 23. PRINT FINAL SHUFFLE-TEST SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "BOBCAT PM2.5 SHUFFLE TEST SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Shuffle models:",
  number.runs,
  "\n"
)

cat(
  "Trees per model: 500\n"
)

cat(
  "Observed bobcat response:",
  observed.response,
  "\n"
)

cat(
  "Mean shuffled response:",
  round(
    mean.shuffled.response,
    6
  ),
  "\n"
)

cat(
  "Positive shuffled responses:",
  positive.runs,
  "\n"
)

cat(
  "Negative shuffled responses:",
  negative.runs,
  "\n"
)

cat(
  "Flat shuffled responses:",
  flat.runs,
  "\n"
)

cat(
  "Shuffled responses >= observed:",
  as.large.as.observed,
  "\n"
)

cat(
  "Empirical proportion:",
  round(
    empirical.proportion,
    3
  ),
  "\n"
)

cat(
  "====================================================\n"
)