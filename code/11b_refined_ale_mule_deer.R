# ============================================================
# 11b_refined_ale_mule_deer.R
#
# VERSION 2 - REFINED ALE ANALYSIS
#
# PURPOSE:
#
# Recalculate the mule deer PM2.5 ALE analysis after discovering
# an important problem with our first ALE implementation.
#
#
# WHAT WAS WRONG WITH THE FIRST ALE?
#
# The PM2.5 distribution is extremely skewed.
#
# Most camera-days have very low wildfire-attributable PM2.5,
# while relatively few observations have high smoke values.
#
# Our original quantile bins therefore produced:
#
#     many extremely narrow low-smoke bins
#
# and
#
#     one extremely wide high-smoke bin.
#
# For example, the final detection-model ALE interval covered
# approximately:
#
#     0.58 to 12.14
#
# That is NOT a very "local" comparison.
#
# Most of the apparent positive ALE response came from that
# single enormous final interval.
#
#
# REFINEMENTS IN THIS SCRIPT:
#
# 1. Restrict interpretation to the 1st-99th percentile of
#    observed PM2.5.
#
# 2. Use additional quantile boundaries in the upper tail.
#
# 3. Print bin width and number of observations for every bin.
#
# 4. Measure how much of the total ALE change is contributed by
#    the single largest local effect.
#
# 5. Use the exact same PM2.5 range for the observed-data
#    diagnostic plots.
#
#
# IMPORTANT:
#
# We are NOT changing the Random Forest model because we dislike
# its result.
#
# We are correcting a diagnostic problem that was visible in
# the structure of the ALE bins.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load cleaned mule deer data
# ------------------------------------------------------------

mule <- read.csv(
  "data-split/mule_deer_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

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
# 4. Re-create binary Detection target
# ------------------------------------------------------------

mule$Detection <- ifelse(
  mule$Daily_Count > 0,
  "Yes",
  "No"
)


mule$Detection <- factor(
  mule$Detection,
  levels = c(
    "No",
    "Yes"
  )
)



# ============================================================
# DEFINE CENTRAL PM2.5 RANGE
# ============================================================



# ------------------------------------------------------------
# 5. Calculate the 1st and 99th percentile
# ------------------------------------------------------------
#
# This prevents a very small number of extreme observations from
# determining the shape of the ALE curve.

smoke.lower <- as.numeric(
  quantile(
    mule$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)


smoke.upper <- as.numeric(
  quantile(
    mule$scale.smoke,
    probs = 0.99,
    na.rm = TRUE
  )
)


cat(
  "\n====================================================\n"
)

cat(
  "REFINED PM2.5 INTERPRETATION RANGE\n"
)

cat(
  "====================================================\n"
)

cat(
  "1st percentile:",
  smoke.lower,
  "\n"
)

cat(
  "99th percentile:",
  smoke.upper,
  "\n"
)



# ------------------------------------------------------------
# 6. Keep observations inside the central smoke range
# ------------------------------------------------------------
#
# IMPORTANT:
#
# The Random Forest will still be fit using ALL cleaned data.
#
# We are only limiting the range over which we INTERPRET the
# PM2.5 response.

mule.central <- mule[
  mule$scale.smoke >= smoke.lower &
    mule$scale.smoke <= smoke.upper,
]


cat(
  "\nOriginal observations:",
  nrow(mule),
  "\n"
)

cat(
  "Observations inside 1st-99th percentile:",
  nrow(mule.central),
  "\n"
)

cat(
  "Observations excluded only from interpretation:",
  nrow(mule) - nrow(mule.central),
  "\n"
)



# ============================================================
# FIT FULL TWO-PART MODELS
# ============================================================



# ------------------------------------------------------------
# 7. Prepare full detection-model data
# ------------------------------------------------------------

detection.full <- mule


detection.full$Cell_ID <- NULL

detection.full$Daily_Count <- NULL



# ------------------------------------------------------------
# 8. Fit full probability Random Forest
# ------------------------------------------------------------

mule.detection.rf.full <- ranger(
  Detection ~ .,
  data = detection.full,
  num.trees = 500,
  min.node.size = 20,
  probability = TRUE,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 9. Prepare full positive-count data
# ------------------------------------------------------------

positive.full <- mule[
  mule$Daily_Count > 0,
]


positive.full.model <- positive.full


positive.full.model$Cell_ID <- NULL

positive.full.model$Detection <- NULL



# ------------------------------------------------------------
# 10. Fit full positive-count Random Forest
# ------------------------------------------------------------

mule.positive.rf.full <- ranger(
  Daily_Count ~ .,
  data = positive.full.model,
  num.trees = 500,
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ============================================================
# PREPARE CENTRAL-RANGE INTERPRETATION DATA
# ============================================================



# ------------------------------------------------------------
# 11. Detection interpretation data
# ------------------------------------------------------------
#
# This contains only observations inside the central PM2.5
# range.
#
# Daily_Count and Cell_ID are removed to match the variables
# expected by the probability forest.

detection.central <- mule.central


detection.central$Cell_ID <- NULL

detection.central$Daily_Count <- NULL



# ------------------------------------------------------------
# 12. Positive-count interpretation data
# ------------------------------------------------------------

positive.central <- mule.central[
  mule.central$Daily_Count > 0,
]


positive.central.model <- positive.central


positive.central.model$Cell_ID <- NULL

positive.central.model$Detection <- NULL



cat(
  "\nCentral-range positive detection days:",
  nrow(positive.central),
  "\n"
)



# ============================================================
# CREATE IMPROVED QUANTILE BOUNDARIES
# ============================================================
#
# Instead of simple 10% quantiles, we use more detail near the
# high-smoke end.
#
#
# Most of the distribution:
#
#     approximately 5-percentage-point intervals
#
#
# Upper tail:
#
#     additional boundaries at:
#
#     92.5%
#     95%
#     96.5%
#     97.5%
#     98.5%
#     99%
#
#
# This means the upper tail is no longer represented by one
# enormous interval.
# ============================================================



# ------------------------------------------------------------
# 13. Define quantile probabilities
# ------------------------------------------------------------

ale.probabilities <- c(
  0.01,
  0.05,
  0.10,
  0.15,
  0.20,
  0.25,
  0.30,
  0.35,
  0.40,
  0.45,
  0.50,
  0.55,
  0.60,
  0.65,
  0.70,
  0.75,
  0.80,
  0.85,
  0.90,
  0.925,
  0.95,
  0.965,
  0.975,
  0.985,
  0.99
)



# ============================================================
# FUNCTION TO CREATE ALE BREAKS
# ============================================================



# ------------------------------------------------------------
# 14. Create quantile-based ALE boundaries
# ------------------------------------------------------------

create_ale_breaks <- function(
    x,
    probabilities
) {
  
  
  breaks <- as.numeric(
    quantile(
      x,
      probs = probabilities,
      na.rm = TRUE
    )
  )
  
  
  # Because many observations have nearly identical low-smoke
  # values, some quantiles may produce duplicate boundaries.
  #
  # cut() cannot use duplicate boundaries, so we remove them.
  
  breaks <- unique(
    breaks
  )
  
  
  if (length(breaks) < 3) {
    
    stop(
      "Not enough unique PM2.5 boundaries for ALE."
    )
  }
  
  
  return(
    breaks
  )
}



# ------------------------------------------------------------
# 15. Create detection-model PM2.5 boundaries
# ------------------------------------------------------------

detection.breaks <- create_ale_breaks(
  x = detection.central$scale.smoke,
  probabilities = ale.probabilities
)



# ------------------------------------------------------------
# 16. Create positive-count PM2.5 boundaries
# ------------------------------------------------------------
#
# We calculate these independently because PM2.5 among positive
# detection days does not have exactly the same distribution as
# PM2.5 across all camera-days.

positive.breaks <- create_ale_breaks(
  x = positive.central.model$scale.smoke,
  probabilities = ale.probabilities
)



# ============================================================
# DETECTION ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 17. Classification ALE function
# ------------------------------------------------------------

calculate_detection_ale <- function(
    model,
    data,
    breaks,
    variable = "scale.smoke"
) {
  
  
  x <- data[[variable]]
  
  
  
  # Assign every observation to one interval.
  
  bin.id <- cut(
    x,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  
  number.intervals <-
    length(breaks) - 1
  
  
  
  # Storage.
  
  local.effects <- numeric(
    number.intervals
  )
  
  
  observations.per.bin <- integer(
    number.intervals
  )
  
  
  
  # ----------------------------------------------------------
  # Calculate local effects
  # ----------------------------------------------------------
  
  for (b in 1:number.intervals) {
    
    
    rows.in.bin <- which(
      bin.id == b
    )
    
    
    observations.per.bin[b] <-
      length(rows.in.bin)
    
    
    if (length(rows.in.bin) == 0) {
      
      local.effects[b] <- NA
      
      next
    }
    
    
    
    # Only use observations that ACTUALLY occur inside this
    # PM2.5 interval.
    
    lower.data <- data[
      rows.in.bin,
      ,
      drop = FALSE
    ]
    
    
    upper.data <- data[
      rows.in.bin,
      ,
      drop = FALSE
    ]
    
    
    
    # Move those observations only to the two boundaries of
    # their LOCAL smoke region.
    
    lower.data[[variable]] <-
      breaks[b]
    
    
    upper.data[[variable]] <-
      breaks[b + 1]
    
    
    
    # Predict probability of detection.
    
    lower.prediction <- predict(
      model,
      data = lower.data
    )$predictions[, "Yes"]
    
    
    upper.prediction <- predict(
      model,
      data = upper.data
    )$predictions[, "Yes"]
    
    
    
    # Local effect:
    #
    # upper probability - lower probability
    
    local.effects[b] <- mean(
      upper.prediction -
        lower.prediction
    )
  }
  
  
  
  # ----------------------------------------------------------
  # Remove empty intervals if any exist
  # ----------------------------------------------------------
  
  valid <- !is.na(
    local.effects
  )
  
  
  lower.valid <- breaks[
    1:number.intervals
  ][valid]
  
  
  upper.valid <- breaks[
    2:(number.intervals + 1)
  ][valid]
  
  
  effects.valid <- local.effects[
    valid
  ]
  
  
  observations.valid <-
    observations.per.bin[
      valid
    ]
  
  
  
  # ----------------------------------------------------------
  # Accumulate local effects
  # ----------------------------------------------------------
  
  accumulated <- cumsum(
    effects.valid
  )
  
  
  
  # ----------------------------------------------------------
  # Center ALE around zero
  # ----------------------------------------------------------
  
  center.value <- weighted.mean(
    accumulated,
    w = observations.valid
  )
  
  
  centered.ale <-
    accumulated -
    center.value
  
  
  
  # ----------------------------------------------------------
  # Return detailed results
  # ----------------------------------------------------------
  
  results <- data.frame(
    
    Bin = seq_along(
      effects.valid
    ),
    
    Lower_PM25 =
      lower.valid,
    
    Upper_PM25 =
      upper.valid,
    
    Bin_Width =
      upper.valid -
      lower.valid,
    
    Midpoint_PM25 = (
      lower.valid +
        upper.valid
    ) / 2,
    
    Observations =
      observations.valid,
    
    Local_Effect =
      effects.valid,
    
    ALE =
      centered.ale
  )
  
  
  return(
    results
  )
}



# ============================================================
# POSITIVE-COUNT ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 18. Regression ALE function
# ------------------------------------------------------------

calculate_regression_ale <- function(
    model,
    data,
    breaks,
    variable = "scale.smoke"
) {
  
  
  x <- data[[variable]]
  
  
  bin.id <- cut(
    x,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  
  number.intervals <-
    length(breaks) - 1
  
  
  local.effects <- numeric(
    number.intervals
  )
  
  
  observations.per.bin <- integer(
    number.intervals
  )
  
  
  
  for (b in 1:number.intervals) {
    
    
    rows.in.bin <- which(
      bin.id == b
    )
    
    
    observations.per.bin[b] <-
      length(rows.in.bin)
    
    
    if (length(rows.in.bin) == 0) {
      
      local.effects[b] <- NA
      
      next
    }
    
    
    lower.data <- data[
      rows.in.bin,
      ,
      drop = FALSE
    ]
    
    
    upper.data <- data[
      rows.in.bin,
      ,
      drop = FALSE
    ]
    
    
    lower.data[[variable]] <-
      breaks[b]
    
    
    upper.data[[variable]] <-
      breaks[b + 1]
    
    
    lower.prediction <- predict(
      model,
      data = lower.data
    )$predictions
    
    
    upper.prediction <- predict(
      model,
      data = upper.data
    )$predictions
    
    
    local.effects[b] <- mean(
      upper.prediction -
        lower.prediction
    )
  }
  
  
  
  valid <- !is.na(
    local.effects
  )
  
  
  lower.valid <- breaks[
    1:number.intervals
  ][valid]
  
  
  upper.valid <- breaks[
    2:(number.intervals + 1)
  ][valid]
  
  
  effects.valid <- local.effects[
    valid
  ]
  
  
  observations.valid <-
    observations.per.bin[
      valid
    ]
  
  
  
  accumulated <- cumsum(
    effects.valid
  )
  
  
  center.value <- weighted.mean(
    accumulated,
    w = observations.valid
  )
  
  
  centered.ale <-
    accumulated -
    center.value
  
  
  
  results <- data.frame(
    
    Bin = seq_along(
      effects.valid
    ),
    
    Lower_PM25 =
      lower.valid,
    
    Upper_PM25 =
      upper.valid,
    
    Bin_Width =
      upper.valid -
      lower.valid,
    
    Midpoint_PM25 = (
      lower.valid +
        upper.valid
    ) / 2,
    
    Observations =
      observations.valid,
    
    Local_Effect =
      effects.valid,
    
    ALE =
      centered.ale
  )
  
  
  return(
    results
  )
}



# ============================================================
# CALCULATE REFINED DETECTION ALE
# ============================================================



# ------------------------------------------------------------
# 19. Calculate detection probability ALE
# ------------------------------------------------------------

detection.ale <- calculate_detection_ale(
  model = mule.detection.rf.full,
  data = detection.central,
  breaks = detection.breaks
)



cat(
  "\n====================================================\n"
)

cat(
  "REFINED DETECTION ALE\n"
)

cat(
  "====================================================\n"
)


print(
  detection.ale
)



# ------------------------------------------------------------
# 20. Detection ALE overall change
# ------------------------------------------------------------

detection.ale.change <-
  detection.ale$ALE[
    nrow(detection.ale)
  ] -
  detection.ale$ALE[1]


cat(
  "\nDetection ALE 1st-to-99th percentile change:",
  round(
    detection.ale.change,
    6
  ),
  "\n"
)



# ============================================================
# CALCULATE REFINED POSITIVE-COUNT ALE
# ============================================================



# ------------------------------------------------------------
# 21. Calculate positive-count ALE
# ------------------------------------------------------------

positive.ale <- calculate_regression_ale(
  model = mule.positive.rf.full,
  data = positive.central.model,
  breaks = positive.breaks
)



cat(
  "\n====================================================\n"
)

cat(
  "REFINED POSITIVE-COUNT ALE\n"
)

cat(
  "====================================================\n"
)


print(
  positive.ale
)



# ------------------------------------------------------------
# 22. Positive-count overall change
# ------------------------------------------------------------

positive.ale.change <-
  positive.ale$ALE[
    nrow(positive.ale)
  ] -
  positive.ale$ALE[1]


cat(
  "\nPositive-count ALE 1st-to-99th percentile change:",
  round(
    positive.ale.change,
    6
  ),
  "\n"
)



# ============================================================
# CHECK WHETHER ONE BIN DOMINATES THE RESULT
# ============================================================
#
# This is a new diagnostic.
#
# If one local effect represents most of the entire accumulated
# PM2.5 response, we should be cautious about interpreting the
# overall direction.
# ============================================================



# ------------------------------------------------------------
# 23. Detection-model largest local effect
# ------------------------------------------------------------

largest.detection.effect <- max(
  abs(
    detection.ale$Local_Effect
  )
)


total.detection.local.change <- sum(
  abs(
    detection.ale$Local_Effect
  )
)


detection.dominance.percent <- (
  largest.detection.effect /
    total.detection.local.change
) * 100


cat(
  "\nLargest single detection ALE local effect:",
  round(
    largest.detection.effect,
    6
  ),
  "\n"
)


cat(
  "Percent of total absolute detection ALE movement from",
  "largest interval:",
  round(
    detection.dominance.percent,
    2
  ),
  "%\n"
)



# ------------------------------------------------------------
# 24. Positive-count largest local effect
# ------------------------------------------------------------

largest.positive.effect <- max(
  abs(
    positive.ale$Local_Effect
  )
)


total.positive.local.change <- sum(
  abs(
    positive.ale$Local_Effect
  )
)


positive.dominance.percent <- (
  largest.positive.effect /
    total.positive.local.change
) * 100


cat(
  "\nLargest single positive-count ALE local effect:",
  round(
    largest.positive.effect,
    6
  ),
  "\n"
)


cat(
  "Percent of total absolute positive-count ALE movement from",
  "largest interval:",
  round(
    positive.dominance.percent,
    2
  ),
  "%\n"
)



# ============================================================
# OBSERVED DETECTION DATA
# ============================================================



# ------------------------------------------------------------
# 25. Assign observations to the refined detection bins
# ------------------------------------------------------------

mule.central$detection.smoke.bin <- cut(
  mule.central$scale.smoke,
  breaks = detection.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 26. Summarize observed detection rate
# ------------------------------------------------------------

observed.detection <- data.frame()


for (b in sort(
  unique(
    mule.central$detection.smoke.bin
  )
)) {
  
  
  rows.in.bin <- which(
    mule.central$detection.smoke.bin == b
  )
  
  
  if (length(rows.in.bin) == 0) {
    
    next
  }
  
  
  bin.summary <- data.frame(
    
    Bin = b,
    
    Mean_PM25 = mean(
      mule.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Min_PM25 = min(
      mule.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Max_PM25 = max(
      mule.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Observations =
      length(rows.in.bin),
    
    Detection_Days = sum(
      mule.central$Detection[
        rows.in.bin
      ] == "Yes"
    ),
    
    Detection_Rate = mean(
      mule.central$Detection[
        rows.in.bin
      ] == "Yes"
    )
  )
  
  
  observed.detection <- rbind(
    observed.detection,
    bin.summary
  )
}



cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED DETECTION RATE - REFINED SMOKE BINS\n"
)

cat(
  "====================================================\n"
)


print(
  observed.detection
)



# ============================================================
# OBSERVED POSITIVE-COUNT DATA
# ============================================================



# ------------------------------------------------------------
# 27. Assign positive days to refined positive-count bins
# ------------------------------------------------------------

positive.central$positive.smoke.bin <- cut(
  positive.central$scale.smoke,
  breaks = positive.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 28. Summarize observed counts
# ------------------------------------------------------------

observed.positive <- data.frame()


for (b in sort(
  unique(
    positive.central$positive.smoke.bin
  )
)) {
  
  
  rows.in.bin <- which(
    positive.central$positive.smoke.bin == b
  )
  
  
  if (length(rows.in.bin) == 0) {
    
    next
  }
  
  
  bin.summary <- data.frame(
    
    Bin = b,
    
    Mean_PM25 = mean(
      positive.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Min_PM25 = min(
      positive.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Max_PM25 = max(
      positive.central$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Observations =
      length(rows.in.bin),
    
    Mean_Positive_Count = mean(
      positive.central$Daily_Count[
        rows.in.bin
      ]
    ),
    
    Median_Positive_Count = median(
      positive.central$Daily_Count[
        rows.in.bin
      ]
    )
  )
  
  
  observed.positive <- rbind(
    observed.positive,
    bin.summary
  )
}



cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED POSITIVE COUNTS - REFINED SMOKE BINS\n"
)

cat(
  "====================================================\n"
)


print(
  observed.positive
)



# ============================================================
# CREATE RESULTS FOLDER
# ============================================================

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ============================================================
# SAVE RESULTS TABLES
# ============================================================



# ------------------------------------------------------------
# 29. Save refined detection ALE
# ------------------------------------------------------------

write.csv(
  detection.ale,
  "results/mule_deer_v2_refined_detection_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 30. Save refined positive-count ALE
# ------------------------------------------------------------

write.csv(
  positive.ale,
  "results/mule_deer_v2_refined_positive_count_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 31. Save observed detection bins
# ------------------------------------------------------------

write.csv(
  observed.detection,
  "results/mule_deer_v2_refined_observed_detection.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 32. Save observed positive-count bins
# ------------------------------------------------------------

write.csv(
  observed.positive,
  "results/mule_deer_v2_refined_observed_positive_counts.csv",
  row.names = FALSE
)



# ============================================================
# PLOT 1:
# REFINED DETECTION ALE
# ============================================================
#
# geom_step() is used instead of connecting distant ALE points
# with long diagonal lines.
#
# ALE is based on intervals, so a step-style visualization is
# more honest about the way the calculation works.
# ============================================================

detection.ale.plot <- ggplot(
  detection.ale,
  aes(
    x = Upper_PM25,
    y = ALE
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_step(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Refined Mule Deer Detection Response to Wildfire PM2.5",
    subtitle = "ALE restricted to the central 98% of observed smoke values",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "ALE Effect on Detection Probability"
  ) +
  
  theme_minimal()


print(
  detection.ale.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_refined_detection_ale.png",
  plot =
    detection.ale.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 2:
# OBSERVED DETECTION RATE
# ============================================================

observed.detection.plot <- ggplot(
  observed.detection,
  aes(
    x = Mean_PM25,
    y = Detection_Rate
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Observed Mule Deer Detection Rate by PM2.5",
    subtitle = "Restricted to 1st-99th percentile of smoke",
    x = "Mean Wildfire-attributable PM2.5 (scaled)",
    y = "Observed Detection Rate"
  ) +
  
  theme_minimal()


print(
  observed.detection.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_refined_observed_detection.png",
  plot =
    observed.detection.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 3:
# REFINED POSITIVE-COUNT ALE
# ============================================================

positive.ale.plot <- ggplot(
  positive.ale,
  aes(
    x = Upper_PM25,
    y = ALE
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_step(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Refined Mule Deer Positive-Count Response to PM2.5",
    subtitle = "ALE among detection days, central 98% smoke range",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "ALE Effect on Positive Daily Count"
  ) +
  
  theme_minimal()


print(
  positive.ale.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_refined_positive_count_ale.png",
  plot =
    positive.ale.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 4:
# OBSERVED POSITIVE COUNTS
# ============================================================

observed.positive.plot <- ggplot(
  observed.positive,
  aes(
    x = Mean_PM25,
    y = Mean_Positive_Count
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Observed Mule Deer Counts on Detection Days",
    subtitle = "Restricted to 1st-99th percentile of smoke",
    x = "Mean Wildfire-attributable PM2.5 (scaled)",
    y = "Mean Daily Count When Detected"
  ) +
  
  theme_minimal()


print(
  observed.positive.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_refined_observed_positive_counts.png",
  plot =
    observed.positive.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL DIAGNOSTIC SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "REFINED MULE DEER ALE SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Smoke interpretation range:",
  round(smoke.lower, 6),
  "to",
  round(smoke.upper, 6),
  "\n"
)


cat(
  "\nDetection ALE intervals:",
  nrow(detection.ale),
  "\n"
)


cat(
  "Detection ALE low-to-high change:",
  round(
    detection.ale.change,
    6
  ),
  "\n"
)


cat(
  "Largest detection-bin contribution:",
  round(
    detection.dominance.percent,
    2
  ),
  "% of total absolute ALE movement\n"
)


cat(
  "\nPositive-count ALE intervals:",
  nrow(positive.ale),
  "\n"
)


cat(
  "Positive-count ALE low-to-high change:",
  round(
    positive.ale.change,
    6
  ),
  "\n"
)


cat(
  "Largest positive-count-bin contribution:",
  round(
    positive.dominance.percent,
    2
  ),
  "% of total absolute ALE movement\n"
)


cat(
  "\nNEXT DECISION:\n"
)


cat(
  "If the ALE curves are no longer dominated by one huge\n"
)


cat(
  "interval, we will freeze these boundaries and perform\n"
)


cat(
  "the PM2.5 shuffle test using the exact same ALE procedure.\n"
)


cat(
  "====================================================\n"
)