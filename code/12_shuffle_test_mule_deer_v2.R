# ============================================================
# 12_shuffle_test_mule_deer_v2.R
#
# VERSION 2 - PM2.5 SHUFFLE / NULL TEST
#
# PURPOSE:
#
# Test whether the refined ALE effects from our Version 2
# mule-deer models are stronger than effects produced after
# wildfire PM2.5 has been randomized.
#
#
# WHY THIS TEST MATTERS:
#
# Our original Random Forest + PDP method failed this type of
# test.
#
# For bobcat, shuffled PM2.5 still produced 30 out of 30
# positive responses.
#
# That told us the old interpretation method was creating a
# systematic positive pattern even when the true smoke
# relationship had been destroyed.
#
#
# VERSION 2 NOW USES:
#
#   1. A detection-probability Random Forest
#
#   2. A positive-count Random Forest
#
#   3. Refined ALE instead of PDP
#
#   4. PM2.5 interpretation restricted to the
#      1st-99th percentile
#
#   5. More detailed ALE intervals in the upper smoke tail
#
#
# THIS SCRIPT ASKS:
#
# If we randomly scramble PM2.5, does the observed ALE effect
# stand out from the effects produced under the null condition?
#
#
# IMPORTANT:
#
# The shuffled ALE effects do not necessarily need to be
# centered exactly at zero.
#
# Instead, the key comparison is whether the observed effect is
# unusually large relative to the shuffled/null distribution.
#
#
# OBSERVED EFFECTS FROM SCRIPT 11b:
#
# Detection probability ALE change:
#
#     +0.026853
#
#
# Positive-count ALE change:
#
#     +0.432493
#
#
# This is a diagnostic/randomization test.
#
# It does NOT establish causality.
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
# 3. Restore categorical variables as factors
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
#
# Detection = Yes if at least one mule deer detection occurred
# on the camera-day.
#
# Detection = No if Daily_Count = 0.

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
# OBSERVED VERSION 2 ALE EFFECTS
# ============================================================



# ------------------------------------------------------------
# 5. Store observed refined ALE effects
# ------------------------------------------------------------
#
# These values came from:
#
#     11b_refined_ale_mule_deer.R
#
# Each shuffled model will be compared with these real-data
# effects.

observed.detection.effect <- 0.026853

observed.positive.effect <- 0.432493



# ============================================================
# DEFINE ORIGINAL CENTRAL SMOKE RANGE
# ============================================================



# ------------------------------------------------------------
# 6. Calculate original 1st and 99th percentiles
# ------------------------------------------------------------
#
# ALE interpretation is limited to the central 98% of observed
# wildfire PM2.5 values.
#
# This reduces the influence of a small number of extreme smoke
# observations.

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
  "\nPM2.5 interpretation range:\n"
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
# 7. Define refined ALE quantile probabilities
# ------------------------------------------------------------
#
# These probabilities are the same as Script 11b.
#
# Extra boundaries are included in the upper smoke tail because
# wildfire-attributable PM2.5 is strongly right-skewed.

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
# CREATE FIXED ALE BOUNDARIES
# ============================================================
#
# ALE boundaries are created ONCE using the original PM2.5
# distribution.
#
# These same boundaries are reused for every shuffled model.
#
# This makes the shuffled runs directly comparable with one
# another and with the observed ALE effect.
# ============================================================



# ------------------------------------------------------------
# 8. Function for creating unique ALE boundaries
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
  
  breaks <- unique(
    breaks
  )
  
  if (length(breaks) < 3) {
    
    stop(
      "Not enough unique PM2.5 boundaries."
    )
  }
  
  return(
    breaks
  )
}



# ------------------------------------------------------------
# 9. Detection-model ALE boundaries
# ------------------------------------------------------------

original.central <- mule[
  mule$scale.smoke >= smoke.lower &
    mule$scale.smoke <= smoke.upper,
]

detection.breaks <- create_ale_breaks(
  x = original.central$scale.smoke,
  probabilities = ale.probabilities
)



# ------------------------------------------------------------
# 10. Positive-count ALE boundaries
# ------------------------------------------------------------

original.positive.central <- original.central[
  original.central$Daily_Count > 0,
]

positive.breaks <- create_ale_breaks(
  x = original.positive.central$scale.smoke,
  probabilities = ale.probabilities
)



# ============================================================
# DETECTION ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 11. Classification ALE
# ------------------------------------------------------------
#
# This function calculates ALE for the probability that
# Detection = Yes.
#
# For observations inside each PM2.5 interval:
#
#   1. Predict detection probability at the lower boundary.
#
#   2. Predict detection probability at the upper boundary.
#
#   3. Calculate the difference.
#
#   4. Average the local differences.
#
#   5. Accumulate the local effects across the smoke range.

calculate_detection_ale <- function(
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
    )$predictions[, "Yes"]
    
    upper.prediction <- predict(
      model,
      data = upper.data
    )$predictions[, "Yes"]
    
    
    local.effects[b] <- mean(
      upper.prediction -
        lower.prediction
    )
  }
  
  
  valid <- !is.na(
    local.effects
  )
  
  
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
    
    Lower_PM25 =
      breaks[
        1:number.intervals
      ][valid],
    
    Upper_PM25 =
      breaks[
        2:(number.intervals + 1)
      ][valid],
    
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
# 12. Regression ALE
# ------------------------------------------------------------
#
# This version calculates ALE for Daily_Count among camera-days
# where mule deer were detected.

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
    
    Lower_PM25 =
      breaks[
        1:number.intervals
      ][valid],
    
    Upper_PM25 =
      breaks[
        2:(number.intervals + 1)
      ][valid],
    
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
# SET UP SHUFFLE TEST
# ============================================================



# ------------------------------------------------------------
# 13. Number of randomizations
# ------------------------------------------------------------
#
# 99 permutations are used.
#
# With the corrected permutation p-value formula:
#
#     p = (number at least as extreme + 1)
#         /
#         (number of permutations + 1)
#
# 99 runs allow p-values in increments of 0.01.

number.runs <- 99



# ------------------------------------------------------------
# 14. Storage
# ------------------------------------------------------------

detection.shuffle.effects <- numeric(
  number.runs
)

positive.shuffle.effects <- numeric(
  number.runs
)



# ============================================================
# RUN SHUFFLE TEST
# ============================================================

for (r in 1:number.runs) {
  
  
  # ----------------------------------------------------------
  # 15A. Set reproducible shuffle seed
  # ----------------------------------------------------------
  
  set.seed(
    1000 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 15B. Make a fresh copy of the original data
  # ----------------------------------------------------------
  
  shuffled <- mule
  
  
  
  # ----------------------------------------------------------
  # 15C. Randomize PM2.5
  # ----------------------------------------------------------
  #
  # sample() rearranges the existing PM2.5 observations.
  #
  # This keeps the exact same PM2.5 values and distribution,
  # while destroying the original relationship between smoke
  # and the other variables.
  #
  # This represents the null condition.
  
  shuffled$scale.smoke <- sample(
    shuffled$scale.smoke,
    replace = FALSE
  )
  
  
  
  # ==========================================================
  # DETECTION MODEL FOR THIS SHUFFLE
  # ==========================================================
  
  
  
  # ----------------------------------------------------------
  # 15D. Prepare detection-model data
  # ----------------------------------------------------------
  #
  # Cell_ID is removed because it is an identifier.
  #
  # Daily_Count is removed because Detection is derived from
  # Daily_Count and keeping it would create leakage.
  
  shuffled.detection.full <- shuffled
  
  shuffled.detection.full$Cell_ID <- NULL
  
  shuffled.detection.full$Daily_Count <- NULL
  
  
  
  # ----------------------------------------------------------
  # 15E. Fit shuffled detection probability Random Forest
  # ----------------------------------------------------------
  
  shuffled.detection.rf <- ranger(
    Detection ~ .,
    data = shuffled.detection.full,
    num.trees = 500,
    min.node.size = 20,
    probability = TRUE,
    importance = "none",
    respect.unordered.factors = "order",
    seed = 2000 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 15F. Restrict ALE interpretation to central smoke range
  # ----------------------------------------------------------
  
  shuffled.detection.central <-
    shuffled.detection.full[
      shuffled.detection.full$scale.smoke >=
        smoke.lower &
        shuffled.detection.full$scale.smoke <=
        smoke.upper,
    ]
  
  
  
  # ----------------------------------------------------------
  # 15G. Calculate shuffled detection ALE
  # ----------------------------------------------------------
  
  shuffled.detection.ale <-
    calculate_detection_ale(
      model = shuffled.detection.rf,
      data = shuffled.detection.central,
      breaks = detection.breaks
    )
  
  
  
  # ----------------------------------------------------------
  # 15H. Store low-to-high detection ALE change
  # ----------------------------------------------------------
  
  detection.effect <-
    shuffled.detection.ale$ALE[
      nrow(
        shuffled.detection.ale
      )
    ] -
    shuffled.detection.ale$ALE[1]
  
  
  detection.shuffle.effects[r] <-
    detection.effect
  
  
  
  # ==========================================================
  # POSITIVE-COUNT MODEL FOR THIS SHUFFLE
  # ==========================================================
  
  
  
  # ----------------------------------------------------------
  # 15I. Keep only camera-days with detections
  # ----------------------------------------------------------
  
  shuffled.positive <- shuffled[
    shuffled$Daily_Count > 0,
  ]
  
  
  
  # ----------------------------------------------------------
  # 15J. Prepare positive-count data
  # ----------------------------------------------------------
  
  shuffled.positive.full <-
    shuffled.positive
  
  shuffled.positive.full$Cell_ID <- NULL
  
  shuffled.positive.full$Detection <- NULL
  
  
  
  # ----------------------------------------------------------
  # 15K. Fit shuffled positive-count Random Forest
  # ----------------------------------------------------------
  
  shuffled.positive.rf <- ranger(
    Daily_Count ~ .,
    data = shuffled.positive.full,
    num.trees = 500,
    min.node.size = 20,
    importance = "none",
    respect.unordered.factors = "order",
    seed = 3000 + r
  )
  
  
  
  # ----------------------------------------------------------
  # 15L. Restrict positive-count data to central smoke range
  # ----------------------------------------------------------
  
  shuffled.positive.central <-
    shuffled.positive.full[
      shuffled.positive.full$scale.smoke >=
        smoke.lower &
        shuffled.positive.full$scale.smoke <=
        smoke.upper,
    ]
  
  
  
  # ----------------------------------------------------------
  # 15M. Calculate shuffled positive-count ALE
  # ----------------------------------------------------------
  
  shuffled.positive.ale <-
    calculate_regression_ale(
      model = shuffled.positive.rf,
      data = shuffled.positive.central,
      breaks = positive.breaks
    )
  
  
  
  # ----------------------------------------------------------
  # 15N. Store low-to-high positive-count ALE change
  # ----------------------------------------------------------
  
  positive.effect <-
    shuffled.positive.ale$ALE[
      nrow(
        shuffled.positive.ale
      )
    ] -
    shuffled.positive.ale$ALE[1]
  
  
  positive.shuffle.effects[r] <-
    positive.effect
  
  
  
  # ----------------------------------------------------------
  # 15O. Print progress
  # ----------------------------------------------------------
  
  cat(
    "Completed run",
    r,
    "of",
    number.runs,
    "| Detection ALE:",
    round(
      detection.effect,
      6
    ),
    "| Positive-count ALE:",
    round(
      positive.effect,
      6
    ),
    "\n"
  )
}



# ============================================================
# SUMMARIZE DETECTION SHUFFLE RESULTS
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "DETECTION ALE SHUFFLE RESULTS\n"
)

cat(
  "====================================================\n"
)


print(
  detection.shuffle.effects
)


cat(
  "\nDetection shuffle summary:\n"
)

print(
  summary(
    detection.shuffle.effects
  )
)



# ------------------------------------------------------------
# 16. Positive / negative / flat detection counts
# ------------------------------------------------------------

tolerance <- 0.000001


detection.positive.runs <- sum(
  detection.shuffle.effects > tolerance
)

detection.negative.runs <- sum(
  detection.shuffle.effects < -tolerance
)

detection.flat.runs <- sum(
  abs(
    detection.shuffle.effects
  ) <= tolerance
)


cat(
  "\nPositive detection shuffle effects:",
  detection.positive.runs,
  "\n"
)

cat(
  "Negative detection shuffle effects:",
  detection.negative.runs,
  "\n"
)

cat(
  "Flat detection shuffle effects:",
  detection.flat.runs,
  "\n"
)



# ------------------------------------------------------------
# 17. Mean detection null effect
# ------------------------------------------------------------

mean.detection.shuffle <- mean(
  detection.shuffle.effects
)


cat(
  "Mean shuffled detection ALE effect:",
  round(
    mean.detection.shuffle,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 18. Compare observed detection effect with null distribution
# ------------------------------------------------------------
#
# Because the observed detection effect is positive, we count
# how many shuffled runs produced an ALE effect that was at least
# as large as the observed value.
#
# Corrected permutation p-value:
#
#     (count + 1) / (number.runs + 1)

detection.as.large <- sum(
  detection.shuffle.effects >=
    observed.detection.effect
)


detection.empirical.proportion <- (
  detection.as.large + 1
) / (
  number.runs + 1
)


cat(
  "\nObserved detection ALE effect:",
  observed.detection.effect,
  "\n"
)

cat(
  "Shuffled detection effects >= observed:",
  detection.as.large,
  "of",
  number.runs,
  "\n"
)

cat(
  "Corrected permutation p-value:",
  round(
    detection.empirical.proportion,
    3
  ),
  "\n"
)



# ============================================================
# SUMMARIZE POSITIVE-COUNT SHUFFLE RESULTS
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "POSITIVE-COUNT ALE SHUFFLE RESULTS\n"
)

cat(
  "====================================================\n"
)


print(
  positive.shuffle.effects
)


cat(
  "\nPositive-count shuffle summary:\n"
)

print(
  summary(
    positive.shuffle.effects
  )
)



# ------------------------------------------------------------
# 19. Positive / negative / flat positive-count effects
# ------------------------------------------------------------

positive.positive.runs <- sum(
  positive.shuffle.effects > tolerance
)

positive.negative.runs <- sum(
  positive.shuffle.effects < -tolerance
)

positive.flat.runs <- sum(
  abs(
    positive.shuffle.effects
  ) <= tolerance
)


cat(
  "\nPositive positive-count shuffle effects:",
  positive.positive.runs,
  "\n"
)

cat(
  "Negative positive-count shuffle effects:",
  positive.negative.runs,
  "\n"
)

cat(
  "Flat positive-count shuffle effects:",
  positive.flat.runs,
  "\n"
)



# ------------------------------------------------------------
# 20. Mean positive-count null effect
# ------------------------------------------------------------

mean.positive.shuffle <- mean(
  positive.shuffle.effects
)


cat(
  "Mean shuffled positive-count ALE effect:",
  round(
    mean.positive.shuffle,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 21. Compare observed positive-count effect with null
# ------------------------------------------------------------

positive.as.large <- sum(
  positive.shuffle.effects >=
    observed.positive.effect
)


positive.empirical.proportion <- (
  positive.as.large + 1
) / (
  number.runs + 1
)


cat(
  "\nObserved positive-count ALE effect:",
  observed.positive.effect,
  "\n"
)

cat(
  "Shuffled positive-count effects >= observed:",
  positive.as.large,
  "of",
  number.runs,
  "\n"
)

cat(
  "Corrected permutation p-value:",
  round(
    positive.empirical.proportion,
    3
  ),
  "\n"
)



# ============================================================
# CREATE AND SAVE RESULTS TABLE
# ============================================================



# ------------------------------------------------------------
# 22. Create shuffle-results data frame
# ------------------------------------------------------------

shuffle.results <- data.frame(
  
  Run = 1:number.runs,
  
  Detection_ALE_Change =
    detection.shuffle.effects,
  
  Positive_Count_ALE_Change =
    positive.shuffle.effects
)



# ------------------------------------------------------------
# 23. Create results directory if needed
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 24. Save shuffle results
# ------------------------------------------------------------

write.csv(
  shuffle.results,
  "results/mule_deer_v2_ale_shuffle_results.csv",
  row.names = FALSE
)



# ============================================================
# PLOT 1:
# DETECTION NULL DISTRIBUTION
# ============================================================
#
# Dashed vertical line:
#
#     zero
#
# Solid vertical line:
#
#     observed mule deer detection ALE effect
#
# The observed effect should be interpreted relative to the
# shuffled distribution rather than simply by whether it is
# positive or negative.
# ============================================================

detection.shuffle.plot <- ggplot(
  shuffle.results,
  aes(
    x = Detection_ALE_Change
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
    xintercept =
      observed.detection.effect,
    linewidth = 1
  ) +
  
  labs(
    title =
      "Mule Deer Detection ALE Shuffle Test",
    subtitle =
      "99 Random Forest Fits After Randomizing PM2.5",
    x =
      "Detection ALE Low-to-High Change",
    y =
      "Number of Shuffled Models"
  ) +
  
  theme_minimal()


print(
  detection.shuffle.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_detection_ale_shuffle.png",
  plot =
    detection.shuffle.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 2:
# POSITIVE-COUNT NULL DISTRIBUTION
# ============================================================
#
# Dashed vertical line:
#
#     zero
#
# Solid vertical line:
#
#     observed mule deer positive-count ALE effect
# ============================================================

positive.shuffle.plot <- ggplot(
  shuffle.results,
  aes(
    x = Positive_Count_ALE_Change
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
    xintercept =
      observed.positive.effect,
    linewidth = 1
  ) +
  
  labs(
    title =
      "Mule Deer Positive-Count ALE Shuffle Test",
    subtitle =
      "99 Random Forest Fits After Randomizing PM2.5",
    x =
      "Positive-Count ALE Low-to-High Change",
    y =
      "Number of Shuffled Models"
  ) +
  
  theme_minimal()


print(
  positive.shuffle.plot
)


ggsave(
  filename =
    "results/mule_deer_v2_positive_count_ale_shuffle.png",
  plot =
    positive.shuffle.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL VERSION 2 NULL-TEST SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER VERSION 2 ALE NULL TEST\n"
)

cat(
  "====================================================\n"
)



cat(
  "\n--- DETECTION MODEL ---\n"
)

cat(
  "Observed ALE effect:",
  observed.detection.effect,
  "\n"
)

cat(
  "Mean shuffled effect:",
  round(
    mean.detection.shuffle,
    6
  ),
  "\n"
)

cat(
  "Positive shuffled effects:",
  detection.positive.runs,
  "\n"
)

cat(
  "Negative shuffled effects:",
  detection.negative.runs,
  "\n"
)

cat(
  "Shuffled effects >= observed:",
  detection.as.large,
  "\n"
)

cat(
  "Corrected permutation p-value:",
  round(
    detection.empirical.proportion,
    3
  ),
  "\n"
)



cat(
  "\n--- POSITIVE-COUNT MODEL ---\n"
)

cat(
  "Observed ALE effect:",
  observed.positive.effect,
  "\n"
)

cat(
  "Mean shuffled effect:",
  round(
    mean.positive.shuffle,
    6
  ),
  "\n"
)

cat(
  "Positive shuffled effects:",
  positive.positive.runs,
  "\n"
)

cat(
  "Negative shuffled effects:",
  positive.negative.runs,
  "\n"
)

cat(
  "Shuffled effects >= observed:",
  positive.as.large,
  "\n"
)

cat(
  "Corrected permutation p-value:",
  round(
    positive.empirical.proportion,
    3
  ),
  "\n"
)

cat(
  "====================================================\n"
)