# ============================================================
# 14_refined_ale_moose.R
#
# VERSION 2 REFINED ALE ANALYSIS - MOOSE
#
# PURPOSE:
#
# Examine the relationship between wildfire-attributable PM2.5
# and moose detections using refined Accumulated Local Effects
# (ALE).
#
#
# THIS SCRIPT ANALYZES TWO PARTS:
#
# PART 1:
#
#     Probability that moose are detected
#
#
# PART 2:
#
#     Number of detections on camera-days when moose ARE
#     detected
#
#
# WHY ALE?
#
# Our original project used Partial Dependence Plots (PDPs).
#
# The problem was that PDP could produce positive-looking
# PM2.5 relationships even after PM2.5 had been randomized.
#
# ALE is better suited for this analysis because it evaluates
# local changes in predictions using observations that actually
# occur within each PM2.5 region.
#
#
# REFINED ALE APPROACH:
#
#   1. Restrict PM2.5 interpretation to the 1st-99th percentile.
#
#   2. Use more detailed PM2.5 intervals in the upper smoke
#      tail.
#
#   3. Calculate ALE separately for:
#
#        - detection probability
#        - positive detection counts
#
#   4. Compare the model-based ALE patterns with observed data
#      summaries.
#
#
# IMPORTANT:
#
# ALE represents MODEL ASSOCIATIONS.
#
# It does NOT establish that wildfire smoke causes changes in
# moose behavior.
#
# Script 15 will perform the 99-run permutation/null test to
# determine whether the observed ALE effects are stronger than
# effects produced after PM2.5 is randomized.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load cleaned moose dataset
# ------------------------------------------------------------

moose <- read.csv(
  "data-split/moose_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables as factors
# ------------------------------------------------------------

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



# ============================================================
# PART 1:
# FULL DETECTION-PROBABILITY RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 5. Prepare full detection-model data
# ------------------------------------------------------------
#
# Cell_ID is removed because it is only an identifier.
#
# Daily_Count is removed because Detection is directly derived
# from Daily_Count.

detection.full <- moose

detection.full$Cell_ID <- NULL

detection.full$Daily_Count <- NULL



# ------------------------------------------------------------
# 6. Fit full detection Random Forest
# ------------------------------------------------------------
#
# This model uses ALL cleaned moose observations because the
# train/test model in Script 13 was used for validation.
#
# The full model is used here for interpretation.

moose.detection.rf.full <- ranger(
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
# 7. Print detection model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL MOOSE DETECTION MODEL\n"
)

cat(
  "====================================================\n"
)

print(
  moose.detection.rf.full
)



# ============================================================
# PART 2:
# FULL POSITIVE-COUNT RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 8. Keep only camera-days with moose detections
# ------------------------------------------------------------

positive.data <- moose[
  moose$Daily_Count > 0,
]



# ------------------------------------------------------------
# 9. Prepare positive-count model data
# ------------------------------------------------------------

positive.model.data <- positive.data

positive.model.data$Cell_ID <- NULL

positive.model.data$Detection <- NULL



# ------------------------------------------------------------
# 10. Fit full positive-count Random Forest
# ------------------------------------------------------------

moose.positive.rf.full <- ranger(
  Daily_Count ~ .,
  data = positive.model.data,
  num.trees = 500,
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 11. Print positive-count model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL MOOSE POSITIVE-COUNT MODEL\n"
)

cat(
  "====================================================\n"
)

print(
  moose.positive.rf.full
)



# ============================================================
# DEFINE CENTRAL PM2.5 INTERPRETATION RANGE
# ============================================================



# ------------------------------------------------------------
# 12. Calculate 1st and 99th percentiles
# ------------------------------------------------------------
#
# Wildfire PM2.5 is strongly right-skewed.
#
# We restrict interpretation to the central 98% of observations
# so a small number of extreme smoke values do not dominate the
# ALE results.

smoke.lower <- as.numeric(
  quantile(
    moose$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)

smoke.upper <- as.numeric(
  quantile(
    moose$scale.smoke,
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
# 13. Restrict data to central smoke range
# ------------------------------------------------------------

central.data <- moose[
  moose$scale.smoke >= smoke.lower &
    moose$scale.smoke <= smoke.upper,
]


cat(
  "\nCentral observations:",
  nrow(
    central.data
  ),
  "\n"
)

cat(
  "Observations excluded from ALE interpretation:",
  nrow(moose) -
    nrow(central.data),
  "\n"
)



# ------------------------------------------------------------
# 14. Define refined ALE quantile probabilities
# ------------------------------------------------------------
#
# Extra boundaries are added in the upper tail because most
# camera-days have low wildfire PM2.5, while relatively few
# observations contain high smoke levels.

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
# CREATE ALE BREAKS
# ============================================================



# ------------------------------------------------------------
# 15. Function to create unique ALE boundaries
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
# 16. Detection ALE boundaries
# ------------------------------------------------------------

detection.breaks <- create_ale_breaks(
  x = central.data$scale.smoke,
  probabilities = ale.probabilities
)



# ------------------------------------------------------------
# 17. Positive-count ALE boundaries
# ------------------------------------------------------------

positive.central <- central.data[
  central.data$Daily_Count > 0,
]


cat(
  "Positive detection days in central range:",
  nrow(
    positive.central
  ),
  "\n"
)


positive.breaks <- create_ale_breaks(
  x = positive.central$scale.smoke,
  probabilities = ale.probabilities
)



# ============================================================
# DETECTION ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 18. Calculate classification ALE
# ------------------------------------------------------------

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
# 19. Calculate regression ALE
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
# CALCULATE DETECTION ALE
# ============================================================



# ------------------------------------------------------------
# 20. Prepare central detection-model data
# ------------------------------------------------------------

detection.central <- central.data

detection.central$Cell_ID <- NULL

detection.central$Daily_Count <- NULL



# ------------------------------------------------------------
# 21. Calculate detection ALE
# ------------------------------------------------------------

detection.ale <- calculate_detection_ale(
  model = moose.detection.rf.full,
  data = detection.central,
  breaks = detection.breaks
)



# ------------------------------------------------------------
# 22. Print detection ALE table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "REFINED MOOSE DETECTION ALE\n"
)

cat(
  "====================================================\n"
)

print(
  detection.ale
)



# ------------------------------------------------------------
# 23. Calculate low-to-high detection ALE change
# ------------------------------------------------------------

detection.low.to.high <-
  detection.ale$ALE[
    nrow(
      detection.ale
    )
  ] -
  detection.ale$ALE[1]


cat(
  "\nDetection ALE low-to-high change:",
  round(
    detection.low.to.high,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 24. Largest detection local effect
# ------------------------------------------------------------

largest.detection.local <-
  max(
    abs(
      detection.ale$Local_Effect
    ),
    na.rm = TRUE
  )


total.detection.local <-
  sum(
    abs(
      detection.ale$Local_Effect
    ),
    na.rm = TRUE
  )


largest.detection.percent <-
  (
    largest.detection.local /
      total.detection.local
  ) * 100


cat(
  "Largest detection local effect:",
  round(
    largest.detection.local,
    6
  ),
  "\n"
)

cat(
  "Largest detection interval as % of total absolute movement:",
  round(
    largest.detection.percent,
    2
  ),
  "%",
  "\n"
)



# ============================================================
# CALCULATE POSITIVE-COUNT ALE
# ============================================================



# ------------------------------------------------------------
# 25. Prepare positive-count central model data
# ------------------------------------------------------------

positive.central.model <-
  positive.central

positive.central.model$Cell_ID <- NULL

positive.central.model$Detection <- NULL



# ------------------------------------------------------------
# 26. Calculate positive-count ALE
# ------------------------------------------------------------

positive.ale <- calculate_regression_ale(
  model = moose.positive.rf.full,
  data = positive.central.model,
  breaks = positive.breaks
)



# ------------------------------------------------------------
# 27. Print positive-count ALE table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "REFINED MOOSE POSITIVE-COUNT ALE\n"
)

cat(
  "====================================================\n"
)

print(
  positive.ale
)



# ------------------------------------------------------------
# 28. Calculate low-to-high positive-count ALE change
# ------------------------------------------------------------

positive.low.to.high <-
  positive.ale$ALE[
    nrow(
      positive.ale
    )
  ] -
  positive.ale$ALE[1]


cat(
  "\nPositive-count ALE low-to-high change:",
  round(
    positive.low.to.high,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 29. Largest positive-count local effect
# ------------------------------------------------------------

largest.positive.local <-
  max(
    abs(
      positive.ale$Local_Effect
    ),
    na.rm = TRUE
  )


total.positive.local <-
  sum(
    abs(
      positive.ale$Local_Effect
    ),
    na.rm = TRUE
  )


largest.positive.percent <-
  (
    largest.positive.local /
      total.positive.local
  ) * 100


cat(
  "Largest positive-count local effect:",
  round(
    largest.positive.local,
    6
  ),
  "\n"
)

cat(
  "Largest positive-count interval as % of total absolute movement:",
  round(
    largest.positive.percent,
    2
  ),
  "%",
  "\n"
)



# ============================================================
# OBSERVED DETECTION-RATE SUMMARIES
# ============================================================



# ------------------------------------------------------------
# 30. Assign observed detection data to ALE intervals
# ------------------------------------------------------------

central.data$Detection_Bin <- cut(
  central.data$scale.smoke,
  breaks = detection.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 31. Calculate observed detection rate by smoke interval
# ------------------------------------------------------------

observed.detection <- do.call(
  rbind,
  lapply(
    sort(
      unique(
        central.data$Detection_Bin[
          !is.na(
            central.data$Detection_Bin
          )
        ]
      )
    ),
    function(b) {
      
      temp <- central.data[
        central.data$Detection_Bin == b,
      ]
      
      data.frame(
        Bin = b,
        Mean_PM25 = mean(
          temp$scale.smoke
        ),
        Observations = nrow(
          temp
        ),
        Detection_Rate = mean(
          temp$Daily_Count > 0
        )
      )
    }
  )
)



# ------------------------------------------------------------
# 32. Print observed detection table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED MOOSE DETECTION RATE BY PM2.5\n"
)

cat(
  "====================================================\n"
)

print(
  observed.detection
)



# ============================================================
# OBSERVED POSITIVE-COUNT SUMMARIES
# ============================================================



# ------------------------------------------------------------
# 33. Assign positive observations to ALE intervals
# ------------------------------------------------------------

positive.central$Positive_Bin <- cut(
  positive.central$scale.smoke,
  breaks = positive.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 34. Calculate observed positive counts by smoke interval
# ------------------------------------------------------------

observed.positive <- do.call(
  rbind,
  lapply(
    sort(
      unique(
        positive.central$Positive_Bin[
          !is.na(
            positive.central$Positive_Bin
          )
        ]
      )
    ),
    function(b) {
      
      temp <- positive.central[
        positive.central$Positive_Bin == b,
      ]
      
      data.frame(
        Bin = b,
        Mean_PM25 = mean(
          temp$scale.smoke
        ),
        Observations = nrow(
          temp
        ),
        Mean_Positive_Count = mean(
          temp$Daily_Count
        )
      )
    }
  )
)



# ------------------------------------------------------------
# 35. Print observed positive-count table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED MOOSE POSITIVE COUNTS BY PM2.5\n"
)

cat(
  "====================================================\n"
)

print(
  observed.positive
)



# ============================================================
# SAVE TABLES
# ============================================================



# ------------------------------------------------------------
# 36. Create results directory if needed
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 37. Save detection ALE
# ------------------------------------------------------------

write.csv(
  detection.ale,
  "results/moose_v2_refined_detection_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 38. Save positive-count ALE
# ------------------------------------------------------------

write.csv(
  positive.ale,
  "results/moose_v2_refined_positive_count_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 39. Save observed detection bins
# ------------------------------------------------------------

write.csv(
  observed.detection,
  "results/moose_v2_refined_observed_detection.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 40. Save observed positive-count bins
# ------------------------------------------------------------

write.csv(
  observed.positive,
  "results/moose_v2_refined_observed_positive_counts.csv",
  row.names = FALSE
)



# ============================================================
# PLOT 1:
# REFINED DETECTION ALE
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
    title =
      "Refined Moose Detection Response to Wildfire PM2.5",
    subtitle =
      "ALE restricted to the central 98% of observed smoke values",
    x =
      "Wildfire-attributable PM2.5 (scaled)",
    y =
      "ALE Effect on Detection Probability"
  ) +
  
  theme_minimal()


print(
  detection.ale.plot
)


ggsave(
  filename =
    "results/moose_v2_refined_detection_ale.png",
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
    title =
      "Observed Moose Detection Rate by PM2.5",
    subtitle =
      "Restricted to 1st-99th percentile of smoke",
    x =
      "Mean Wildfire-attributable PM2.5 (scaled)",
    y =
      "Observed Detection Rate"
  ) +
  
  theme_minimal()


print(
  observed.detection.plot
)


ggsave(
  filename =
    "results/moose_v2_refined_observed_detection.png",
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
    title =
      "Refined Moose Positive-Count Response to Wildfire PM2.5",
    subtitle =
      "ALE restricted to the central 98% of observed smoke values",
    x =
      "Wildfire-attributable PM2.5 (scaled)",
    y =
      "ALE Effect on Positive Daily Count"
  ) +
  
  theme_minimal()


print(
  positive.ale.plot
)


ggsave(
  filename =
    "results/moose_v2_refined_positive_count_ale.png",
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
    title =
      "Observed Moose Positive Counts by PM2.5",
    subtitle =
      "Restricted to 1st-99th percentile of smoke",
    x =
      "Mean Wildfire-attributable PM2.5 (scaled)",
    y =
      "Mean Count on Positive-Detection Days"
  ) +
  
  theme_minimal()


print(
  observed.positive.plot
)


ggsave(
  filename =
    "results/moose_v2_refined_observed_positive_counts.png",
  plot =
    observed.positive.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE VERSION 2 REFINED ALE SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "\n--- DETECTION MODEL ---\n"
)

cat(
  "ALE low-to-high change:",
  round(
    detection.low.to.high,
    6
  ),
  "\n"
)

cat(
  "Largest local effect:",
  round(
    largest.detection.local,
    6
  ),
  "\n"
)

cat(
  "Largest interval contribution:",
  round(
    largest.detection.percent,
    2
  ),
  "%",
  "\n"
)


cat(
  "\n--- POSITIVE-COUNT MODEL ---\n"
)

cat(
  "ALE low-to-high change:",
  round(
    positive.low.to.high,
    6
  ),
  "\n"
)

cat(
  "Largest local effect:",
  round(
    largest.positive.local,
    6
  ),
  "\n"
)

cat(
  "Largest interval contribution:",
  round(
    largest.positive.percent,
    2
  ),
  "%",
  "\n"
)

cat(
  "====================================================\n"
)