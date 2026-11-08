# ============================================================
# 11_ale_diagnostics_mule_deer.R
#
# VERSION 2 PM2.5 INTERPRETATION
#
# PURPOSE:
#
# Examine the relationship between wildfire-attributable PM2.5
# and mule deer detections using:
#
#   1. Accumulated Local Effects (ALE)
#   2. Observed smoke-bin summaries
#
#
# WHY ARE WE USING ALE?
#
# Our original analysis used Partial Dependence Plots (PDPs).
#
# The problem with PDP is that it can create unrealistic
# combinations of predictors.
#
# For example, PDP may take a camera-day that originally
# occurred under very low smoke and ask:
#
#     "What would the prediction be if this exact camera-day
#      suddenly had extremely high smoke?"
#
# while leaving temperature, season, habitat, etc. unchanged.
#
# That can be problematic because wildfire smoke is related to
# other environmental variables such as:
#
#     temperature
#     season
#     weather conditions
#
#
# Our shuffle test showed that the PDP approach was producing
# positive-looking PM2.5 responses even after smoke was
# randomized.
#
#
# ALE works differently.
#
# Instead of forcing every observation across the entire smoke
# range, ALE asks:
#
#     "Within a region of PM2.5 where observations actually
#      exist, how does increasing smoke slightly change the
#      prediction?"
#
# It calculates these LOCAL changes and then accumulates them
# across the observed PM2.5 range.
#
#
# THIS SCRIPT ANALYZES BOTH PARTS OF OUR NEW MODEL:
#
# PART 1:
#
#     Probability that mule deer are detected
#
#
# PART 2:
#
#     Number of detections when mule deer ARE detected
#
#
# IMPORTANT:
#
# ALE shows MODEL ASSOCIATIONS.
#
# It does NOT prove that wildfire smoke causes changes in
# mule deer behavior.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load the cleaned mule deer dataset
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
# 4. Re-create the binary Detection variable
# ------------------------------------------------------------
#
# Part 1 of our two-part model predicts whether at least one
# mule deer was detected on a camera-day.

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
# PART 1:
# FULL DETECTION MODEL
# ============================================================



# ------------------------------------------------------------
# 5. Prepare data for the full detection model
# ------------------------------------------------------------
#
# For interpretation, we now use ALL cleaned mule deer
# observations.
#
# Cell_ID is removed because it is only an identifier.
#
# Daily_Count is removed because Detection is derived directly
# from Daily_Count. Keeping Daily_Count would create leakage.

detection.data <- mule


detection.data$Cell_ID <- NULL

detection.data$Daily_Count <- NULL



# ------------------------------------------------------------
# 6. Fit the full detection Random Forest
# ------------------------------------------------------------
#
# These settings are identical to Script 10:
#
#     500 trees
#     min.node.size = 20
#     probability forest
#
# probability = TRUE means the model gives us:
#
#     P(No detection)
#
# and
#
#     P(Detection)
#
# We care about P(Detection = Yes).

mule.detection.rf.full <- ranger(
  Detection ~ .,
  data = detection.data,
  num.trees = 500,
  min.node.size = 20,
  probability = TRUE,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)



# ------------------------------------------------------------
# 7. Print detection-model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL MULE DEER DETECTION MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  mule.detection.rf.full
)



# ============================================================
# PART 2:
# FULL POSITIVE-COUNT MODEL
# ============================================================



# ------------------------------------------------------------
# 8. Keep only camera-days with mule deer detections
# ------------------------------------------------------------

positive.data <- mule[
  mule$Daily_Count > 0,
]



# ------------------------------------------------------------
# 9. Prepare data for the positive-count model
# ------------------------------------------------------------
#
# Cell_ID is only an identifier.
#
# Detection is always "Yes" in this subset, so it provides no
# useful information and is also removed.

positive.model.data <- positive.data


positive.model.data$Cell_ID <- NULL

positive.model.data$Detection <- NULL



# ------------------------------------------------------------
# 10. Fit full positive-count Random Forest
# ------------------------------------------------------------

mule.positive.rf.full <- ranger(
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
  "FULL MULE DEER POSITIVE-COUNT MODEL\n"
)

cat(
  "====================================================\n"
)


print(
  mule.positive.rf.full
)



# ============================================================
# WHAT IS ALE?
# ============================================================
#
# We are going to calculate ALE manually.
#
#
# STEP 1:
#
# Divide the observed PM2.5 values into approximately equal-
# sized bins.
#
#
# STEP 2:
#
# For observations actually located inside each bin, predict
# them twice:
#
#     once at the LOWER PM2.5 boundary
#
#     once at the UPPER PM2.5 boundary
#
#
# STEP 3:
#
# Calculate:
#
#     prediction at upper boundary
#                 -
#     prediction at lower boundary
#
#
# This gives a LOCAL effect of increasing PM2.5 within that
# region of the data.
#
#
# STEP 4:
#
# Average those local effects within each PM2.5 bin.
#
#
# STEP 5:
#
# Accumulate the effects from low smoke to high smoke.
#
#
# STEP 6:
#
# Center the ALE curve around zero.
#
#
# INTERPRETATION:
#
# ALE above zero:
#
#     PM2.5 in that region pushes predictions above the model's
#     overall average effect.
#
#
# ALE below zero:
#
#     PM2.5 in that region pushes predictions below the model's
#     overall average effect.
#
#
# The SHAPE and CHANGE across the curve are more important than
# whether a particular point happens to be above or below zero.
#
# ============================================================



# ============================================================
# DETECTION-MODEL ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 12. Create a function for classification ALE
# ------------------------------------------------------------
#
# This function is specifically designed for our probability
# Random Forest.
#
# It calculates the ALE effect on:
#
#     Probability of Detection = "Yes"
#
#
# Arguments:
#
# model:
#     trained ranger probability model
#
# data:
#     dataset used for interpretation
#
# variable:
#     predictor whose effect we want to measure
#
# number.bins:
#     approximate number of local PM2.5 regions

calculate_detection_ale <- function(
    model,
    data,
    variable = "scale.smoke",
    number.bins = 10
) {
  
  
  # ----------------------------------------------------------
  # A. Extract the PM2.5 values
  # ----------------------------------------------------------
  
  x <- data[[variable]]
  
  
  
  # ----------------------------------------------------------
  # B. Create quantile-based bin boundaries
  # ----------------------------------------------------------
  #
  # Quantile bins try to place roughly similar numbers of
  # observations into each region.
  #
  # This is useful because the smoke distribution is extremely
  # skewed.
  #
  # unique() removes duplicate boundaries.
  #
  # Duplicate boundaries can occur because many camera-days
  # have nearly identical very-low smoke values.
  
  breaks <- unique(
    as.numeric(
      quantile(
        x,
        probs = seq(
          0,
          1,
          length.out = number.bins + 1
        ),
        na.rm = TRUE
      )
    )
  )
  
  
  
  # ----------------------------------------------------------
  # C. Make sure we have enough unique boundaries
  # ----------------------------------------------------------
  
  if (length(breaks) < 3) {
    
    stop(
      "Not enough unique PM2.5 values to calculate ALE."
    )
  }
  
  
  
  # ----------------------------------------------------------
  # D. Assign every observation to a PM2.5 interval
  # ----------------------------------------------------------
  
  bin.id <- cut(
    x,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  
  number.intervals <- length(breaks) - 1
  
  
  
  # ----------------------------------------------------------
  # E. Create storage
  # ----------------------------------------------------------
  
  local.effects <- numeric(
    number.intervals
  )
  
  
  observations.per.bin <- integer(
    number.intervals
  )
  
  
  
  # ----------------------------------------------------------
  # F. Calculate the LOCAL PM2.5 effect in each bin
  # ----------------------------------------------------------
  
  for (b in 1:number.intervals) {
    
    
    # Find observations whose real PM2.5 value falls inside
    # this particular interval.
    
    rows.in.bin <- which(
      bin.id == b
    )
    
    
    observations.per.bin[b] <-
      length(rows.in.bin)
    
    
    # If somehow a bin contains no observations, store zero.
    
    if (length(rows.in.bin) == 0) {
      
      local.effects[b] <- 0
      
      next
    }
    
    
    
    # --------------------------------------------------------
    # Create two copies of ONLY these observations
    # --------------------------------------------------------
    
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
    
    
    
    # --------------------------------------------------------
    # Set PM2.5 to the lower and upper boundaries
    # --------------------------------------------------------
    
    lower.data[[variable]] <-
      breaks[b]
    
    
    upper.data[[variable]] <-
      breaks[b + 1]
    
    
    
    # --------------------------------------------------------
    # Predict probability of mule deer detection
    # --------------------------------------------------------
    
    lower.prediction <- predict(
      model,
      data = lower.data
    )$predictions[, "Yes"]
    
    
    upper.prediction <- predict(
      model,
      data = upper.data
    )$predictions[, "Yes"]
    
    
    
    # --------------------------------------------------------
    # Calculate LOCAL changes
    # --------------------------------------------------------
    #
    # For every observation:
    #
    #     probability at upper PM2.5 boundary
    #
    # minus
    #
    #     probability at lower PM2.5 boundary
    
    prediction.difference <-
      upper.prediction -
      lower.prediction
    
    
    
    # Average the local effects for observations actually
    # located in this PM2.5 region.
    
    local.effects[b] <- mean(
      prediction.difference
    )
  }
  
  
  
  # ----------------------------------------------------------
  # G. Accumulate local effects
  # ----------------------------------------------------------
  #
  # cumsum() adds each local effect to everything before it.
  #
  # This turns our local changes into an ALE curve.
  
  accumulated.effects <- cumsum(
    local.effects
  )
  
  
  
  # ----------------------------------------------------------
  # H. Center ALE around zero
  # ----------------------------------------------------------
  #
  # ALE is normally centered so that its average effect across
  # the observed data is zero.
  #
  # We weight each interval by how many real observations occur
  # in that interval.
  
  center.value <- weighted.mean(
    accumulated.effects,
    w = observations.per.bin
  )
  
  
  centered.ale <-
    accumulated.effects -
    center.value
  
  
  
  # ----------------------------------------------------------
  # I. Return results
  # ----------------------------------------------------------
  
  results <- data.frame(
    
    Bin = 1:number.intervals,
    
    Lower_PM25 = breaks[
      1:number.intervals
    ],
    
    Upper_PM25 = breaks[
      2:(number.intervals + 1)
    ],
    
    Midpoint_PM25 = (
      breaks[1:number.intervals] +
        breaks[2:(number.intervals + 1)]
    ) / 2,
    
    Observations = observations.per.bin,
    
    Local_Effect = local.effects,
    
    ALE = centered.ale
  )
  
  
  return(
    results
  )
}



# ============================================================
# POSITIVE-COUNT ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 13. Create regression ALE function
# ------------------------------------------------------------
#
# This works exactly like the previous function except the
# model predicts:
#
#     number of detections
#
# rather than:
#
#     probability of detection

calculate_regression_ale <- function(
    model,
    data,
    variable = "scale.smoke",
    number.bins = 10
) {
  
  
  x <- data[[variable]]
  
  
  
  # Create quantile-based PM2.5 boundaries.
  
  breaks <- unique(
    as.numeric(
      quantile(
        x,
        probs = seq(
          0,
          1,
          length.out = number.bins + 1
        ),
        na.rm = TRUE
      )
    )
  )
  
  
  
  if (length(breaks) < 3) {
    
    stop(
      "Not enough unique PM2.5 values to calculate ALE."
    )
  }
  
  
  
  # Assign each observation to its real PM2.5 region.
  
  bin.id <- cut(
    x,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  
  number.intervals <- length(breaks) - 1
  
  
  local.effects <- numeric(
    number.intervals
  )
  
  
  observations.per.bin <- integer(
    number.intervals
  )
  
  
  
  # Calculate local effects.
  
  for (b in 1:number.intervals) {
    
    
    rows.in.bin <- which(
      bin.id == b
    )
    
    
    observations.per.bin[b] <-
      length(rows.in.bin)
    
    
    if (length(rows.in.bin) == 0) {
      
      local.effects[b] <- 0
      
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
    
    
    
    # Regression predictions are a single numerical vector.
    
    lower.prediction <- predict(
      model,
      data = lower.data
    )$predictions
    
    
    upper.prediction <- predict(
      model,
      data = upper.data
    )$predictions
    
    
    
    prediction.difference <-
      upper.prediction -
      lower.prediction
    
    
    local.effects[b] <- mean(
      prediction.difference
    )
  }
  
  
  
  accumulated.effects <- cumsum(
    local.effects
  )
  
  
  
  center.value <- weighted.mean(
    accumulated.effects,
    w = observations.per.bin
  )
  
  
  
  centered.ale <-
    accumulated.effects -
    center.value
  
  
  
  results <- data.frame(
    
    Bin = 1:number.intervals,
    
    Lower_PM25 = breaks[
      1:number.intervals
    ],
    
    Upper_PM25 = breaks[
      2:(number.intervals + 1)
    ],
    
    Midpoint_PM25 = (
      breaks[1:number.intervals] +
        breaks[2:(number.intervals + 1)]
    ) / 2,
    
    Observations = observations.per.bin,
    
    Local_Effect = local.effects,
    
    ALE = centered.ale
  )
  
  
  return(
    results
  )
}



# ============================================================
# CALCULATE DETECTION ALE
# ============================================================



# ------------------------------------------------------------
# 14. Calculate PM2.5 ALE for detection probability
# ------------------------------------------------------------
#
# We start with 10 quantile-based regions.
#
# Because they are based on the actual PM2.5 distribution, each
# region should contain a meaningful amount of real data.

detection.ale <- calculate_detection_ale(
  model = mule.detection.rf.full,
  data = detection.data,
  variable = "scale.smoke",
  number.bins = 10
)



cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER DETECTION PM2.5 ALE\n"
)

cat(
  "====================================================\n"
)


print(
  detection.ale
)



# ------------------------------------------------------------
# 15. Calculate overall detection-ALE change
# ------------------------------------------------------------
#
# Unlike our old PDP calculation, these endpoints are based on
# accumulated LOCAL effects from observed PM2.5 regions.
#
# Positive:
#
#     detection probability generally rises from low to high
#     smoke
#
# Negative:
#
#     detection probability generally falls
#
# Near zero:
#
#     little overall change
#
# We will still NOT trust this statistic by itself.
#
# Script 12 will compare it against shuffled-smoke ALE curves.

detection.ale.change <-
  detection.ale$ALE[
    nrow(detection.ale)
  ] -
  detection.ale$ALE[1]


cat(
  "\nDetection ALE low-to-high change:",
  round(
    detection.ale.change,
    6
  ),
  "\n"
)



# ============================================================
# CALCULATE POSITIVE-COUNT ALE
# ============================================================



# ------------------------------------------------------------
# 16. Calculate PM2.5 ALE among detection days
# ------------------------------------------------------------

positive.ale <- calculate_regression_ale(
  model = mule.positive.rf.full,
  data = positive.model.data,
  variable = "scale.smoke",
  number.bins = 10
)



cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER POSITIVE-COUNT PM2.5 ALE\n"
)

cat(
  "====================================================\n"
)


print(
  positive.ale
)



# ------------------------------------------------------------
# 17. Calculate overall positive-count ALE change
# ------------------------------------------------------------

positive.ale.change <-
  positive.ale$ALE[
    nrow(positive.ale)
  ] -
  positive.ale$ALE[1]


cat(
  "\nPositive-count ALE low-to-high change:",
  round(
    positive.ale.change,
    6
  ),
  "\n"
)



# ============================================================
# OBSERVED DATA CHECK
# ============================================================
#
# A model interpretation should make sense when compared with
# the actual observations.
#
# Therefore, we now create observed PM2.5-bin summaries.
#
# These plots DO NOT control for other predictors.
#
# They are not replacements for the Random Forest.
#
# They are simply an important reality check.
# ============================================================



# ------------------------------------------------------------
# 18. Create observed bins using DETECTION ALE boundaries
# ------------------------------------------------------------
#
# We reuse exactly the boundaries created by ALE.
#
# This makes the observed-data plot directly comparable to the
# detection ALE curve.

detection.breaks <- c(
  detection.ale$Lower_PM25[1],
  detection.ale$Upper_PM25
)


mule$detection.smoke.bin <- cut(
  mule$scale.smoke,
  breaks = detection.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 19. Create observed detection summary
# ------------------------------------------------------------

observed.detection <- data.frame()


for (b in 1:nrow(detection.ale)) {
  
  
  rows.in.bin <- which(
    mule$detection.smoke.bin == b
  )
  
  
  if (length(rows.in.bin) == 0) {
    
    next
  }
  
  
  bin.summary <- data.frame(
    
    Bin = b,
    
    Mean_PM25 = mean(
      mule$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Observations = length(
      rows.in.bin
    ),
    
    Detection_Days = sum(
      mule$Detection[
        rows.in.bin
      ] == "Yes"
    ),
    
    Detection_Rate = mean(
      mule$Detection[
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
  "OBSERVED DETECTION RATE BY PM2.5 BIN\n"
)

cat(
  "====================================================\n"
)


print(
  observed.detection
)



# ------------------------------------------------------------
# 20. Create observed bins for positive-count data
# ------------------------------------------------------------

positive.breaks <- c(
  positive.ale$Lower_PM25[1],
  positive.ale$Upper_PM25
)


positive.data$positive.smoke.bin <- cut(
  positive.data$scale.smoke,
  breaks = positive.breaks,
  include.lowest = TRUE,
  labels = FALSE
)



# ------------------------------------------------------------
# 21. Create observed positive-count summary
# ------------------------------------------------------------

observed.positive <- data.frame()


for (b in 1:nrow(positive.ale)) {
  
  
  rows.in.bin <- which(
    positive.data$positive.smoke.bin == b
  )
  
  
  if (length(rows.in.bin) == 0) {
    
    next
  }
  
  
  bin.summary <- data.frame(
    
    Bin = b,
    
    Mean_PM25 = mean(
      positive.data$scale.smoke[
        rows.in.bin
      ]
    ),
    
    Observations = length(
      rows.in.bin
    ),
    
    Mean_Positive_Count = mean(
      positive.data$Daily_Count[
        rows.in.bin
      ]
    ),
    
    Median_Positive_Count = median(
      positive.data$Daily_Count[
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
  "OBSERVED POSITIVE COUNTS BY PM2.5 BIN\n"
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



# ------------------------------------------------------------
# 22. Make sure results folder exists
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)



# ============================================================
# SAVE TABLES
# ============================================================



# ------------------------------------------------------------
# 23. Save detection ALE
# ------------------------------------------------------------

write.csv(
  detection.ale,
  "results/mule_deer_v2_detection_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 24. Save positive-count ALE
# ------------------------------------------------------------

write.csv(
  positive.ale,
  "results/mule_deer_v2_positive_count_ale.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 25. Save observed detection-bin results
# ------------------------------------------------------------

write.csv(
  observed.detection,
  "results/mule_deer_v2_observed_detection_bins.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 26. Save observed positive-count results
# ------------------------------------------------------------

write.csv(
  observed.positive,
  "results/mule_deer_v2_observed_positive_count_bins.csv",
  row.names = FALSE
)



# ============================================================
# PLOT 1:
# DETECTION-PROBABILITY ALE
# ============================================================



# ------------------------------------------------------------
# 27. Plot ALE for probability of detection
# ------------------------------------------------------------
#
# Horizontal zero line:
#
#     average modeled PM2.5 effect
#
#
# A rising curve means higher PM2.5 regions tend to push
# detection probability upward.
#
# A falling curve means higher PM2.5 regions tend to push it
# downward.

detection.ale.plot <- ggplot(
  detection.ale,
  aes(
    x = Midpoint_PM25,
    y = ALE
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Mule Deer Detection Response to Wildfire PM2.5",
    subtitle = "Accumulated Local Effects (ALE)",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "ALE Effect on Detection Probability"
  ) +
  
  theme_minimal()


print(
  detection.ale.plot
)



ggsave(
  filename = "results/mule_deer_v2_detection_ale.png",
  plot = detection.ale.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 2:
# OBSERVED DETECTION RATE
# ============================================================



# ------------------------------------------------------------
# 28. Plot actual observed detection rate
# ------------------------------------------------------------

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
    subtitle = "Observed camera-days grouped using ALE smoke bins",
    x = "Mean Wildfire-attributable PM2.5 (scaled)",
    y = "Observed Detection Rate"
  ) +
  
  theme_minimal()


print(
  observed.detection.plot
)



ggsave(
  filename = "results/mule_deer_v2_observed_detection_rate.png",
  plot = observed.detection.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 3:
# POSITIVE-COUNT ALE
# ============================================================



# ------------------------------------------------------------
# 29. Plot ALE for count when detected
# ------------------------------------------------------------

positive.ale.plot <- ggplot(
  positive.ale,
  aes(
    x = Midpoint_PM25,
    y = ALE
  )
) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title = "Mule Deer Detection Count Response to Wildfire PM2.5",
    subtitle = "ALE among camera-days with mule deer detections",
    x = "Wildfire-attributable PM2.5 (scaled)",
    y = "ALE Effect on Positive Daily Count"
  ) +
  
  theme_minimal()


print(
  positive.ale.plot
)



ggsave(
  filename = "results/mule_deer_v2_positive_count_ale.png",
  plot = positive.ale.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# PLOT 4:
# OBSERVED POSITIVE COUNTS
# ============================================================



# ------------------------------------------------------------
# 30. Plot actual counts on detection days
# ------------------------------------------------------------

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
    subtitle = "Observed positive camera-days grouped by PM2.5",
    x = "Mean Wildfire-attributable PM2.5 (scaled)",
    y = "Mean Daily Count When Detected"
  ) +
  
  theme_minimal()


print(
  observed.positive.plot
)



ggsave(
  filename = "results/mule_deer_v2_observed_positive_counts.png",
  plot = observed.positive.plot,
  width = 8,
  height = 6,
  dpi = 300
)



# ============================================================
# FINAL SUMMARY
# ============================================================



# ------------------------------------------------------------
# 31. Print the important ALE results
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER VERSION 2 PM2.5 DIAGNOSTIC SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Detection ALE intervals:",
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
  "Positive-count ALE intervals:",
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
  "\nIMPORTANT:\n"
)


cat(
  "These ALE directions are NOT considered validated yet.\n"
)


cat(
  "The next script will shuffle PM2.5 and repeat the ALE\n"
)


cat(
  "analysis to determine whether these effects survive a\n"
)


cat(
  "null/randomization test.\n"
)


cat(
  "====================================================\n"
)