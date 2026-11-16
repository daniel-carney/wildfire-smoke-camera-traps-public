# ============================================================
# 18_moose_daily_count_pm25_ale.R
#
# MOOSE PM2.5 ACCUMULATED LOCAL EFFECTS
#
# PURPOSE:
#
# Examine the modeled relationship between wildfire-related
# PM2.5 and daily moose detection counts.
#
# This script uses the finalized Random Forest settings:
#
#   Trees = 500
#   mtry = 3
#   Minimum node size = 20
#   Split rule = Poisson
#
# The model is fit using all cleaned moose observations for
# interpretation.
#
# The PM2.5 response is summarized using:
#
#   1. Accumulated Local Effects (ALE)
#   2. Raw observed daily counts within the same PM2.5 bins
#
# ALE shows the modeled association with PM2.5 after accounting
# for the other predictors in the Random Forest.
#
# It does not establish a causal effect of smoke.
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
# 3. Restore categorical variables
# ------------------------------------------------------------

moose$Road_Type <- as.factor(
  moose$Road_Type
)

moose$Habitat_Type <- as.factor(
  moose$Habitat_Type
)

moose$Land_Mgnt <- as.factor(
  moose$Land_Mgnt
)

moose$Cell_ID <- as.factor(
  moose$Cell_ID
)


# ============================================================
# SELECT MODEL VARIABLES
# ============================================================


# ------------------------------------------------------------
# 4. Keep the finalized model variables
# ------------------------------------------------------------

moose.model.data <- moose[
  ,
  c(
    "Cell_ID",
    "Daily_Count",
    "scale.smoke",
    "julian",
    "scale.temp",
    "Road_Type",
    "Cam_Height",
    "Distance_to_Road",
    "Habitat_Type",
    "Land_Mgnt",
    "elev",
    "wadnr"
  )
]


# ------------------------------------------------------------
# 5. Remove incomplete observations
# ------------------------------------------------------------

moose.model.data <- moose.model.data[
  complete.cases(
    moose.model.data
  ),
]


# ============================================================
# FIT FULL-DATA INTERPRETATION MODEL
# ============================================================


# ------------------------------------------------------------
# 6. Fit finalized Poisson Random Forest
# ------------------------------------------------------------

set.seed(42)

moose.rf.full <- ranger(
  
  Daily_Count ~ .,
  
  data =
    moose.model.data,
  
  num.trees =
    500,
  
  mtry =
    3,
  
  min.node.size =
    20,
  
  splitrule =
    "poisson",
  
  poisson.tau =
    1,
  
  importance =
    "permutation",
  
  respect.unordered.factors =
    "order",
  
  seed =
    42
)


cat(
  "\n====================================================\n"
)

cat(
  "MOOSE FULL-DATA INTERPRETATION MODEL\n"
)

cat(
  "====================================================\n"
)

cat(
  "Observations:",
  nrow(
    moose.model.data
  ),
  "\n"
)

cat(
  "Camera sites:",
  length(
    unique(
      moose.model.data$Cell_ID
    )
  ),
  "\n"
)

cat(
  "OOB R-squared:",
  round(
    moose.rf.full$r.squared,
    6
  ),
  "\n"
)


# ============================================================
# DEFINE PM2.5 ANALYSIS RANGE
# ============================================================


# ------------------------------------------------------------
# 7. Use the central 1st through 99th percentile
#
# The smoke distribution is strongly right-skewed.
#
# Using the central 98% prevents a few extreme observations
# from dominating the ALE calculation.
# ------------------------------------------------------------

smoke.lower <- as.numeric(
  quantile(
    moose.model.data$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)

smoke.upper <- as.numeric(
  quantile(
    moose.model.data$scale.smoke,
    probs = 0.99,
    na.rm = TRUE
  )
)


cat(
  "\nPM2.5 1st percentile:",
  smoke.lower,
  "\n"
)

cat(
  "PM2.5 99th percentile:",
  smoke.upper,
  "\n"
)


# ------------------------------------------------------------
# 8. Keep observations inside central PM2.5 range
# ------------------------------------------------------------

ale.data <- moose.model.data[
  moose.model.data$scale.smoke >=
    smoke.lower &
    moose.model.data$scale.smoke <=
    smoke.upper,
]


cat(
  "Observations included in ALE:",
  nrow(
    ale.data
  ),
  "\n"
)

cat(
  "Observations excluded from ALE:",
  nrow(
    moose.model.data
  ) -
    nrow(
      ale.data
    ),
  "\n"
)


# ============================================================
# CREATE PM2.5 BREAKS
# ============================================================


# ------------------------------------------------------------
# 9. Quantile probabilities
#
# More breaks are used in the upper part of the distribution
# because high-smoke observations are less common.
# ------------------------------------------------------------

ale.probs <- c(
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


# ------------------------------------------------------------
# 10. Convert probabilities into actual PM2.5 values
# ------------------------------------------------------------

smoke.breaks <- as.numeric(
  quantile(
    moose.model.data$scale.smoke,
    probs =
      ale.probs,
    na.rm =
      TRUE
  )
)


# Remove duplicate breaks if multiple quantiles have the same
# smoke value.

smoke.breaks <- unique(
  smoke.breaks
)


cat(
  "\nNumber of unique PM2.5 breaks:",
  length(
    smoke.breaks
  ),
  "\n"
)

cat(
  "Number of ALE intervals:",
  length(
    smoke.breaks
  ) - 1,
  "\n"
)


# ============================================================
# CUSTOM ALE CALCULATION
# ============================================================


# ------------------------------------------------------------
# 11. Create empty storage
# ------------------------------------------------------------

ale.intervals <- data.frame(
  
  Lower =
    numeric(),
  
  Upper =
    numeric(),
  
  Midpoint =
    numeric(),
  
  N =
    integer(),
  
  Local_Effect =
    numeric(),
  
  stringsAsFactors =
    FALSE
)


# ------------------------------------------------------------
# 12. Calculate local effects for every PM2.5 interval
# ------------------------------------------------------------

for (
  i in 1:(
    length(
      smoke.breaks
    ) - 1
  )
) {
  
  lower.value <- smoke.breaks[i]
  
  upper.value <- smoke.breaks[i + 1]
  
  
  # Include observations that actually fall inside this interval.
  
  if (
    i <
    length(
      smoke.breaks
    ) - 1
  ) {
    
    interval.rows <- which(
      
      ale.data$scale.smoke >=
        lower.value &
        
        ale.data$scale.smoke <
        upper.value
    )
    
  } else {
    
    interval.rows <- which(
      
      ale.data$scale.smoke >=
        lower.value &
        
        ale.data$scale.smoke <=
        upper.value
    )
  }
  
  
  # Skip empty intervals.
  
  if (
    length(
      interval.rows
    ) == 0
  ) {
    
    next
  }
  
  
  interval.data <- ale.data[
    interval.rows,
  ]
  
  
  # Create two copies of the same observations.
  #
  # In one copy, PM2.5 is set to the lower boundary.
  # In the other, it is set to the upper boundary.
  
  lower.data <- interval.data
  
  upper.data <- interval.data
  
  
  lower.data$scale.smoke <-
    lower.value
  
  upper.data$scale.smoke <-
    upper.value
  
  
  # Predict at the lower boundary.
  
  lower.pred <- predict(
    
    moose.rf.full,
    
    data =
      lower.data
    
  )$predictions
  
  
  # Predict at the upper boundary.
  
  upper.pred <- predict(
    
    moose.rf.full,
    
    data =
      upper.data
    
  )$predictions
  
  
  # Average prediction change inside this interval.
  
  local.effect <- mean(
    upper.pred -
      lower.pred
  )
  
  
  # Store interval results.
  
  ale.intervals <- rbind(
    
    ale.intervals,
    
    data.frame(
      
      Lower =
        lower.value,
      
      Upper =
        upper.value,
      
      Midpoint =
        (
          lower.value +
            upper.value
        ) / 2,
      
      N =
        length(
          interval.rows
        ),
      
      Local_Effect =
        local.effect
    )
  )
}


# ============================================================
# ACCUMULATE LOCAL EFFECTS
# ============================================================


# ------------------------------------------------------------
# 13. Cumulative ALE
# ------------------------------------------------------------

ale.intervals$ALE_Raw <- cumsum(
  ale.intervals$Local_Effect
)


# ------------------------------------------------------------
# 14. Center the ALE curve around zero
#
# ALE values are relative model effects, not predicted counts.
#
# Zero represents the average modeled PM2.5 effect.
# ------------------------------------------------------------

ale.weights <- ale.intervals$N /
  sum(
    ale.intervals$N
  )


ale.center <- sum(
  ale.intervals$ALE_Raw *
    ale.weights
)


ale.intervals$ALE <- (
  ale.intervals$ALE_Raw -
    ale.center
)


# ============================================================
# ALE SUMMARY
# ============================================================


# ------------------------------------------------------------
# 15. Overall low-to-high PM2.5 change
# ------------------------------------------------------------

overall.ale.change <- (
  tail(
    ale.intervals$ALE,
    1
  ) -
    head(
      ale.intervals$ALE,
      1
    )
)


# ------------------------------------------------------------
# 16. Largest local PM2.5 change
# ------------------------------------------------------------

largest.local.row <- which.max(
  abs(
    ale.intervals$Local_Effect
  )
)


largest.local.effect <-
  ale.intervals$Local_Effect[
    largest.local.row
  ]


largest.local.lower <-
  ale.intervals$Lower[
    largest.local.row
  ]


largest.local.upper <-
  ale.intervals$Upper[
    largest.local.row
  ]


# ------------------------------------------------------------
# 17. Percentage of observations in largest interval
# ------------------------------------------------------------

largest.interval.percent <- (
  ale.intervals$N[
    largest.local.row
  ] /
    sum(
      ale.intervals$N
    )
) * 100


cat(
  "\n====================================================\n"
)

cat(
  "MOOSE PM2.5 ALE SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Overall ALE low-to-high change:",
  round(
    overall.ale.change,
    6
  ),
  "\n"
)

cat(
  "Largest local effect:",
  round(
    largest.local.effect,
    6
  ),
  "\n"
)

cat(
  "Largest-effect interval:",
  round(
    largest.local.lower,
    4
  ),
  "to",
  round(
    largest.local.upper,
    4
  ),
  "\n"
)

cat(
  "Observations in largest-effect interval:",
  round(
    largest.interval.percent,
    2
  ),
  "%\n"
)


# ============================================================
# RAW OBSERVED DAILY COUNTS BY PM2.5
# ============================================================


# ------------------------------------------------------------
# 18. Assign every observation to the ALE bins
# ------------------------------------------------------------

observed.data <- ale.data


observed.data$PM25_Bin <- cut(
  
  observed.data$scale.smoke,
  
  breaks =
    smoke.breaks,
  
  include.lowest =
    TRUE,
  
  right =
    TRUE
)


# ------------------------------------------------------------
# 19. Remove observations that could not be assigned
# ------------------------------------------------------------

observed.data <- observed.data[
  !is.na(
    observed.data$PM25_Bin
  ),
]


# ------------------------------------------------------------
# 20. Summarize observed counts by PM2.5 bin
# ------------------------------------------------------------

observed.summary <- aggregate(
  
  cbind(
    Daily_Count,
    scale.smoke
  ) ~ PM25_Bin,
  
  data =
    observed.data,
  
  FUN =
    mean
)


names(
  observed.summary
)[
  names(
    observed.summary
  ) ==
    "Daily_Count"
] <- "Mean_Daily_Count"


names(
  observed.summary
)[
  names(
    observed.summary
  ) ==
    "scale.smoke"
] <- "Mean_PM25"


# ------------------------------------------------------------
# 21. Number of camera-days per bin
# ------------------------------------------------------------

bin.n <- aggregate(
  
  Daily_Count ~ PM25_Bin,
  
  data =
    observed.data,
  
  FUN =
    length
)


names(
  bin.n
)[2] <- "N"


# ------------------------------------------------------------
# 22. Total detections per bin
# ------------------------------------------------------------

bin.total <- aggregate(
  
  Daily_Count ~ PM25_Bin,
  
  data =
    observed.data,
  
  FUN =
    sum
)


names(
  bin.total
)[2] <- "Total_Detections"


# ------------------------------------------------------------
# 23. Combine raw summaries
# ------------------------------------------------------------

observed.summary <- merge(
  
  observed.summary,
  
  bin.n,
  
  by =
    "PM25_Bin"
)


observed.summary <- merge(
  
  observed.summary,
  
  bin.total,
  
  by =
    "PM25_Bin"
)


observed.summary <- observed.summary[
  order(
    observed.summary$Mean_PM25
  ),
]


rownames(
  observed.summary
) <- NULL


cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED DAILY COUNTS BY PM2.5 BIN\n"
)

cat(
  "====================================================\n"
)

print(
  observed.summary
)


# ============================================================
# CREATE RESULTS DIRECTORY
# ============================================================


# ------------------------------------------------------------
# 24. Create results folder if needed
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)


# ============================================================
# SAVE ALE RESULTS
# ============================================================


# ------------------------------------------------------------
# 25. Save detailed ALE table
# ------------------------------------------------------------

write.csv(
  
  ale.intervals,
  
  "results/Moose_Daily_Count_PM25_ALE.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 26. Save ALE summary
# ------------------------------------------------------------

ale.summary <- data.frame(
  
  PM25_1st_Percentile =
    smoke.lower,
  
  PM25_99th_Percentile =
    smoke.upper,
  
  Overall_ALE_Change =
    overall.ale.change,
  
  Largest_Local_Effect =
    largest.local.effect,
  
  Largest_Effect_Lower =
    largest.local.lower,
  
  Largest_Effect_Upper =
    largest.local.upper,
  
  Largest_Interval_Percent =
    largest.interval.percent
)


write.csv(
  
  ale.summary,
  
  "results/Moose_Daily_Count_PM25_ALE_Summary.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 27. Save observed-count summary
# ------------------------------------------------------------

write.csv(
  
  observed.summary,
  
  "results/Moose_Observed_Daily_Counts_By_PM25.csv",
  
  row.names =
    FALSE
)


# ============================================================
# ALE PLOT
# ============================================================


# ------------------------------------------------------------
# 28. Plot ALE relationship
# ------------------------------------------------------------

ale.plot <- ggplot(
  
  ale.intervals,
  
  aes(
    x =
      Midpoint,
    
    y =
      ALE
  )
  
) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  geom_hline(
    
    yintercept =
      0,
    
    linetype =
      "dashed"
  ) +
  
  labs(
    
    title =
      "Moose PM2.5 Accumulated Local Effects",
    
    subtitle =
      "Random Forest modeled association with daily detection count",
    
    x =
      "Standardized Wildfire-Attributable PM2.5",
    
    y =
      "Accumulated Local Effect"
  ) +
  
  theme_minimal()


print(
  ale.plot
)


# ------------------------------------------------------------
# 29. Save ALE plot
# ------------------------------------------------------------

ggsave(
  
  filename =
    "results/Moose_Daily_Count_PM25_ALE.png",
  
  plot =
    ale.plot,
  
  width =
    8,
  
  height =
    6,
  
  dpi =
    300
)


# ============================================================
# OBSERVED COUNT PLOT
# ============================================================


# ------------------------------------------------------------
# 30. Plot raw observed counts
# ------------------------------------------------------------

observed.plot <- ggplot(
  
  observed.summary,
  
  aes(
    x =
      Mean_PM25,
    
    y =
      Mean_Daily_Count
  )
  
) +
  
  geom_line(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  labs(
    
    title =
      "Observed Moose Daily Counts by PM2.5",
    
    subtitle =
      "Raw mean daily detections within PM2.5 bins",
    
    x =
      "Mean Standardized Wildfire-Attributable PM2.5",
    
    y =
      "Observed Mean Daily Count"
  ) +
  
  theme_minimal()


print(
  observed.plot
)


# ------------------------------------------------------------
# 31. Save observed-count plot
# ------------------------------------------------------------

ggsave(
  
  filename =
    "results/Moose_Observed_Daily_Counts_By_PM25.png",
  
  plot =
    observed.plot,
  
  width =
    8,
  
  height =
    6,
  
  dpi =
    300
)


# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "FINAL MOOSE ALE SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Trees: 500\n"
)

cat(
  "mtry: 3\n"
)

cat(
  "Minimum node size: 20\n"
)

cat(
  "Split rule: Poisson\n"
)

cat(
  "\nPM2.5 range:",
  round(
    smoke.lower,
    4
  ),
  "to",
  round(
    smoke.upper,
    4
  ),
  "\n"
)

cat(
  "Overall ALE change:",
  round(
    overall.ale.change,
    6
  ),
  "\n"
)

cat(
  "Largest local effect:",
  round(
    largest.local.effect,
    6
  ),
  "\n"
)

cat(
  "====================================================\n"
)