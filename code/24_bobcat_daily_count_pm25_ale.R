# ============================================================
# 24_bobcat_daily_count_pm25_ale.R
#
# BOBCAT DAILY-COUNT PM2.5 ALE ANALYSIS
#
# PURPOSE:
#
# Examine how predicted daily bobcat detection counts change
# across wildfire-attributable PM2.5 levels.
#
# The Random Forest response is Daily_Count, so all camera-days
# are represented, including zero-detection days.
#
# The full-data interpretation model includes camera site as a
# categorical predictor to represent persistent differences
# among camera locations.
#
# PM2.5 response is evaluated using Accumulated Local Effects
# (ALE).
#
# Observed daily-count summaries are also calculated across the
# same PM2.5 intervals for descriptive comparison.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Load cleaned bobcat dataset
# ------------------------------------------------------------

bobcat <- read.csv(
  "data-split/bobcat_model_data.csv"
)



# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

bobcat$Road_Type <- as.factor(
  bobcat$Road_Type
)

bobcat$Habitat_Type <- as.factor(
  bobcat$Habitat_Type
)

bobcat$Land_Mgnt <- as.factor(
  bobcat$Land_Mgnt
)

bobcat$Cell_ID <- as.factor(
  bobcat$Cell_ID
)



# ============================================================
# SELECT MODEL VARIABLES
# ============================================================



# ------------------------------------------------------------
# 4. Keep variables used in the daily-count model
# ------------------------------------------------------------

bobcat.rf.data <- bobcat[
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

bobcat.rf.data <- bobcat.rf.data[
  complete.cases(
    bobcat.rf.data
  ),
]



# ============================================================
# FIT FULL DAILY-COUNT RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 6. Fit full-data Random Forest
# ------------------------------------------------------------

bobcat.daily.rf.full <- ranger(
  
  Daily_Count ~ .,
  
  data =
    bobcat.rf.data,
  
  num.trees =
    500,
  
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



# ------------------------------------------------------------
# 7. Print model summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL BOBCAT DAILY-COUNT RANDOM FOREST\n"
)

cat(
  "====================================================\n"
)


print(
  bobcat.daily.rf.full
)



# ============================================================
# DEFINE PM2.5 INTERPRETATION RANGE
# ============================================================



# ------------------------------------------------------------
# 8. Calculate PM2.5 limits
# ------------------------------------------------------------

smoke.lower <- as.numeric(
  quantile(
    bobcat.rf.data$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)


smoke.upper <- as.numeric(
  quantile(
    bobcat.rf.data$scale.smoke,
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
# 9. Restrict data to central PM2.5 range
# ------------------------------------------------------------

central.data <- bobcat.rf.data[
  bobcat.rf.data$scale.smoke >= smoke.lower &
    bobcat.rf.data$scale.smoke <= smoke.upper,
]



cat(
  "\nCentral observations:",
  nrow(
    central.data
  ),
  "\n"
)


cat(
  "Excluded observations:",
  nrow(
    bobcat.rf.data
  ) -
    nrow(
      central.data
    ),
  "\n"
)



# ============================================================
# CREATE REFINED ALE BOUNDARIES
# ============================================================



# ------------------------------------------------------------
# 10. Define quantile probabilities
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



# ------------------------------------------------------------
# 11. Create unique PM2.5 boundaries
# ------------------------------------------------------------

ale.breaks <- as.numeric(
  quantile(
    central.data$scale.smoke,
    probs =
      ale.probabilities,
    na.rm =
      TRUE
  )
)


ale.breaks <- unique(
  ale.breaks
)



cat(
  "Number of ALE boundaries:",
  length(
    ale.breaks
  ),
  "\n"
)


cat(
  "Number of ALE intervals:",
  length(
    ale.breaks
  ) - 1,
  "\n"
)



# ============================================================
# ALE FUNCTION
# ============================================================



# ------------------------------------------------------------
# 12. Define regression ALE function
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
    breaks =
      breaks,
    include.lowest =
      TRUE,
    labels =
      FALSE
  )
  
  
  number.intervals <-
    length(
      breaks
    ) - 1
  
  
  local.effects <- numeric(
    number.intervals
  )
  
  
  observations.per.bin <- integer(
    number.intervals
  )
  
  
  for (
    b in 1:number.intervals
  ) {
    
    rows.in.bin <- which(
      bin.id == b
    )
    
    
    observations.per.bin[b] <-
      length(
        rows.in.bin
      )
    
    
    if (
      length(
        rows.in.bin
      ) == 0
    ) {
      
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
      data =
        lower.data
    )$predictions
    
    
    upper.prediction <- predict(
      model,
      data =
        upper.data
    )$predictions
    
    
    local.effects[b] <- mean(
      upper.prediction -
        lower.prediction
    )
  }
  
  
  valid <- !is.na(
    local.effects
  )
  
  
  effects.valid <-
    local.effects[
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
    w =
      observations.valid
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
# CALCULATE PM2.5 ALE
# ============================================================



# ------------------------------------------------------------
# 13. Calculate ALE
# ------------------------------------------------------------

bobcat.ale <- calculate_regression_ale(
  
  model =
    bobcat.daily.rf.full,
  
  data =
    central.data,
  
  breaks =
    ale.breaks
)



# ------------------------------------------------------------
# 14. Print ALE table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "BOBCAT DAILY-COUNT PM2.5 ALE\n"
)

cat(
  "====================================================\n"
)


print(
  bobcat.ale
)



# ============================================================
# ALE DIRECTION
# ============================================================



# ------------------------------------------------------------
# 15. Calculate low-to-high ALE change
# ------------------------------------------------------------

ale.low.to.high <-
  
  bobcat.ale$ALE[
    nrow(
      bobcat.ale
    )
  ] -
  
  bobcat.ale$ALE[
    1
  ]



cat(
  "\nALE low-to-high change:",
  round(
    ale.low.to.high,
    6
  ),
  "\n"
)



# ------------------------------------------------------------
# 16. Determine PM2.5 response direction
# ------------------------------------------------------------

if (
  ale.low.to.high < 0
) {
  
  ale.direction <-
    "NEGATIVE"
  
} else if (
  ale.low.to.high > 0
) {
  
  ale.direction <-
    "POSITIVE"
  
} else {
  
  ale.direction <-
    "FLAT"
}



cat(
  "ALE direction:",
  ale.direction,
  "\n"
)



# ============================================================
# CHECK WHETHER ONE INTERVAL DOMINATES
# ============================================================



# ------------------------------------------------------------
# 17. Largest absolute local effect
# ------------------------------------------------------------

largest.local.effect <- max(
  abs(
    bobcat.ale$Local_Effect
  ),
  na.rm =
    TRUE
)



# ------------------------------------------------------------
# 18. Total absolute ALE movement
# ------------------------------------------------------------

total.absolute.movement <- sum(
  abs(
    bobcat.ale$Local_Effect
  ),
  na.rm =
    TRUE
)



# ------------------------------------------------------------
# 19. Largest interval contribution
# ------------------------------------------------------------

largest.interval.percent <- (
  
  largest.local.effect /
    total.absolute.movement
  
) * 100



cat(
  "Largest local effect:",
  round(
    largest.local.effect,
    6
  ),
  "\n"
)


cat(
  "Largest interval contribution:",
  round(
    largest.interval.percent,
    2
  ),
  "%",
  "\n"
)



# ============================================================
# RAW OBSERVED DAILY COUNTS BY PM2.5
# ============================================================



# ------------------------------------------------------------
# 20. Assign observations to PM2.5 intervals
# ------------------------------------------------------------

central.data$Smoke_Bin <- cut(
  
  central.data$scale.smoke,
  
  breaks =
    ale.breaks,
  
  include.lowest =
    TRUE,
  
  labels =
    FALSE
)



# ------------------------------------------------------------
# 21. Calculate observed daily-count summaries
# ------------------------------------------------------------

observed.counts <- do.call(
  
  rbind,
  
  lapply(
    
    sort(
      unique(
        central.data$Smoke_Bin[
          !is.na(
            central.data$Smoke_Bin
          )
        ]
      )
    ),
    
    function(
    b
    ) {
      
      rows <- which(
        central.data$Smoke_Bin ==
          b
      )
      
      
      temp <- central.data[
        rows,
        ,
        drop = FALSE
      ]
      
      
      data.frame(
        
        Bin =
          b,
        
        Mean_PM25 =
          mean(
            temp$scale.smoke,
            na.rm = TRUE
          ),
        
        Observations =
          nrow(
            temp
          ),
        
        Mean_Daily_Count =
          mean(
            temp$Daily_Count,
            na.rm = TRUE
          ),
        
        Total_Detections =
          sum(
            temp$Daily_Count,
            na.rm = TRUE
          ),
        
        Positive_Detection_Rate =
          mean(
            temp$Daily_Count > 0,
            na.rm = TRUE
          )
      )
    }
  )
)



# ------------------------------------------------------------
# 22. Print observed count table
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED BOBCAT DAILY COUNTS BY PM2.5\n"
)

cat(
  "====================================================\n"
)


print(
  observed.counts
)



# ============================================================
# SAVE RESULTS
# ============================================================



# ------------------------------------------------------------
# 23. Create results directory
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)



# ------------------------------------------------------------
# 24. Save ALE table
# ------------------------------------------------------------

write.csv(
  
  bobcat.ale,
  
  "results/Bobcat_Daily_Count_PM25_ALE.csv",
  
  row.names =
    FALSE
)



# ------------------------------------------------------------
# 25. Save observed daily-count summaries
# ------------------------------------------------------------

write.csv(
  
  observed.counts,
  
  "results/Bobcat_Observed_Daily_Counts_By_PM25.csv",
  
  row.names =
    FALSE
)



# ------------------------------------------------------------
# 26. Create summary table
# ------------------------------------------------------------

ale.summary <- data.frame(
  
  Species =
    "Bobcat",
  
  ALE_Low_to_High_Change =
    ale.low.to.high,
  
  ALE_Direction =
    ale.direction,
  
  Largest_Local_Effect =
    largest.local.effect,
  
  Largest_Interval_Percent =
    largest.interval.percent,
  
  Central_Observations =
    nrow(
      central.data
    ),
  
  PM25_1st_Percentile =
    smoke.lower,
  
  PM25_99th_Percentile =
    smoke.upper
)



# ------------------------------------------------------------
# 27. Save summary table
# ------------------------------------------------------------

write.csv(
  
  ale.summary,
  
  "results/Bobcat_Daily_Count_PM25_ALE_Summary.csv",
  
  row.names =
    FALSE
)



# ============================================================
# PLOT 1:
# RANDOM FOREST ALE
# ============================================================

ale.plot <- ggplot(
  
  bobcat.ale,
  
  aes(
    x =
      Upper_PM25,
    y =
      ALE
  )
  
) +
  
  geom_hline(
    yintercept =
      0,
    linetype =
      "dashed"
  ) +
  
  geom_step(
    linewidth =
      1
  ) +
  
  geom_point(
    size =
      2
  ) +
  
  labs(
    
    title =
      "Bobcat Daily-Count Response to Wildfire PM2.5",
    
    subtitle =
      "Accumulated Local Effects",
    
    x =
      "Wildfire-attributable PM2.5 (scaled)",
    
    y =
      "ALE Effect on Predicted Daily Count"
  ) +
  
  theme_minimal()



print(
  ale.plot
)



ggsave(
  
  filename =
    "results/Bobcat_Daily_Count_PM25_ALE.png",
  
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
# PLOT 2:
# OBSERVED MEAN DAILY COUNT
# ============================================================

observed.plot <- ggplot(
  
  observed.counts,
  
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
      "Observed Bobcat Daily Counts by Wildfire PM2.5",
    
    subtitle =
      "Mean daily detections within PM2.5 intervals",
    
    x =
      "Mean Wildfire-attributable PM2.5 (scaled)",
    
    y =
      "Observed Mean Daily Count"
  ) +
  
  theme_minimal()



print(
  observed.plot
)



ggsave(
  
  filename =
    "results/Bobcat_Observed_Daily_Counts_By_PM25.png",
  
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
  "BOBCAT DAILY-COUNT PM2.5 ALE SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Central observations:",
  nrow(
    central.data
  ),
  "\n"
)


cat(
  "PM2.5 1st percentile:",
  round(
    smoke.lower,
    6
  ),
  "\n"
)


cat(
  "PM2.5 99th percentile:",
  round(
    smoke.upper,
    6
  ),
  "\n"
)


cat(
  "ALE low-to-high change:",
  round(
    ale.low.to.high,
    6
  ),
  "\n"
)


cat(
  "ALE direction:",
  ale.direction,
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
  "Largest interval contribution:",
  round(
    largest.interval.percent,
    2
  ),
  "%",
  "\n"
)


cat(
  "====================================================\n"
)