# ============================================================
# 33_coyote_daily_count_pm25_permutation.R
#
# COYOTE DAILY-COUNT PM2.5 PERMUTATION TEST
#
# PURPOSE:
#
# Test whether the PM2.5 response learned by the Random Forest
# is stronger than responses produced when PM2.5 is randomly
# reassigned among camera-days.
#
#
# TWO RESPONSE STATISTICS ARE TESTED:
#
# 1. Overall ALE change
#
#    Change in ALE from the low end to the high end of the
#    central PM2.5 range.
#
#
# 2. Upper-tail ALE change
#
#    Accumulated PM2.5 effect from the 90th through 99th
#    percentile of observed smoke.
#
#
# The same model structure, ALE boundaries, and settings are
# used for the observed and randomized models.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)

library(ggplot2)



# ------------------------------------------------------------
# 2. Set number of permutations
# ------------------------------------------------------------

number.runs <- 30



# ------------------------------------------------------------
# 3. Load cleaned coyote dataset
# ------------------------------------------------------------

coyote <- read.csv(
  "data-split/coyote_model_data.csv"
)



# ------------------------------------------------------------
# 4. Restore categorical variables
# ------------------------------------------------------------

coyote$Road_Type <- as.factor(
  coyote$Road_Type
)

coyote$Habitat_Type <- as.factor(
  coyote$Habitat_Type
)

coyote$Land_Mgnt <- as.factor(
  coyote$Land_Mgnt
)

coyote$Cell_ID <- as.factor(
  coyote$Cell_ID
)



# ============================================================
# SELECT MODEL VARIABLES
# ============================================================



# ------------------------------------------------------------
# 5. Keep variables used in the daily-count model
# ------------------------------------------------------------

coyote.rf.data <- coyote[
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
# 6. Remove incomplete observations
# ------------------------------------------------------------

coyote.rf.data <- coyote.rf.data[
  complete.cases(
    coyote.rf.data
  ),
]



# ============================================================
# DEFINE FIXED PM2.5 RANGE AND ALE BOUNDARIES
# ============================================================



# ------------------------------------------------------------
# 7. Calculate central PM2.5 range
# ------------------------------------------------------------

smoke.lower <- as.numeric(
  quantile(
    coyote.rf.data$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)


smoke.upper <- as.numeric(
  quantile(
    coyote.rf.data$scale.smoke,
    probs = 0.99,
    na.rm = TRUE
  )
)



# ------------------------------------------------------------
# 8. Define upper-tail threshold
# ------------------------------------------------------------

high.smoke.threshold <- as.numeric(
  quantile(
    coyote.rf.data$scale.smoke,
    probs = 0.90,
    na.rm = TRUE
  )
)



cat(
  "\n====================================================\n"
)

cat(
  "PM2.5 ANALYSIS RANGE\n"
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
  "90th percentile:",
  high.smoke.threshold,
  "\n"
)


cat(
  "99th percentile:",
  smoke.upper,
  "\n"
)



# ------------------------------------------------------------
# 9. Restrict original data to central PM2.5 range
# ------------------------------------------------------------

central.data <- coyote.rf.data[
  coyote.rf.data$scale.smoke >= smoke.lower &
    coyote.rf.data$scale.smoke <= smoke.upper,
]



# ------------------------------------------------------------
# 10. Define refined quantile probabilities
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
# 11. Create fixed ALE boundaries
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
  "ALE intervals:",
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
# RESPONSE-STATISTIC FUNCTION
# ============================================================



# ------------------------------------------------------------
# 13. Calculate ALE summary statistics
# ------------------------------------------------------------

calculate_ale_statistics <- function(
    ale.table,
    high.threshold
) {
  
  overall.change <-
    
    ale.table$ALE[
      nrow(
        ale.table
      )
    ] -
    
    ale.table$ALE[
      1
    ]
  
  
  upper.rows <- which(
    ale.table$Lower_PM25 >=
      high.threshold
  )
  
  
  if (
    length(
      upper.rows
    ) == 0
  ) {
    
    upper.change <- NA
    
  } else {
    
    upper.change <- sum(
      ale.table$Local_Effect[
        upper.rows
      ],
      na.rm = TRUE
    )
  }
  
  
  return(
    c(
      Overall_Change =
        overall.change,
      
      Upper_Tail_Change =
        upper.change
    )
  )
}



# ============================================================
# OBSERVED RANDOM FOREST
# ============================================================



# ------------------------------------------------------------
# 14. Fit observed model
# ------------------------------------------------------------

set.seed(42)


observed.rf <- ranger(
  
  Daily_Count ~ .,
  
  data =
    coyote.rf.data,
  
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
# 15. Calculate observed ALE
# ------------------------------------------------------------

observed.ale <- calculate_regression_ale(
  
  model =
    observed.rf,
  
  data =
    central.data,
  
  breaks =
    ale.breaks
)



# ------------------------------------------------------------
# 16. Calculate observed statistics
# ------------------------------------------------------------

observed.statistics <- calculate_ale_statistics(
  
  ale.table =
    observed.ale,
  
  high.threshold =
    high.smoke.threshold
)



observed.overall <-
  observed.statistics[
    "Overall_Change"
  ]


observed.upper <-
  observed.statistics[
    "Upper_Tail_Change"
  ]



cat(
  "\n====================================================\n"
)

cat(
  "OBSERVED PM2.5 RESPONSE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Overall ALE change:",
  round(
    observed.overall,
    6
  ),
  "\n"
)


cat(
  "Upper-tail ALE change:",
  round(
    observed.upper,
    6
  ),
  "\n"
)



# ============================================================
# PERMUTATION TEST
# ============================================================



# ------------------------------------------------------------
# 17. Create result vectors
# ------------------------------------------------------------

null.overall <- numeric(
  number.runs
)


null.upper <- numeric(
  number.runs
)



# ------------------------------------------------------------
# 18. Run PM2.5 permutations
# ------------------------------------------------------------

for (
  i in 1:number.runs
) {
  
  set.seed(
    1000 + i
  )
  
  
  shuffled.data <- coyote.rf.data
  
  
  shuffled.data$scale.smoke <- sample(
    shuffled.data$scale.smoke,
    replace = FALSE
  )
  
  
  shuffled.central <- shuffled.data[
    shuffled.data$scale.smoke >= smoke.lower &
      shuffled.data$scale.smoke <= smoke.upper,
  ]
  
  
  shuffled.rf <- ranger(
    
    Daily_Count ~ .,
    
    data =
      shuffled.data,
    
    num.trees =
      500,
    
    min.node.size =
      20,
    
    splitrule =
      "poisson",
    
    poisson.tau =
      1,
    
    importance =
      "none",
    
    respect.unordered.factors =
      "order",
    
    seed =
      2000 + i
  )
  
  
  shuffled.ale <- calculate_regression_ale(
    
    model =
      shuffled.rf,
    
    data =
      shuffled.central,
    
    breaks =
      ale.breaks
  )
  
  
  shuffled.statistics <-
    calculate_ale_statistics(
      
      ale.table =
        shuffled.ale,
      
      high.threshold =
        high.smoke.threshold
    )
  
  
  null.overall[i] <-
    shuffled.statistics[
      "Overall_Change"
    ]
  
  
  null.upper[i] <-
    shuffled.statistics[
      "Upper_Tail_Change"
    ]
  
  
  cat(
    "Completed permutation",
    i,
    "of",
    number.runs,
    "\n"
  )
}



# ============================================================
# NULL DISTRIBUTION SUMMARIES
# ============================================================



# ------------------------------------------------------------
# 19. Overall null summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "OVERALL ALE NULL DISTRIBUTION\n"
)

cat(
  "====================================================\n"
)


print(
  summary(
    null.overall
  )
)



# ------------------------------------------------------------
# 20. Upper-tail null summary
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "UPPER-TAIL ALE NULL DISTRIBUTION\n"
)

cat(
  "====================================================\n"
)


print(
  summary(
    null.upper
  )
)



# ============================================================
# EMPIRICAL PERMUTATION P-VALUES
# ============================================================



# ------------------------------------------------------------
# 21. Overall lower-tail probability
# ------------------------------------------------------------

overall.lower.p <- (
  
  sum(
    null.overall <=
      observed.overall,
    na.rm = TRUE
  ) + 1
  
) / (
  number.runs + 1
)



# ------------------------------------------------------------
# 22. Overall upper-tail probability
# ------------------------------------------------------------

overall.upper.p <- (
  
  sum(
    null.overall >=
      observed.overall,
    na.rm = TRUE
  ) + 1
  
) / (
  number.runs + 1
)



# ------------------------------------------------------------
# 23. Overall two-sided p-value
# ------------------------------------------------------------

overall.two.sided.p <- min(
  1,
  2 *
    min(
      overall.lower.p,
      overall.upper.p
    )
)



# ------------------------------------------------------------
# 24. Upper-tail lower-tail probability
# ------------------------------------------------------------

high.lower.p <- (
  
  sum(
    null.upper <=
      observed.upper,
    na.rm = TRUE
  ) + 1
  
) / (
  number.runs + 1
)



# ------------------------------------------------------------
# 25. Upper-tail upper-tail probability
# ------------------------------------------------------------

high.upper.p <- (
  
  sum(
    null.upper >=
      observed.upper,
    na.rm = TRUE
  ) + 1
  
) / (
  number.runs + 1
)



# ------------------------------------------------------------
# 26. Upper-tail two-sided p-value
# ------------------------------------------------------------

high.two.sided.p <- min(
  1,
  2 *
    min(
      high.lower.p,
      high.upper.p
    )
)



# ============================================================
# PERCENTILE POSITION WITHIN NULL DISTRIBUTION
# ============================================================



# ------------------------------------------------------------
# 27. Overall null percentile
# ------------------------------------------------------------

overall.null.percentile <- mean(
  null.overall <=
    observed.overall,
  na.rm = TRUE
) * 100



# ------------------------------------------------------------
# 28. Upper-tail null percentile
# ------------------------------------------------------------

high.null.percentile <- mean(
  null.upper <=
    observed.upper,
  na.rm = TRUE
) * 100



# ============================================================
# PRINT FINAL TEST RESULTS
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COYOTE PM2.5 PERMUTATION RESULTS\n"
)

cat(
  "====================================================\n"
)


cat(
  "\n--- OVERALL PM2.5 RESPONSE ---\n"
)


cat(
  "Observed ALE change:",
  round(
    observed.overall,
    6
  ),
  "\n"
)


cat(
  "Null mean:",
  round(
    mean(
      null.overall,
      na.rm = TRUE
    ),
    6
  ),
  "\n"
)


cat(
  "Null median:",
  round(
    median(
      null.overall,
      na.rm = TRUE
    ),
    6
  ),
  "\n"
)


cat(
  "Observed null percentile:",
  round(
    overall.null.percentile,
    2
  ),
  "%\n"
)


cat(
  "Two-sided permutation p-value:",
  round(
    overall.two.sided.p,
    4
  ),
  "\n"
)



cat(
  "\n--- UPPER-TAIL PM2.5 RESPONSE ---\n"
)


cat(
  "Upper-tail range:",
  round(
    high.smoke.threshold,
    6
  ),
  "to",
  round(
    smoke.upper,
    6
  ),
  "\n"
)


cat(
  "Observed upper-tail ALE change:",
  round(
    observed.upper,
    6
  ),
  "\n"
)


cat(
  "Null mean:",
  round(
    mean(
      null.upper,
      na.rm = TRUE
    ),
    6
  ),
  "\n"
)


cat(
  "Null median:",
  round(
    median(
      null.upper,
      na.rm = TRUE
    ),
    6
  ),
  "\n"
)


cat(
  "Observed null percentile:",
  round(
    high.null.percentile,
    2
  ),
  "%\n"
)


cat(
  "Two-sided permutation p-value:",
  round(
    high.two.sided.p,
    4
  ),
  "\n"
)



# ============================================================
# SAVE RESULTS
# ============================================================



# ------------------------------------------------------------
# 29. Create results directory
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)



# ------------------------------------------------------------
# 30. Save permutation runs
# ------------------------------------------------------------

permutation.results <- data.frame(
  
  Run =
    1:number.runs,
  
  Overall_ALE_Change =
    null.overall,
  
  Upper_Tail_ALE_Change =
    null.upper
)



write.csv(
  
  permutation.results,
  
  "results/Coyote_PM25_Daily_Count_Permutation_Runs.csv",
  
  row.names =
    FALSE
)



# ------------------------------------------------------------
# 31. Create summary table
# ------------------------------------------------------------

permutation.summary <- data.frame(
  
  Species =
    "Coyote",
  
  Permutations =
    number.runs,
  
  Observed_Overall_ALE =
    observed.overall,
  
  Overall_Null_Mean =
    mean(
      null.overall,
      na.rm = TRUE
    ),
  
  Overall_Null_Median =
    median(
      null.overall,
      na.rm = TRUE
    ),
  
  Overall_Null_Percentile =
    overall.null.percentile,
  
  Overall_Two_Sided_P =
    overall.two.sided.p,
  
  High_Smoke_Threshold =
    high.smoke.threshold,
  
  Observed_Upper_Tail_ALE =
    observed.upper,
  
  Upper_Tail_Null_Mean =
    mean(
      null.upper,
      na.rm = TRUE
    ),
  
  Upper_Tail_Null_Median =
    median(
      null.upper,
      na.rm = TRUE
    ),
  
  Upper_Tail_Null_Percentile =
    high.null.percentile,
  
  Upper_Tail_Two_Sided_P =
    high.two.sided.p
)



# ------------------------------------------------------------
# 32. Save summary
# ------------------------------------------------------------

write.csv(
  
  permutation.summary,
  
  "results/Coyote_PM25_Daily_Count_Permutation_Summary.csv",
  
  row.names =
    FALSE
)



# ============================================================
# PLOT 1:
# OVERALL ALE NULL DISTRIBUTION
# ============================================================

overall.plot.data <- data.frame(
  ALE_Change =
    null.overall
)



overall.null.plot <- ggplot(
  
  overall.plot.data,
  
  aes(
    x =
      ALE_Change
  )
  
) +
  
  geom_histogram(
    bins =
      20
  ) +
  
  geom_vline(
    xintercept =
      observed.overall,
    linewidth =
      1
  ) +
  
  labs(
    
    title =
      "Coyote Overall PM2.5 ALE Permutation Test",
    
    subtitle =
      paste(
        number.runs,
        "Random Forest models after randomizing PM2.5"
      ),
    
    x =
      "Overall ALE Change",
    
    y =
      "Number of Permutations"
  ) +
  
  theme_minimal()



print(
  overall.null.plot
)



ggsave(
  
  filename =
    "results/Coyote_PM25_Overall_ALE_Permutation.png",
  
  plot =
    overall.null.plot,
  
  width =
    8,
  
  height =
    6,
  
  dpi =
    300
)



# ============================================================
# PLOT 2:
# UPPER-TAIL ALE NULL DISTRIBUTION
# ============================================================

upper.plot.data <- data.frame(
  ALE_Change =
    null.upper
)



upper.null.plot <- ggplot(
  
  upper.plot.data,
  
  aes(
    x =
      ALE_Change
  )
  
) +
  
  geom_histogram(
    bins =
      20
  ) +
  
  geom_vline(
    xintercept =
      observed.upper,
    linewidth =
      1
  ) +
  
  labs(
    
    title =
      "Coyote High-Smoke PM2.5 ALE Permutation Test",
    
    subtitle =
      paste(
        "Upper 10% of PM2.5 exposure -",
        number.runs,
        "PM2.5 permutations"
      ),
    
    x =
      "Upper-Tail ALE Change",
    
    y =
      "Number of Permutations"
  ) +
  
  theme_minimal()



print(
  upper.null.plot
)



ggsave(
  
  filename =
    "results/Coyote_PM25_Upper_Tail_ALE_Permutation.png",
  
  plot =
    upper.null.plot,
  
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
  "FINAL COYOTE PM2.5 PERMUTATION SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Permutations:",
  number.runs,
  "\n"
)


cat(
  "Observed overall ALE:",
  round(
    observed.overall,
    6
  ),
  "\n"
)


cat(
  "Overall two-sided p-value:",
  round(
    overall.two.sided.p,
    4
  ),
  "\n"
)


cat(
  "Observed upper-tail ALE:",
  round(
    observed.upper,
    6
  ),
  "\n"
)


cat(
  "Upper-tail two-sided p-value:",
  round(
    high.two.sided.p,
    4
  ),
  "\n"
)


cat(
  "====================================================\n"
)