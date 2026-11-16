# ============================================================
# 19_moose_pm25_daily_count_permutation.R
#
# MOOSE PM2.5 PERMUTATION TEST
#
# PURPOSE:
#
# Test whether the PM2.5 relationship found by the Random
# Forest is stronger than relationships that can appear when
# PM2.5 is randomly rearranged.
#
# Final Random Forest settings:
#
#   Trees = 500
#   mtry = 3
#   Minimum node size = 20
#   Split rule = Poisson
#
# The observed PM2.5 relationship is measured using ALE.
#
# We then randomly shuffle PM2.5 values, refit the model,
# calculate a new ALE response, and compare the randomized
# results with the observed result.
#
# ============================================================


# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(ranger)


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
# 4. Keep finalized predictor set
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
# DEFINE PM2.5 RANGE AND ALE BREAKS
# ============================================================


# ------------------------------------------------------------
# 6. Use 1st through 99th percentile PM2.5 range
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


# ------------------------------------------------------------
# 7. Define quantile probabilities
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
# 8. Convert probabilities to actual PM2.5 values
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


smoke.breaks <- unique(
  smoke.breaks
)


# ============================================================
# ALE FUNCTION
# ============================================================


# ------------------------------------------------------------
# 9. Function to calculate PM2.5 ALE
#
# This function:
#
#   1. Fits a Random Forest
#   2. Calculates ALE across the fixed PM2.5 intervals
#   3. Returns the ALE curve and summary statistics
# ------------------------------------------------------------

calculate_pm25_ale <- function(
    model.data,
    seed.value
) {
  
  # ----------------------------------------------------------
  # Fit Random Forest
  # ----------------------------------------------------------
  
  current.model <- ranger(
    
    Daily_Count ~ .,
    
    data =
      model.data,
    
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
      "none",
    
    respect.unordered.factors =
      "order",
    
    seed =
      seed.value
  )
  
  
  # ----------------------------------------------------------
  # Keep central PM2.5 range
  # ----------------------------------------------------------
  
  current.ale.data <- model.data[
    model.data$scale.smoke >=
      smoke.lower &
      model.data$scale.smoke <=
      smoke.upper,
  ]
  
  
  # ----------------------------------------------------------
  # Empty storage
  # ----------------------------------------------------------
  
  current.intervals <- data.frame(
    
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
  
  
  # ----------------------------------------------------------
  # Calculate local effects
  # ----------------------------------------------------------
  
  for (
    i in 1:(
      length(
        smoke.breaks
      ) - 1
    )
  ) {
    
    lower.value <-
      smoke.breaks[i]
    
    upper.value <-
      smoke.breaks[i + 1]
    
    
    if (
      i <
      length(
        smoke.breaks
      ) - 1
    ) {
      
      interval.rows <- which(
        
        current.ale.data$scale.smoke >=
          lower.value &
          
          current.ale.data$scale.smoke <
          upper.value
      )
      
    } else {
      
      interval.rows <- which(
        
        current.ale.data$scale.smoke >=
          lower.value &
          
          current.ale.data$scale.smoke <=
          upper.value
      )
    }
    
    
    if (
      length(
        interval.rows
      ) == 0
    ) {
      
      next
    }
    
    
    interval.data <- current.ale.data[
      interval.rows,
    ]
    
    
    lower.data <-
      interval.data
    
    upper.data <-
      interval.data
    
    
    lower.data$scale.smoke <-
      lower.value
    
    upper.data$scale.smoke <-
      upper.value
    
    
    lower.pred <- predict(
      
      current.model,
      
      data =
        lower.data
      
    )$predictions
    
    
    upper.pred <- predict(
      
      current.model,
      
      data =
        upper.data
      
    )$predictions
    
    
    local.effect <- mean(
      upper.pred -
        lower.pred
    )
    
    
    current.intervals <- rbind(
      
      current.intervals,
      
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
  
  
  # ----------------------------------------------------------
  # Accumulate local effects
  # ----------------------------------------------------------
  
  current.intervals$ALE_Raw <- cumsum(
    current.intervals$Local_Effect
  )
  
  
  # ----------------------------------------------------------
  # Center ALE around zero
  # ----------------------------------------------------------
  
  current.weights <- current.intervals$N /
    sum(
      current.intervals$N
    )
  
  
  current.center <- sum(
    current.intervals$ALE_Raw *
      current.weights
  )
  
  
  current.intervals$ALE <- (
    current.intervals$ALE_Raw -
      current.center
  )
  
  
  # ----------------------------------------------------------
  # Overall low-to-high ALE change
  # ----------------------------------------------------------
  
  overall.change <- (
    tail(
      current.intervals$ALE,
      1
    ) -
      head(
        current.intervals$ALE,
        1
      )
  )
  
  
  # ----------------------------------------------------------
  # Upper-smoke ALE change
  #
  # Compare the ALE near the 90th percentile with the ALE
  # near the 99th percentile.
  # ----------------------------------------------------------
  
  upper.start <- as.numeric(
    quantile(
      model.data$scale.smoke,
      probs =
        0.90,
      na.rm =
        TRUE
    )
  )
  
  
  upper.rows <- which(
    current.intervals$Midpoint >=
      upper.start
  )
  
  
  if (
    length(
      upper.rows
    ) >= 2
  ) {
    
    upper.change <- (
      tail(
        current.intervals$ALE[
          upper.rows
        ],
        1
      ) -
        head(
          current.intervals$ALE[
            upper.rows
          ],
          1
        )
    )
    
  } else {
    
    upper.change <- NA
  }
  
  
  return(
    list(
      
      intervals =
        current.intervals,
      
      overall.change =
        overall.change,
      
      upper.change =
        upper.change
    )
  )
}


# ============================================================
# OBSERVED MODEL
# ============================================================


# ------------------------------------------------------------
# 10. Calculate observed PM2.5 ALE
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "CALCULATING OBSERVED MOOSE PM2.5 RESPONSE\n"
)

cat(
  "====================================================\n"
)


observed.result <- calculate_pm25_ale(
  
  model.data =
    moose.model.data,
  
  seed.value =
    42
)


observed.overall <-
  observed.result$overall.change


observed.upper <-
  observed.result$upper.change


cat(
  "Observed overall ALE change:",
  round(
    observed.overall,
    6
  ),
  "\n"
)


cat(
  "Observed upper-smoke ALE change:",
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
# 11. Number of permutations
#
# 30 permutations are used here for the project iteration.
#
# A larger number, such as 99 or more, can be used for a final
# analysis if additional computation time is available.
# ------------------------------------------------------------

n.permutations <- 30


# ------------------------------------------------------------
# 12. Create storage
# ------------------------------------------------------------

permutation.results <- data.frame(
  
  Permutation =
    1:n.permutations,
  
  Overall_ALE_Change =
    NA_real_,
  
  Upper_ALE_Change =
    NA_real_
)


# ------------------------------------------------------------
# 13. Run randomized PM2.5 models
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "RUNNING PM2.5 PERMUTATIONS\n"
)

cat(
  "====================================================\n"
)


for (
  p in 1:n.permutations
) {
  
  cat(
    "Permutation",
    p,
    "of",
    n.permutations,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Copy original data
  # ----------------------------------------------------------
  
  permuted.data <-
    moose.model.data
  
  
  # ----------------------------------------------------------
  # Randomize PM2.5
  #
  # Daily_Count and every other predictor remain unchanged.
  # ----------------------------------------------------------
  
  set.seed(
    1000 + p
  )
  
  
  permuted.data$scale.smoke <- sample(
    
    permuted.data$scale.smoke,
    
    size =
      nrow(
        permuted.data
      ),
    
    replace =
      FALSE
  )
  
  
  # ----------------------------------------------------------
  # Calculate ALE using randomized PM2.5
  # ----------------------------------------------------------
  
  permuted.result <- calculate_pm25_ale(
    
    model.data =
      permuted.data,
    
    seed.value =
      2000 + p
  )
  
  
  # ----------------------------------------------------------
  # Save result
  # ----------------------------------------------------------
  
  permutation.results$Overall_ALE_Change[p] <-
    permuted.result$overall.change
  
  
  permutation.results$Upper_ALE_Change[p] <-
    permuted.result$upper.change
}


# ============================================================
# CALCULATE PERMUTATION P-VALUES
# ============================================================


# ------------------------------------------------------------
# 14. Overall ALE permutation p-value
#
# Absolute values are used because an unusually strong
# positive OR negative PM2.5 response is of interest.
# ------------------------------------------------------------

overall.p.value <- (
  
  1 +
    sum(
      abs(
        permutation.results$Overall_ALE_Change
      ) >=
        abs(
          observed.overall
        )
    )
  
) / (
  
  n.permutations +
    1
)


# ------------------------------------------------------------
# 15. Upper-smoke permutation p-value
# ------------------------------------------------------------

valid.upper <- !is.na(
  permutation.results$Upper_ALE_Change
)


upper.p.value <- (
  
  1 +
    sum(
      abs(
        permutation.results$Upper_ALE_Change[
          valid.upper
        ]
      ) >=
        abs(
          observed.upper
        )
    )
  
) / (
  
  sum(
    valid.upper
  ) +
    1
)


# ============================================================
# PERMUTATION SUMMARY
# ============================================================


cat(
  "\n====================================================\n"
)

cat(
  "MOOSE PM2.5 PERMUTATION RESULTS\n"
)

cat(
  "====================================================\n"
)


cat(
  "Observed overall ALE change:",
  round(
    observed.overall,
    6
  ),
  "\n"
)


cat(
  "Overall permutation p-value:",
  round(
    overall.p.value,
    4
  ),
  "\n"
)


cat(
  "\nObserved upper-smoke ALE change:",
  round(
    observed.upper,
    6
  ),
  "\n"
)


cat(
  "Upper-smoke permutation p-value:",
  round(
    upper.p.value,
    4
  ),
  "\n"
)


cat(
  "\nNumber of permutations:",
  n.permutations,
  "\n"
)


# ============================================================
# SAVE RESULTS
# ============================================================


# ------------------------------------------------------------
# 16. Create results directory
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)


# ------------------------------------------------------------
# 17. Save all permutation results
# ------------------------------------------------------------

write.csv(
  
  permutation.results,
  
  "results/Moose_PM25_Permutation_Results.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 18. Create summary table
# ------------------------------------------------------------

permutation.summary <- data.frame(
  
  Species =
    "Moose",
  
  Permutations =
    n.permutations,
  
  Observed_Overall_ALE_Change =
    observed.overall,
  
  Overall_P_Value =
    overall.p.value,
  
  Observed_Upper_ALE_Change =
    observed.upper,
  
  Upper_P_Value =
    upper.p.value,
  
  stringsAsFactors =
    FALSE
)


# ------------------------------------------------------------
# 19. Save summary
# ------------------------------------------------------------

write.csv(
  
  permutation.summary,
  
  "results/Moose_PM25_Permutation_Summary.csv",
  
  row.names =
    FALSE
)


# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "FINAL MOOSE PERMUTATION SUMMARY\n"
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
  "Permutations:",
  n.permutations,
  "\n"
)

cat(
  "\nObserved overall ALE change:",
  round(
    observed.overall,
    6
  ),
  "\n"
)

cat(
  "Overall p-value:",
  round(
    overall.p.value,
    4
  ),
  "\n"
)

cat(
  "Observed upper-smoke ALE change:",
  round(
    observed.upper,
    6
  ),
  "\n"
)

cat(
  "Upper-smoke p-value:",
  round(
    upper.p.value,
    4
  ),
  "\n"
)

cat(
  "====================================================\n"
)