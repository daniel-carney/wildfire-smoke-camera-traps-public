# ============================================================
# 22_mule_deer_pm25_daily_count_permutation.R
#
# MULE DEER PM2.5 PERMUTATION TEST
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
# PM2.5 is then randomly shuffled, the model is refit, and a
# new ALE response is calculated.
#
# The observed ALE response is compared with the distribution
# of ALE responses from randomized PM2.5 values.
#
# ============================================================


# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(ranger)


# ------------------------------------------------------------
# 2. Load cleaned mule deer dataset
# ------------------------------------------------------------

mule <- read.csv(
  "data-split/mule_deer_model_data.csv"
)


# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

mule$Road_Type <- as.factor(
  mule$Road_Type
)

mule$Habitat_Type <- as.factor(
  mule$Habitat_Type
)

mule$Land_Mgnt <- as.factor(
  mule$Land_Mgnt
)

mule$Cell_ID <- as.factor(
  mule$Cell_ID
)


# ============================================================
# SELECT MODEL VARIABLES
# ============================================================


# ------------------------------------------------------------
# 4. Keep finalized predictor set
# ------------------------------------------------------------

mule.model.data <- mule[
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

mule.model.data <- mule.model.data[
  complete.cases(
    mule.model.data
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
    mule.model.data$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)

smoke.upper <- as.numeric(
  quantile(
    mule.model.data$scale.smoke,
    probs = 0.99,
    na.rm = TRUE
  )
)


# ------------------------------------------------------------
# 7. Define ALE quantile probabilities
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
# 8. Convert quantiles to PM2.5 break values
# ------------------------------------------------------------

smoke.breaks <- as.numeric(
  quantile(
    mule.model.data$scale.smoke,
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
  # Restrict to fixed PM2.5 range
  # ----------------------------------------------------------
  
  current.ale.data <- model.data[
    model.data$scale.smoke >=
      smoke.lower &
      model.data$scale.smoke <=
      smoke.upper,
  ]
  
  
  # ----------------------------------------------------------
  # Create empty interval table
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
  # Calculate local effect in each PM2.5 interval
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
  # Overall low-to-high PM2.5 ALE change
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
  # Compare response from approximately the 90th percentile
  # through the 99th percentile.
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
# 10. Calculate observed Mule Deer PM2.5 ALE
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "CALCULATING OBSERVED MULE DEER PM2.5 RESPONSE\n"
)

cat(
  "====================================================\n"
)


observed.result <- calculate_pm25_ale(
  
  model.data =
    mule.model.data,
  
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
# Use 30 while developing the analysis.
#
# A larger number can be used later for the final analysis.
# ------------------------------------------------------------

n.permutations <- 30


# ------------------------------------------------------------
# 12. Create result storage
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
# 13. Run PM2.5 permutations
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "RUNNING MULE DEER PM2.5 PERMUTATIONS\n"
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
    mule.model.data
  
  
  # ----------------------------------------------------------
  # Randomly shuffle PM2.5
  #
  # The response variable and all other predictors remain
  # unchanged.
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
  # Fit model and calculate randomized ALE
  # ----------------------------------------------------------
  
  permuted.result <- calculate_pm25_ale(
    
    model.data =
      permuted.data,
    
    seed.value =
      2000 + p
  )
  
  
  permutation.results$Overall_ALE_Change[p] <-
    permuted.result$overall.change
  
  
  permutation.results$Upper_ALE_Change[p] <-
    permuted.result$upper.change
}


# ============================================================
# PERMUTATION P-VALUES
# ============================================================


# ------------------------------------------------------------
# 14. Overall ALE p-value
#
# Absolute values are used so that unusually strong positive
# or negative responses both count as extreme.
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
# 15. Upper-smoke ALE p-value
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
# RESULTS
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER PM2.5 PERMUTATION RESULTS\n"
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
# 17. Save individual permutation results
# ------------------------------------------------------------

write.csv(
  
  permutation.results,
  
  "results/Mule_Deer_PM25_Permutation_Results.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 18. Create permutation summary
# ------------------------------------------------------------

permutation.summary <- data.frame(
  
  Species =
    "Mule Deer",
  
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
  
  "results/Mule_Deer_PM25_Permutation_Summary.csv",
  
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
  "FINAL MULE DEER PERMUTATION SUMMARY\n"
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