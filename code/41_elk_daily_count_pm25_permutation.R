# ============================================================
# 41_elk_daily_count_pm25_permutation.R
#
# ELK PM2.5 PERMUTATION TEST
#
# PURPOSE:
#
# Test whether the PM2.5 relationship estimated by the
# Elk Random Forest is stronger than relationships that
# can arise after PM2.5 is randomly rearranged among
# camera-days.
#
# Final Random Forest settings:
#
#   Trees = 500
#   mtry = 3
#   Minimum node size = 20
#   Split rule = Poisson
#
# TWO PM2.5 RESPONSE STATISTICS ARE TESTED:
#
#   1. Overall ALE change
#
#   2. Upper-smoke ALE change
#
# The same model settings and ALE boundaries are used for
# observed and randomized datasets.
#
# ============================================================


# ------------------------------------------------------------
# 1. Load package
# ------------------------------------------------------------

library(ranger)


# ------------------------------------------------------------
# 2. Load cleaned elk data
# ------------------------------------------------------------

elk <- read.csv(
  "data-split/elk_model_data.csv"
)


# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

elk$Road_Type <- as.factor(
  elk$Road_Type
)

elk$Habitat_Type <- as.factor(
  elk$Habitat_Type
)

elk$Land_Mgnt <- as.factor(
  elk$Land_Mgnt
)

elk$Cell_ID <- as.factor(
  elk$Cell_ID
)


# ============================================================
# SELECT MODEL VARIABLES
# ============================================================

elk.model.data <- elk[
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


elk.model.data <- elk.model.data[
  complete.cases(
    elk.model.data
  ),
]


# ============================================================
# FIXED PM2.5 RANGE
# ============================================================

smoke.lower <- as.numeric(
  quantile(
    elk.model.data$scale.smoke,
    probs = 0.01,
    na.rm = TRUE
  )
)


smoke.upper <- as.numeric(
  quantile(
    elk.model.data$scale.smoke,
    probs = 0.99,
    na.rm = TRUE
  )
)


# ============================================================
# FIXED ALE BREAKS
# ============================================================

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


smoke.breaks <- as.numeric(
  quantile(
    elk.model.data$scale.smoke,
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

calculate_pm25_ale <- function(
    model.data,
    seed.value
) {
  
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
  
  
  current.ale.data <- model.data[
    model.data$scale.smoke >=
      smoke.lower &
      model.data$scale.smoke <=
      smoke.upper,
  ]
  
  
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
  
  
  current.intervals$ALE_Raw <- cumsum(
    current.intervals$Local_Effect
  )
  
  
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
  # Approximately 90th through 99th percentile
  # ----------------------------------------------------------
  
  upper.start <- as.numeric(
    quantile(
      model.data$scale.smoke,
      probs = 0.90,
      na.rm = TRUE
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
# OBSERVED ELK RESPONSE
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "CALCULATING OBSERVED ELK PM2.5 RESPONSE\n"
)

cat(
  "====================================================\n"
)


observed.result <- calculate_pm25_ale(
  
  model.data =
    elk.model.data,
  
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

n.permutations <- 30


permutation.results <- data.frame(
  
  Permutation =
    1:n.permutations,
  
  Overall_ALE_Change =
    NA_real_,
  
  Upper_ALE_Change =
    NA_real_
)


cat(
  "\n====================================================\n"
)

cat(
  "RUNNING ELK PM2.5 PERMUTATIONS\n"
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
  
  
  permuted.data <-
    elk.model.data
  
  
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
# PRINT RESULTS
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "ELK PM2.5 PERMUTATION RESULTS\n"
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

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)


write.csv(
  
  permutation.results,
  
  "results/Elk_PM25_Permutation_Results.csv",
  
  row.names =
    FALSE
)


permutation.summary <- data.frame(
  
  Species =
    "Elk",
  
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


write.csv(
  
  permutation.summary,
  
  "results/Elk_PM25_Permutation_Summary.csv",
  
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
  "FINAL ELK PERMUTATION SUMMARY\n"
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