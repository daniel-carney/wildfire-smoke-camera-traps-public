# ============================================================
# 17_random_forest_daily_count_moose.R
#
# MOOSE DAILY-COUNT RANDOM FOREST
#
# FINAL MODELING METHOD:
#
#   80% Training Sites
#   10% Testing Sites
#   10% Validation Sites
#
# Camera sites are kept completely separate between datasets.
#
# Random Forest settings:
#
#   Trees = 500
#   mtry = 3
#   Minimum node size = 20
#   Split rule = Poisson
#
# The training model is evaluated first on the test set.
#
# The validation set is kept completely untouched until the
# final evaluation.
#
# A second model is then fit using the full dataset for later
# interpretation with variable importance and ALE.
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
# 4. Keep the finalized predictor set
# ------------------------------------------------------------

moose.rf.data <- moose[
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

moose.rf.data <- moose.rf.data[
  complete.cases(
    moose.rf.data
  ),
]


# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MOOSE DAILY-COUNT RANDOM FOREST\n"
)

cat(
  "====================================================\n"
)

cat(
  "Total camera-days:",
  nrow(
    moose.rf.data
  ),
  "\n"
)

cat(
  "Total camera sites:",
  length(
    unique(
      moose.rf.data$Cell_ID
    )
  ),
  "\n"
)

cat(
  "Zero-count days:",
  sum(
    moose.rf.data$Daily_Count == 0
  ),
  "\n"
)

cat(
  "Positive-count days:",
  sum(
    moose.rf.data$Daily_Count > 0
  ),
  "\n"
)

cat(
  "Total detections:",
  sum(
    moose.rf.data$Daily_Count
  ),
  "\n"
)

cat(
  "Mean daily count:",
  round(
    mean(
      moose.rf.data$Daily_Count
    ),
    6
  ),
  "\n"
)


# ============================================================
# 80 / 10 / 10 GROUPED SITE SPLIT
# ============================================================


# ------------------------------------------------------------
# 6. Identify unique camera sites
# ------------------------------------------------------------

unique.sites <- unique(
  moose.rf.data$Cell_ID
)


# ------------------------------------------------------------
# 7. Shuffle camera sites reproducibly
# ------------------------------------------------------------

set.seed(42)

shuffled.sites <- sample(
  unique.sites,
  length(
    unique.sites
  ),
  replace = FALSE
)


# ------------------------------------------------------------
# 8. Calculate number of sites in each dataset
# ------------------------------------------------------------

n.sites <- length(
  shuffled.sites
)

n.train <- floor(
  0.80 *
    n.sites
)

n.test <- floor(
  0.10 *
    n.sites
)

n.validation <- n.sites -
  n.train -
  n.test


# ------------------------------------------------------------
# 9. Assign camera sites
# ------------------------------------------------------------

training.sites <- shuffled.sites[
  1:n.train
]


testing.sites <- shuffled.sites[
  (n.train + 1):
    (n.train + n.test)
]


validation.sites <- shuffled.sites[
  (n.train + n.test + 1):
    n.sites
]


# ------------------------------------------------------------
# 10. Create training, testing, and validation datasets
# ------------------------------------------------------------

train <- moose.rf.data[
  moose.rf.data$Cell_ID %in%
    training.sites,
]


test <- moose.rf.data[
  moose.rf.data$Cell_ID %in%
    testing.sites,
]


validation <- moose.rf.data[
  moose.rf.data$Cell_ID %in%
    validation.sites,
]


# ------------------------------------------------------------
# 11. Remove Cell_ID from predictive model
#
# Cell_ID is used to create the grouped split.
#
# It is not included in the validation model because we want
# to test performance on completely unseen camera sites.
# ------------------------------------------------------------

train.model <- train

test.model <- test

validation.model <- validation


train.model$Cell_ID <- NULL

test.model$Cell_ID <- NULL

validation.model$Cell_ID <- NULL


# ------------------------------------------------------------
# 12. Print split information
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "80 / 10 / 10 DATASET SPLIT\n"
)

cat(
  "====================================================\n"
)

cat(
  "Training sites:",
  length(
    training.sites
  ),
  "\n"
)

cat(
  "Testing sites:",
  length(
    testing.sites
  ),
  "\n"
)

cat(
  "Validation sites:",
  length(
    validation.sites
  ),
  "\n"
)

cat(
  "\nTraining camera-days:",
  nrow(
    train.model
  ),
  "\n"
)

cat(
  "Testing camera-days:",
  nrow(
    test.model
  ),
  "\n"
)

cat(
  "Validation camera-days:",
  nrow(
    validation.model
  ),
  "\n"
)


# ============================================================
# MODEL EVALUATION FUNCTION
# ============================================================


# ------------------------------------------------------------
# 13. Function for calculating predictive performance
# ------------------------------------------------------------

evaluate_model <- function(
    observed,
    predicted,
    training.mean
) {
  
  # ----------------------------------------------------------
  # Mean Absolute Error
  # ----------------------------------------------------------
  
  mae <- mean(
    abs(
      predicted -
        observed
    )
  )
  
  
  # ----------------------------------------------------------
  # Root Mean Squared Error
  # ----------------------------------------------------------
  
  rmse <- sqrt(
    mean(
      (
        predicted -
          observed
      )^2
    )
  )
  
  
  # ----------------------------------------------------------
  # R-squared
  # ----------------------------------------------------------
  
  sse <- sum(
    (
      observed -
        predicted
    )^2
  )
  
  
  sst <- sum(
    (
      observed -
        mean(
          observed
        )
    )^2
  )
  
  
  r2 <- 1 -
    (
      sse /
        sst
    )
  
  
  # ----------------------------------------------------------
  # Poisson deviance
  # ----------------------------------------------------------
  
  epsilon <- 1e-10
  
  
  predicted.safe <- pmax(
    predicted,
    epsilon
  )
  
  
  poisson.dev <- ifelse(
    
    observed == 0,
    
    2 *
      predicted.safe,
    
    2 *
      (
        observed *
          log(
            observed /
              predicted.safe
          ) -
          (
            observed -
              predicted.safe
          )
      )
  )
  
  
  mean.poisson.dev <- mean(
    poisson.dev
  )
  
  
  # ----------------------------------------------------------
  # Simple baseline model
  #
  # Predict the training-set mean for every observation.
  # ----------------------------------------------------------
  
  baseline.predictions <- rep(
    training.mean,
    length(
      observed
    )
  )
  
  
  baseline.safe <- pmax(
    baseline.predictions,
    epsilon
  )
  
  
  baseline.dev <- ifelse(
    
    observed == 0,
    
    2 *
      baseline.safe,
    
    2 *
      (
        observed *
          log(
            observed /
              baseline.safe
          ) -
          (
            observed -
              baseline.safe
          )
      )
  )
  
  
  mean.baseline.dev <- mean(
    baseline.dev
  )
  
  
  return(
    c(
      
      MAE =
        mae,
      
      RMSE =
        rmse,
      
      R2 =
        r2,
      
      Poisson_Deviance =
        mean.poisson.dev,
      
      Baseline_Deviance =
        mean.baseline.dev
    )
  )
}


# ============================================================
# TRAIN RANDOM FOREST
# ============================================================


# ------------------------------------------------------------
# 14. Fit finalized Random Forest
#
# Final settings:
#
# 500 trees
# mtry = 3
# minimum node size = 20
# Poisson splitting rule
# ------------------------------------------------------------

set.seed(42)

moose.daily.rf <- ranger(
  
  Daily_Count ~ .,
  
  data =
    train.model,
  
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


# ============================================================
# OUT-OF-BAG PERFORMANCE
# ============================================================


# ------------------------------------------------------------
# 15. Print OOB R-squared
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "TRAINING / OOB PERFORMANCE\n"
)

cat(
  "====================================================\n"
)

cat(
  "OOB R-squared:",
  round(
    moose.daily.rf$r.squared,
    6
  ),
  "\n"
)


# ============================================================
# TEST SET EVALUATION
# ============================================================


# ------------------------------------------------------------
# 16. Predict the 10% test dataset
# ------------------------------------------------------------

test.predictions <- predict(
  
  moose.daily.rf,
  
  data =
    test.model
  
)$predictions


# ------------------------------------------------------------
# 17. Calculate test metrics
# ------------------------------------------------------------

test.metrics <- evaluate_model(
  
  observed =
    test.model$Daily_Count,
  
  predicted =
    test.predictions,
  
  training.mean =
    mean(
      train.model$Daily_Count
    )
)


cat(
  "\n====================================================\n"
)

cat(
  "TEST SET PERFORMANCE\n"
)

cat(
  "====================================================\n"
)

cat(
  "Observed test mean:",
  round(
    mean(
      test.model$Daily_Count
    ),
    6
  ),
  "\n"
)

cat(
  "Predicted test mean:",
  round(
    mean(
      test.predictions
    ),
    6
  ),
  "\n"
)

cat(
  "Test MAE:",
  round(
    test.metrics["MAE"],
    6
  ),
  "\n"
)

cat(
  "Test RMSE:",
  round(
    test.metrics["RMSE"],
    6
  ),
  "\n"
)

cat(
  "Test R-squared:",
  round(
    test.metrics["R2"],
    6
  ),
  "\n"
)

cat(
  "Test Poisson deviance:",
  round(
    test.metrics[
      "Poisson_Deviance"
    ],
    6
  ),
  "\n"
)

cat(
  "Test baseline deviance:",
  round(
    test.metrics[
      "Baseline_Deviance"
    ],
    6
  ),
  "\n"
)


# ============================================================
# FINAL VALIDATION SET EVALUATION
# ============================================================


# ------------------------------------------------------------
# 18. Predict completely untouched validation sites
# ------------------------------------------------------------

validation.predictions <- predict(
  
  moose.daily.rf,
  
  data =
    validation.model
  
)$predictions


# ------------------------------------------------------------
# 19. Calculate validation metrics
# ------------------------------------------------------------

validation.metrics <- evaluate_model(
  
  observed =
    validation.model$Daily_Count,
  
  predicted =
    validation.predictions,
  
  training.mean =
    mean(
      train.model$Daily_Count
    )
)


cat(
  "\n====================================================\n"
)

cat(
  "FINAL VALIDATION PERFORMANCE\n"
)

cat(
  "====================================================\n"
)

cat(
  "Observed validation mean:",
  round(
    mean(
      validation.model$Daily_Count
    ),
    6
  ),
  "\n"
)

cat(
  "Predicted validation mean:",
  round(
    mean(
      validation.predictions
    ),
    6
  ),
  "\n"
)

cat(
  "Validation MAE:",
  round(
    validation.metrics["MAE"],
    6
  ),
  "\n"
)

cat(
  "Validation RMSE:",
  round(
    validation.metrics["RMSE"],
    6
  ),
  "\n"
)

cat(
  "Validation R-squared:",
  round(
    validation.metrics["R2"],
    6
  ),
  "\n"
)

cat(
  "Validation Poisson deviance:",
  round(
    validation.metrics[
      "Poisson_Deviance"
    ],
    6
  ),
  "\n"
)

cat(
  "Validation baseline deviance:",
  round(
    validation.metrics[
      "Baseline_Deviance"
    ],
    6
  ),
  "\n"
)


# ============================================================
# VARIABLE IMPORTANCE — VALIDATION MODEL
# ============================================================


# ------------------------------------------------------------
# 20. Extract permutation importance
# ------------------------------------------------------------

validation.importance <- data.frame(
  
  Variable =
    names(
      moose.daily.rf$variable.importance
    ),
  
  Importance =
    as.numeric(
      moose.daily.rf$variable.importance
    ),
  
  stringsAsFactors =
    FALSE
)


# ------------------------------------------------------------
# 21. Sort from highest to lowest importance
# ------------------------------------------------------------

validation.importance <- validation.importance[
  order(
    validation.importance$Importance,
    decreasing = TRUE
  ),
]


rownames(
  validation.importance
) <- NULL


cat(
  "\n====================================================\n"
)

cat(
  "VALIDATION MODEL VARIABLE IMPORTANCE\n"
)

cat(
  "====================================================\n"
)

print(
  validation.importance
)


# ------------------------------------------------------------
# 22. Find PM2.5 importance rank
# ------------------------------------------------------------

smoke.rank.validation <- which(
  validation.importance$Variable ==
    "scale.smoke"
)


cat(
  "\nPM2.5 importance rank:",
  smoke.rank.validation,
  "of",
  nrow(
    validation.importance
  ),
  "\n"
)


# ============================================================
# FULL-DATA INTERPRETATION MODEL
# ============================================================


# ------------------------------------------------------------
# 23. Prepare full dataset
#
# Cell_ID is retained here as a factor because this model is
# used for interpretation rather than prediction at new sites.
#
# This allows the model to represent persistent differences
# among camera locations.
# ------------------------------------------------------------

full.model.data <- moose.rf.data


# ------------------------------------------------------------
# 24. Fit full-data interpretation model
#
# Uses the exact same finalized RF settings.
# ------------------------------------------------------------

set.seed(42)

moose.daily.rf.full <- ranger(
  
  Daily_Count ~ .,
  
  data =
    full.model.data,
  
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


# ------------------------------------------------------------
# 25. Print full-model OOB R-squared
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FULL-DATA INTERPRETATION MODEL\n"
)

cat(
  "====================================================\n"
)

cat(
  "Full-model OOB R-squared:",
  round(
    moose.daily.rf.full$r.squared,
    6
  ),
  "\n"
)


# ============================================================
# FULL-MODEL VARIABLE IMPORTANCE
# ============================================================


# ------------------------------------------------------------
# 26. Extract full-model permutation importance
# ------------------------------------------------------------

full.importance <- data.frame(
  
  Variable =
    names(
      moose.daily.rf.full$variable.importance
    ),
  
  Importance =
    as.numeric(
      moose.daily.rf.full$variable.importance
    ),
  
  stringsAsFactors =
    FALSE
)


# ------------------------------------------------------------
# 27. Sort full-model importance
# ------------------------------------------------------------

full.importance <- full.importance[
  order(
    full.importance$Importance,
    decreasing = TRUE
  ),
]


rownames(
  full.importance
) <- NULL


cat(
  "\n====================================================\n"
)

cat(
  "FULL MODEL VARIABLE IMPORTANCE\n"
)

cat(
  "====================================================\n"
)

print(
  full.importance
)


# ------------------------------------------------------------
# 28. Calculate ecological predictor PM2.5 rank
#
# Cell_ID is removed from the ranking because it is a site
# identifier rather than an ecological or camera predictor.
# ------------------------------------------------------------

ecological.importance <- full.importance[
  full.importance$Variable !=
    "Cell_ID",
]


smoke.rank.full <- which(
  ecological.importance$Variable ==
    "scale.smoke"
)


cat(
  "\nPM2.5 ecological/camera importance rank:",
  smoke.rank.full,
  "of",
  nrow(
    ecological.importance
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
# 30. Create model-performance table
# ------------------------------------------------------------

performance.table <- data.frame(
  
  Dataset = c(
    "Test",
    "Validation"
  ),
  
  MAE = c(
    test.metrics["MAE"],
    validation.metrics["MAE"]
  ),
  
  RMSE = c(
    test.metrics["RMSE"],
    validation.metrics["RMSE"]
  ),
  
  R2 = c(
    test.metrics["R2"],
    validation.metrics["R2"]
  ),
  
  Poisson_Deviance = c(
    test.metrics[
      "Poisson_Deviance"
    ],
    validation.metrics[
      "Poisson_Deviance"
    ]
  ),
  
  Baseline_Deviance = c(
    test.metrics[
      "Baseline_Deviance"
    ],
    validation.metrics[
      "Baseline_Deviance"
    ]
  ),
  
  stringsAsFactors =
    FALSE
)


# ------------------------------------------------------------
# 31. Add whether RF beats baseline
# ------------------------------------------------------------

performance.table$Beats_Baseline <- ifelse(
  
  performance.table$Poisson_Deviance <
    performance.table$Baseline_Deviance,
  
  "Yes",
  
  "No"
)


# ------------------------------------------------------------
# 32. Save model performance
# ------------------------------------------------------------

write.csv(
  
  performance.table,
  
  "results/Moose_Daily_Count_RF_Performance.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 33. Save validation-model importance
# ------------------------------------------------------------

write.csv(
  
  validation.importance,
  
  "results/Moose_Daily_Count_RF_Validation_Importance.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 34. Save full-model importance
# ------------------------------------------------------------

write.csv(
  
  full.importance,
  
  "results/Moose_Daily_Count_RF_Full_Model_Importance.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 35. Save site assignments
# ------------------------------------------------------------

site.assignments <- data.frame(
  
  Cell_ID = c(
    
    as.character(
      training.sites
    ),
    
    as.character(
      testing.sites
    ),
    
    as.character(
      validation.sites
    )
  ),
  
  Dataset = c(
    
    rep(
      "Training",
      length(
        training.sites
      )
    ),
    
    rep(
      "Test",
      length(
        testing.sites
      )
    ),
    
    rep(
      "Validation",
      length(
        validation.sites
      )
    )
  ),
  
  stringsAsFactors =
    FALSE
)


write.csv(
  
  site.assignments,
  
  "results/Moose_80_10_10_Site_Assignments.csv",
  
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
  "FINAL MOOSE RANDOM FOREST SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Split: 80% training / 10% test / 10% validation\n"
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
  "\nTest R-squared:",
  round(
    test.metrics["R2"],
    6
  ),
  "\n"
)


cat(
  "Test Poisson deviance:",
  round(
    test.metrics[
      "Poisson_Deviance"
    ],
    6
  ),
  "\n"
)


cat(
  "Validation R-squared:",
  round(
    validation.metrics["R2"],
    6
  ),
  "\n"
)


cat(
  "Validation Poisson deviance:",
  round(
    validation.metrics[
      "Poisson_Deviance"
    ],
    6
  ),
  "\n"
)


cat(
  "Validation baseline deviance:",
  round(
    validation.metrics[
      "Baseline_Deviance"
    ],
    6
  ),
  "\n"
)


cat(
  "PM2.5 validation importance rank:",
  smoke.rank.validation,
  "\n"
)


cat(
  "PM2.5 full-model ecological rank:",
  smoke.rank.full,
  "\n"
)


cat(
  "====================================================\n"
)