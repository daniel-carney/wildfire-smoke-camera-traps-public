# ============================================================
# 43_random_forest_daily_count_cougar.R
#
# COUGAR DAILY-COUNT RANDOM FOREST
#
# FINAL MODELING METHOD:
#
#   80% Training Sites
#   10% Testing Sites
#   10% Validation Sites
#
# Camera sites are kept completely separate between datasets.
#
# Final Random Forest settings:
#
#   Trees = 500
#   mtry = 3
#   Minimum node size = 20
#   Split rule = Poisson
#
# The training model is evaluated on unseen test sites and
# untouched validation sites.
#
# A second model is fit using the complete dataset for
# interpretation of predictor importance and later PM2.5 ALE.
#
# ============================================================


# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(ranger)


# ------------------------------------------------------------
# 2. Load cleaned cougar dataset
# ------------------------------------------------------------

cougar <- read.csv(
  "data-split/cougar_model_data.csv"
)


# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

cougar$Road_Type <- as.factor(
  cougar$Road_Type
)

cougar$Habitat_Type <- as.factor(
  cougar$Habitat_Type
)

cougar$Land_Mgnt <- as.factor(
  cougar$Land_Mgnt
)

cougar$Cell_ID <- as.factor(
  cougar$Cell_ID
)


# ============================================================
# SELECT MODEL VARIABLES
# ============================================================


# ------------------------------------------------------------
# 4. Keep finalized predictor set
# ------------------------------------------------------------

cougar.rf.data <- cougar[
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

cougar.rf.data <- cougar.rf.data[
  complete.cases(
    cougar.rf.data
  ),
]


# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COUGAR DAILY-COUNT RANDOM FOREST\n"
)

cat(
  "====================================================\n"
)

cat(
  "Total camera-days:",
  nrow(
    cougar.rf.data
  ),
  "\n"
)

cat(
  "Total camera sites:",
  length(
    unique(
      cougar.rf.data$Cell_ID
    )
  ),
  "\n"
)

cat(
  "Zero-count days:",
  sum(
    cougar.rf.data$Daily_Count == 0
  ),
  "\n"
)

cat(
  "Positive-count days:",
  sum(
    cougar.rf.data$Daily_Count > 0
  ),
  "\n"
)

cat(
  "Total detections:",
  sum(
    cougar.rf.data$Daily_Count
  ),
  "\n"
)

cat(
  "Mean daily count:",
  round(
    mean(
      cougar.rf.data$Daily_Count
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
  cougar.rf.data$Cell_ID
)


# ------------------------------------------------------------
# 7. Shuffle sites reproducibly
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
# 8. Calculate number of sites per dataset
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
# 9. Assign sites
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
# 10. Create row-level datasets
# ------------------------------------------------------------

train <- cougar.rf.data[
  cougar.rf.data$Cell_ID %in%
    training.sites,
]


test <- cougar.rf.data[
  cougar.rf.data$Cell_ID %in%
    testing.sites,
]


validation <- cougar.rf.data[
  cougar.rf.data$Cell_ID %in%
    validation.sites,
]


# ------------------------------------------------------------
# 11. Remove Cell_ID from predictive models
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

evaluate_model <- function(
    observed,
    predicted,
    training.mean
) {
  
  mae <- mean(
    abs(
      predicted -
        observed
    )
  )
  
  
  rmse <- sqrt(
    mean(
      (
        predicted -
          observed
      )^2
    )
  )
  
  
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
# TRAIN PREDICTIVE RANDOM FOREST
# ============================================================

set.seed(42)

cougar.daily.rf <- ranger(
  
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
# OOB PERFORMANCE
# ============================================================

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
    cougar.daily.rf$r.squared,
    6
  ),
  "\n"
)


# ============================================================
# TEST SET PERFORMANCE
# ============================================================

test.predictions <- predict(
  
  cougar.daily.rf,
  
  data =
    test.model
  
)$predictions


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
# VALIDATION PERFORMANCE
# ============================================================

validation.predictions <- predict(
  
  cougar.daily.rf,
  
  data =
    validation.model
  
)$predictions


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
# PREDICTIVE VARIABLE IMPORTANCE
# ============================================================

validation.importance <- data.frame(
  
  Variable =
    names(
      cougar.daily.rf$variable.importance
    ),
  
  Importance =
    as.numeric(
      cougar.daily.rf$variable.importance
    ),
  
  stringsAsFactors =
    FALSE
)


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
  "PREDICTIVE MODEL VARIABLE IMPORTANCE\n"
)

cat(
  "====================================================\n"
)

print(
  validation.importance
)


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

full.model.data <- cougar.rf.data


set.seed(42)

cougar.daily.rf.full <- ranger(
  
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
    cougar.daily.rf.full$r.squared,
    6
  ),
  "\n"
)


# ============================================================
# FULL-MODEL VARIABLE IMPORTANCE
# ============================================================

full.importance <- data.frame(
  
  Variable =
    names(
      cougar.daily.rf.full$variable.importance
    ),
  
  Importance =
    as.numeric(
      cougar.daily.rf.full$variable.importance
    ),
  
  stringsAsFactors =
    FALSE
)


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

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)


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


performance.table$Beats_Baseline <- ifelse(
  
  performance.table$Poisson_Deviance <
    performance.table$Baseline_Deviance,
  
  "Yes",
  
  "No"
)


write.csv(
  
  performance.table,
  
  "results/Cougar_Daily_Count_RF_Performance.csv",
  
  row.names =
    FALSE
)


write.csv(
  
  validation.importance,
  
  "results/Cougar_Daily_Count_RF_Validation_Importance.csv",
  
  row.names =
    FALSE
)


write.csv(
  
  full.importance,
  
  "results/Cougar_Daily_Count_RF_Full_Model_Importance.csv",
  
  row.names =
    FALSE
)


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
  
  "results/Cougar_80_10_10_Site_Assignments.csv",
  
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
  "FINAL COUGAR RANDOM FOREST SUMMARY\n"
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
  "Test baseline deviance:",
  round(
    test.metrics[
      "Baseline_Deviance"
    ],
    6
  ),
  "\n"
)

cat(
  "\nValidation R-squared:",
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
  "\nPM2.5 predictive-model importance rank:",
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