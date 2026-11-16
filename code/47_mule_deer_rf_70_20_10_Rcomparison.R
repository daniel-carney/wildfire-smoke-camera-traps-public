# ============================================================
# 47_mule_deer_rf_80_10_10_comparison.R
#
# MULE DEER RANDOM FOREST
# 80 / 10 / 10 TRAIN-TEST-VALIDATION COMPARISON
#
# PURPOSE:
#
# Compare several Random Forest configurations using:
#
#   80% Training Sites
#   10% Testing Sites
#   10% Validation Sites
#
# Camera sites are kept completely separate between datasets.
#
# The test dataset is used to compare model configurations.
#
# The validation dataset remains untouched until the best
# Random Forest configuration has been selected.
#
# ============================================================


# ------------------------------------------------------------
# 1. Load required packages
# ------------------------------------------------------------

library(ranger)
library(ggplot2)


# ------------------------------------------------------------
# 2. Load cleaned mule deer dataset
# ------------------------------------------------------------

mule <- read.csv(
  "data-split/mule_deer_model_data.csv"
)


# ------------------------------------------------------------
# 3. Restore categorical variables
# ------------------------------------------------------------

mule$Road_Type <- as.factor(mule$Road_Type)

mule$Habitat_Type <- as.factor(mule$Habitat_Type)

mule$Land_Mgnt <- as.factor(mule$Land_Mgnt)

mule$Cell_ID <- as.factor(mule$Cell_ID)


# ============================================================
# SELECT VARIABLES
# ============================================================


# ------------------------------------------------------------
# 4. Keep variables used in the Random Forest
# ------------------------------------------------------------

mule.rf.data <- mule[
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

mule.rf.data <- mule.rf.data[
  complete.cases(mule.rf.data),
]


# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER 80/10/10 RANDOM FOREST COMPARISON\n"
)

cat(
  "====================================================\n"
)

cat(
  "Total camera-days:",
  nrow(mule.rf.data),
  "\n"
)

cat(
  "Total camera sites:",
  length(unique(mule.rf.data$Cell_ID)),
  "\n"
)

cat(
  "Mean daily count:",
  round(
    mean(mule.rf.data$Daily_Count),
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
  mule.rf.data$Cell_ID
)


# ------------------------------------------------------------
# 7. Shuffle sites reproducibly
# ------------------------------------------------------------

set.seed(42)

shuffled.sites <- sample(
  unique.sites,
  length(unique.sites),
  replace = FALSE
)


# ------------------------------------------------------------
# 8. Calculate number of sites for each dataset
# ------------------------------------------------------------

n.sites <- length(
  shuffled.sites
)

n.train <- floor(
  0.80 * n.sites
)

n.test <- floor(
  0.10 * n.sites
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
# 10. Create datasets
# ------------------------------------------------------------

train <- mule.rf.data[
  mule.rf.data$Cell_ID %in%
    training.sites,
]


test <- mule.rf.data[
  mule.rf.data$Cell_ID %in%
    testing.sites,
]


validation <- mule.rf.data[
  mule.rf.data$Cell_ID %in%
    validation.sites,
]


# ------------------------------------------------------------
# 11. Remove Cell_ID from predictive datasets
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
  "DATASET SPLIT\n"
)

cat(
  "====================================================\n"
)

cat(
  "Training sites:",
  length(training.sites),
  "\n"
)

cat(
  "Testing sites:",
  length(testing.sites),
  "\n"
)

cat(
  "Validation sites:",
  length(validation.sites),
  "\n"
)

cat(
  "\nTraining camera-days:",
  nrow(train.model),
  "\n"
)

cat(
  "Testing camera-days:",
  nrow(test.model),
  "\n"
)

cat(
  "Validation camera-days:",
  nrow(validation.model),
  "\n"
)


# ============================================================
# MODEL CONFIGURATIONS
# ============================================================


# ------------------------------------------------------------
# 13. Define Random Forest configurations
# ------------------------------------------------------------

model.settings <- data.frame(
  
  Model = c(
    "Original RF",
    "Smaller Node RF",
    "Higher mtry RF",
    "Tuned RF",
    "Tuned RF 1000 Trees"
  ),
  
  Trees = c(
    500,
    500,
    500,
    500,
    1000
  ),
  
  Min_Node_Size = c(
    20,
    10,
    20,
    10,
    10
  ),
  
  Mtry = c(
    3,
    3,
    5,
    5,
    5
  ),
  
  stringsAsFactors = FALSE
)


cat(
  "\n====================================================\n"
)

cat(
  "MODEL CONFIGURATIONS\n"
)

cat(
  "====================================================\n"
)

print(
  model.settings
)


# ============================================================
# MODEL EVALUATION FUNCTION
# ============================================================


# ------------------------------------------------------------
# 14. Function for calculating performance metrics
# ------------------------------------------------------------

evaluate_model <- function(
    observed,
    predicted,
    training.mean
) {
  
  # Mean Absolute Error
  
  mae <- mean(
    abs(
      predicted -
        observed
    )
  )
  
  
  # Root Mean Squared Error
  
  rmse <- sqrt(
    mean(
      (
        predicted -
          observed
      )^2
    )
  )
  
  
  # R-squared
  
  sse <- sum(
    (
      observed -
        predicted
    )^2
  )
  
  
  sst <- sum(
    (
      observed -
        mean(observed)
    )^2
  )
  
  
  r2 <- 1 - (
    sse / sst
  )
  
  
  # Prevent log(0)
  
  epsilon <- 1e-10
  
  
  predicted.safe <- pmax(
    predicted,
    epsilon
  )
  
  
  # Poisson deviance
  
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
  
  
  # Simple mean-count baseline
  
  baseline.predictions <- rep(
    training.mean,
    length(observed)
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
# TRAIN AND TEST MODEL CONFIGURATIONS
# ============================================================


# ------------------------------------------------------------
# 15. Empty lists for results and fitted models
# ------------------------------------------------------------

comparison.results <- list()

fitted.models <- list()


# ------------------------------------------------------------
# 16. Train every RF configuration
# ------------------------------------------------------------

for (
  i in 1:nrow(model.settings)
) {
  
  current.name <-
    model.settings$Model[i]
  
  current.trees <-
    model.settings$Trees[i]
  
  current.node.size <-
    model.settings$Min_Node_Size[i]
  
  current.mtry <-
    model.settings$Mtry[i]
  
  
  cat(
    "\n====================================================\n"
  )
  
  cat(
    "FITTING:",
    current.name,
    "\n"
  )
  
  cat(
    "====================================================\n"
  )
  
  cat(
    "Trees:",
    current.trees,
    "\n"
  )
  
  cat(
    "mtry:",
    current.mtry,
    "\n"
  )
  
  cat(
    "Minimum node size:",
    current.node.size,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Fit Random Forest using TRAINING data only
  # ----------------------------------------------------------
  
  current.model <- ranger(
    
    Daily_Count ~ .,
    
    data =
      train.model,
    
    num.trees =
      current.trees,
    
    mtry =
      current.mtry,
    
    min.node.size =
      current.node.size,
    
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
  
  
  # Save fitted model
  
  fitted.models[[current.name]] <-
    current.model
  
  
  # ----------------------------------------------------------
  # Predict TEST data
  # ----------------------------------------------------------
  
  test.predictions <- predict(
    
    current.model,
    
    data =
      test.model
    
  )$predictions
  
  
  # ----------------------------------------------------------
  # Evaluate TEST performance
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # Store TEST results
  # ----------------------------------------------------------
  
  comparison.results[[i]] <- data.frame(
    
    Model =
      current.name,
    
    Trees =
      current.trees,
    
    Mtry =
      current.mtry,
    
    Min_Node_Size =
      current.node.size,
    
    OOB_R2 =
      current.model$r.squared,
    
    Test_MAE =
      test.metrics["MAE"],
    
    Test_RMSE =
      test.metrics["RMSE"],
    
    Test_R2 =
      test.metrics["R2"],
    
    Test_Poisson_Deviance =
      test.metrics[
        "Poisson_Deviance"
      ],
    
    Test_Baseline_Deviance =
      test.metrics[
        "Baseline_Deviance"
      ],
    
    stringsAsFactors =
      FALSE
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
    "Test RMSE:",
    round(
      test.metrics["RMSE"],
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
}


# ============================================================
# COMBINE TEST RESULTS
# ============================================================


# ------------------------------------------------------------
# 17. Combine results into one table
# ------------------------------------------------------------

comparison.table <- do.call(
  rbind,
  comparison.results
)


rownames(
  comparison.table
) <- NULL


cat(
  "\n====================================================\n"
)

cat(
  "80/10/10 TEST-SET MODEL COMPARISON\n"
)

cat(
  "====================================================\n"
)

print(
  comparison.table
)


# ============================================================
# SELECT BEST MODEL USING TEST DATA
# ============================================================


# ------------------------------------------------------------
# 18. Select model with lowest test Poisson deviance
# ------------------------------------------------------------

best.row <- which.min(
  comparison.table$Test_Poisson_Deviance
)


best.model.name <- comparison.table$Model[
  best.row
]


best.model <- fitted.models[[best.model.name]]


cat(
  "\n====================================================\n"
)

cat(
  "BEST MODEL SELECTED USING TEST DATA\n"
)

cat(
  "====================================================\n"
)

cat(
  "Best configuration:",
  best.model.name,
  "\n"
)

cat(
  "Test Poisson deviance:",
  round(
    comparison.table$Test_Poisson_Deviance[
      best.row
    ],
    6
  ),
  "\n"
)


# ============================================================
# FINAL VALIDATION
# ============================================================


# ------------------------------------------------------------
# 19. Predict untouched validation data
# ------------------------------------------------------------

cat(
  "\n====================================================\n"
)

cat(
  "FINAL UNSEEN VALIDATION EVALUATION\n"
)

cat(
  "====================================================\n"
)


validation.predictions <- predict(
  
  best.model,
  
  data =
    validation.model
  
)$predictions


# ------------------------------------------------------------
# 20. Calculate validation metrics
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
# CREATE VALIDATION RESULTS TABLE
# ============================================================


# ------------------------------------------------------------
# 21. Save selected model validation results
# ------------------------------------------------------------

validation.table <- data.frame(
  
  Selected_Model =
    best.model.name,
  
  Validation_MAE =
    validation.metrics["MAE"],
  
  Validation_RMSE =
    validation.metrics["RMSE"],
  
  Validation_R2 =
    validation.metrics["R2"],
  
  Validation_Poisson_Deviance =
    validation.metrics[
      "Poisson_Deviance"
    ],
  
  Validation_Baseline_Deviance =
    validation.metrics[
      "Baseline_Deviance"
    ],
  
  stringsAsFactors =
    FALSE
)


# ============================================================
# CHECK WHETHER MODELS BEAT SIMPLE BASELINE
# ============================================================


# ------------------------------------------------------------
# 22. Test-set baseline comparison
# ------------------------------------------------------------

comparison.table$Beats_Test_Baseline <- ifelse(
  
  comparison.table$Test_Poisson_Deviance <
    comparison.table$Test_Baseline_Deviance,
  
  "Yes",
  
  "No"
)


# ------------------------------------------------------------
# 23. Validation baseline comparison
# ------------------------------------------------------------

validation.table$Beats_Validation_Baseline <- ifelse(
  
  validation.table$Validation_Poisson_Deviance <
    validation.table$Validation_Baseline_Deviance,
  
  "Yes",
  
  "No"
)


# ============================================================
# SAVE RESULTS
# ============================================================


# ------------------------------------------------------------
# 24. Create results folder if necessary
# ------------------------------------------------------------

dir.create(
  "results",
  recursive =
    TRUE,
  showWarnings =
    FALSE
)


# ------------------------------------------------------------
# 25. Save test comparison table
# ------------------------------------------------------------

write.csv(
  
  comparison.table,
  
  "results/Mule_Deer_RF_80_10_10_Test_Comparison.csv",
  
  row.names =
    FALSE
)


# ------------------------------------------------------------
# 26. Save validation results
# ------------------------------------------------------------

write.csv(
  
  validation.table,
  
  "results/Mule_Deer_RF_80_10_10_Validation.csv",
  
  row.names =
    FALSE
)


# ============================================================
# TEST PERFORMANCE PLOT
# ============================================================


# ------------------------------------------------------------
# 27. Plot test Poisson deviance
# ------------------------------------------------------------

comparison.plot <- ggplot(
  
  comparison.table,
  
  aes(
    
    x = reorder(
      Model,
      Test_Poisson_Deviance
    ),
    
    y =
      Test_Poisson_Deviance
  )
  
) +
  
  geom_col() +
  
  geom_hline(
    
    yintercept =
      comparison.table$Test_Baseline_Deviance[1],
    
    linetype =
      "dashed"
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "Mule Deer Random Forest 80/10/10 Comparison",
    
    subtitle =
      "Models compared using the 10% test dataset",
    
    x =
      "Random Forest Configuration",
    
    y =
      "Mean Poisson Deviance"
  ) +
  
  theme_minimal()


print(
  comparison.plot
)


# ------------------------------------------------------------
# 28. Save comparison plot
# ------------------------------------------------------------

ggsave(
  
  filename =
    "results/Mule_Deer_RF_80_10_10_Test_Comparison.png",
  
  plot =
    comparison.plot,
  
  width =
    8,
  
  height =
    6,
  
  dpi =
    300
)


# ============================================================
# SAVE SITE ASSIGNMENTS
# ============================================================


# ------------------------------------------------------------
# 29. Create site assignment table
# ------------------------------------------------------------

site.assignments <- data.frame(
  
  Cell_ID = c(
    as.character(training.sites),
    as.character(testing.sites),
    as.character(validation.sites)
  ),
  
  Dataset = c(
    
    rep(
      "Training",
      length(training.sites)
    ),
    
    rep(
      "Test",
      length(testing.sites)
    ),
    
    rep(
      "Validation",
      length(validation.sites)
    )
  )
)


# ------------------------------------------------------------
# 30. Save site assignments
# ------------------------------------------------------------

write.csv(
  
  site.assignments,
  
  "results/Mule_Deer_80_10_10_Site_Assignments.csv",
  
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
  "FINAL 80/10/10 SUMMARY\n"
)

cat(
  "====================================================\n"
)


cat(
  "Training sites:",
  length(training.sites),
  "\n"
)

cat(
  "Testing sites:",
  length(testing.sites),
  "\n"
)

cat(
  "Validation sites:",
  length(validation.sites),
  "\n"
)


cat(
  "\nBest model from test data:",
  best.model.name,
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
  "Validation RMSE:",
  round(
    validation.metrics["RMSE"],
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
  "\nResults saved to:\n"
)


cat(
  "results/Mule_Deer_RF_80_10_10_Test_Comparison.csv\n"
)

cat(
  "results/Mule_Deer_RF_80_10_10_Validation.csv\n"
)

cat(
  "results/Mule_Deer_80_10_10_Site_Assignments.csv\n"
)

cat(
  "results/Mule_Deer_RF_80_10_10_Test_Comparison.png\n"
)


cat(
  "====================================================\n"
)