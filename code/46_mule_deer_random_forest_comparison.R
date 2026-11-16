# ============================================================
# 46_mule_deer_random_forest_comparison.R
#
# MULE DEER RANDOM FOREST MODEL COMPARISON
#
# PURPOSE:
#
# Compare several Random Forest configurations using the same
# mule deer daily-count dataset and the same grouped train/test
# split.
#
# The goal is to determine whether changing Random Forest
# hyperparameters improves predictive performance compared
# with the original model.
#
# MODELS COMPARED:
#
# 1. Original Random Forest
# 2. Smaller node size
# 3. Higher mtry
# 4. Tuned RF
# 5. Tuned RF with more trees
#
# All models are evaluated on the exact same unseen camera
# sites using:
#
# - Mean Absolute Error
# - Root Mean Squared Error
# - Test R-squared
# - Mean Poisson Deviance
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
# SELECT MODEL VARIABLES
# ============================================================


# ------------------------------------------------------------
# 4. Keep the same variables used in the original model
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
  "MULE DEER RANDOM FOREST COMPARISON\n"
)

cat(
  "====================================================\n"
)

cat(
  "Camera-days:",
  nrow(mule.rf.data),
  "\n"
)

cat(
  "Camera sites:",
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
# GROUPED TRAIN / TEST SPLIT
# ============================================================


# ------------------------------------------------------------
# 6. Identify unique camera sites
# ------------------------------------------------------------

unique.sites <- unique(
  mule.rf.data$Cell_ID
)


# ------------------------------------------------------------
# 7. Reproduce the same 80/20 site split
# ------------------------------------------------------------

set.seed(42)

training.sites <- sample(
  unique.sites,
  size = floor(
    0.80 *
      length(unique.sites)
  )
)

testing.sites <- setdiff(
  unique.sites,
  training.sites
)


# ------------------------------------------------------------
# 8. Create training and testing datasets
# ------------------------------------------------------------

train <- mule.rf.data[
  mule.rf.data$Cell_ID %in% training.sites,
]

test <- mule.rf.data[
  mule.rf.data$Cell_ID %in% testing.sites,
]


# ------------------------------------------------------------
# 9. Remove site identifier from predictive model
# ------------------------------------------------------------

train.model <- train

test.model <- test

train.model$Cell_ID <- NULL

test.model$Cell_ID <- NULL


cat(
  "\nTraining sites:",
  length(training.sites),
  "\n"
)

cat(
  "Testing sites:",
  length(testing.sites),
  "\n"
)

cat(
  "Training camera-days:",
  nrow(train.model),
  "\n"
)

cat(
  "Testing camera-days:",
  nrow(test.model),
  "\n"
)


# ============================================================
# MODEL CONFIGURATIONS
# ============================================================


# ------------------------------------------------------------
# 10. Define models to compare
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
# EVALUATION FUNCTION
# ============================================================


# ------------------------------------------------------------
# 11. Function for evaluating predictions
# ------------------------------------------------------------

evaluate_model <- function(
    observed,
    predicted,
    training.mean
) {
  
  # ---------------------------
  # Mean Absolute Error
  # ---------------------------
  
  mae <- mean(
    abs(
      predicted -
        observed
    )
  )
  
  
  # ---------------------------
  # Root Mean Squared Error
  # ---------------------------
  
  rmse <- sqrt(
    mean(
      (
        predicted -
          observed
      )^2
    )
  )
  
  
  # ---------------------------
  # Test R-squared
  # ---------------------------
  
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
    sse /
      sst
  )
  
  
  # ---------------------------
  # Poisson Deviance
  # ---------------------------
  
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
  
  
  # ---------------------------
  # Baseline Poisson Deviance
  # ---------------------------
  
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
      MAE = mae,
      
      RMSE = rmse,
      
      Test_R2 = r2,
      
      Poisson_Deviance = mean.poisson.dev,
      
      Baseline_Deviance = mean.baseline.dev
    )
  )
}


# ============================================================
# FIT ALL RANDOM FOREST MODELS
# ============================================================


# ------------------------------------------------------------
# 12. Create empty results list
# ------------------------------------------------------------

comparison.results <- list()


# ------------------------------------------------------------
# 13. Store fitted models
# ------------------------------------------------------------

fitted.models <- list()


# ------------------------------------------------------------
# 14. Fit each Random Forest configuration
# ------------------------------------------------------------

for (
  i in 1:nrow(model.settings)
) {
  
  current.name <- model.settings$Model[i]
  
  current.trees <- model.settings$Trees[i]
  
  current.node.size <- model.settings$Min_Node_Size[i]
  
  current.mtry <- model.settings$Mtry[i]
  
  
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
  # Fit model
  # ----------------------------------------------------------
  
  current.model <- ranger(
    
    Daily_Count ~ .,
    
    data = train.model,
    
    num.trees = current.trees,
    
    mtry = current.mtry,
    
    min.node.size = current.node.size,
    
    splitrule = "poisson",
    
    poisson.tau = 1,
    
    importance = "permutation",
    
    respect.unordered.factors = "order",
    
    seed = 42
  )
  
  
  # ----------------------------------------------------------
  # Save fitted model
  # ----------------------------------------------------------
  
  fitted.models[[current.name]] <- current.model
  
  
  # ----------------------------------------------------------
  # Predict unseen test sites
  # ----------------------------------------------------------
  
  predictions <- predict(
    
    current.model,
    
    data = test.model
    
  )$predictions
  
  
  # ----------------------------------------------------------
  # Evaluate model
  # ----------------------------------------------------------
  
  metrics <- evaluate_model(
    
    observed = test.model$Daily_Count,
    
    predicted = predictions,
    
    training.mean = mean(
      train.model$Daily_Count
    )
  )
  
  
  # ----------------------------------------------------------
  # Store results
  # ----------------------------------------------------------
  
  comparison.results[[i]] <- data.frame(
    
    Model = current.name,
    
    Trees = current.trees,
    
    Mtry = current.mtry,
    
    Min_Node_Size = current.node.size,
    
    OOB_R2 = current.model$r.squared,
    
    MAE = metrics["MAE"],
    
    RMSE = metrics["RMSE"],
    
    Test_R2 = metrics["Test_R2"],
    
    Poisson_Deviance =
      metrics["Poisson_Deviance"],
    
    Baseline_Deviance =
      metrics["Baseline_Deviance"],
    
    stringsAsFactors = FALSE
  )
  
  
  cat(
    "\nTest R-squared:",
    round(
      metrics["Test_R2"],
      6
    ),
    "\n"
  )
  
  cat(
    "RMSE:",
    round(
      metrics["RMSE"],
      6
    ),
    "\n"
  )
  
  cat(
    "Poisson deviance:",
    round(
      metrics["Poisson_Deviance"],
      6
    ),
    "\n"
  )
}


# ============================================================
# COMBINE RESULTS
# ============================================================


# ------------------------------------------------------------
# 15. Combine model results into one table
# ------------------------------------------------------------

comparison.table <- do.call(
  rbind,
  comparison.results
)


# ------------------------------------------------------------
# 16. Reset row names
# ------------------------------------------------------------

rownames(
  comparison.table
) <- NULL


cat(
  "\n====================================================\n"
)

cat(
  "MULE DEER RANDOM FOREST COMPARISON RESULTS\n"
)

cat(
  "====================================================\n"
)

print(
  comparison.table
)


# ============================================================
# IDENTIFY BEST MODEL
# ============================================================


# ------------------------------------------------------------
# 17. Find lowest Poisson deviance
# ------------------------------------------------------------

best.poisson.row <- which.min(
  comparison.table$Poisson_Deviance
)

best.poisson.model <-
  comparison.table$Model[
    best.poisson.row
  ]


# ------------------------------------------------------------
# 18. Find lowest RMSE
# ------------------------------------------------------------

best.rmse.row <- which.min(
  comparison.table$RMSE
)

best.rmse.model <-
  comparison.table$Model[
    best.rmse.row
  ]


# ------------------------------------------------------------
# 19. Find highest test R-squared
# ------------------------------------------------------------

best.r2.row <- which.max(
  comparison.table$Test_R2
)

best.r2.model <-
  comparison.table$Model[
    best.r2.row
  ]


cat(
  "\n====================================================\n"
)

cat(
  "BEST MODELS\n"
)

cat(
  "====================================================\n"
)

cat(
  "Lowest Poisson deviance:",
  best.poisson.model,
  "\n"
)

cat(
  "Lowest RMSE:",
  best.rmse.model,
  "\n"
)

cat(
  "Highest test R-squared:",
  best.r2.model,
  "\n"
)


# ============================================================
# COMPARE WITH BASELINE
# ============================================================


# ------------------------------------------------------------
# 20. Determine whether each RF beats baseline deviance
# ------------------------------------------------------------

comparison.table$Beats_Baseline <- ifelse(
  
  comparison.table$Poisson_Deviance <
    comparison.table$Baseline_Deviance,
  
  "Yes",
  
  "No"
)


# ============================================================
# SAVE RESULTS
# ============================================================


# ------------------------------------------------------------
# 21. Create results directory
# ------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 22. Save comparison table
# ------------------------------------------------------------

write.csv(
  
  comparison.table,
  
  "results/Mule_Deer_RF_Model_Comparison.csv",
  
  row.names = FALSE
)


# ============================================================
# CREATE COMPARISON PLOT
# ============================================================


# ------------------------------------------------------------
# 23. Plot Poisson deviance by model
# ------------------------------------------------------------

comparison.plot <- ggplot(
  
  comparison.table,
  
  aes(
    x = reorder(
      Model,
      Poisson_Deviance
    ),
    
    y = Poisson_Deviance
  )
  
) +
  
  geom_col() +
  
  geom_hline(
    
    yintercept =
      comparison.table$Baseline_Deviance[1],
    
    linetype = "dashed"
  ) +
  
  coord_flip() +
  
  labs(
    
    title =
      "Mule Deer Random Forest Model Comparison",
    
    subtitle =
      "Lower Poisson deviance indicates better test-set performance",
    
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
# 24. Save comparison plot
# ------------------------------------------------------------

ggsave(
  
  filename =
    "results/Mule_Deer_RF_Model_Comparison.png",
  
  plot =
    comparison.plot,
  
  width = 8,
  
  height = 6,
  
  dpi = 300
)


# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "FINAL MODEL-COMPARISON SUMMARY\n"
)

cat(
  "====================================================\n"
)

cat(
  "Models compared:",
  nrow(comparison.table),
  "\n"
)

cat(
  "Best model by Poisson deviance:",
  best.poisson.model,
  "\n"
)

cat(
  "Best model by RMSE:",
  best.rmse.model,
  "\n"
)

cat(
  "Best model by test R-squared:",
  best.r2.model,
  "\n"
)

cat(
  "\nComparison table saved to:\n"
)

cat(
  "results/Mule_Deer_RF_Model_Comparison.csv\n"
)

cat(
  "\nComparison plot saved to:\n"
)

cat(
  "results/Mule_Deer_RF_Model_Comparison.png\n"
)

cat(
  "====================================================\n"
)