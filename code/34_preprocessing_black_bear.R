# ============================================================
# 34_preprocessing_black_bear.R
#
# BLACK BEAR DATA PREPARATION
#
# PURPOSE:
#
# Prepare the black bear daily camera-trap dataset for the
# Random Forest analyses.
#
# The output contains Daily_Count along with the environmental,
# habitat, camera, and site variables used in the current
# species-level modeling framework.
#
# ============================================================



# ------------------------------------------------------------
# 1. Find available CSV files in data-tidy
# ------------------------------------------------------------

available.files <- list.files(
  "data-tidy",
  pattern = "\\.csv$",
  full.names = TRUE
)



# ------------------------------------------------------------
# 2. Look specifically for Poisson black bear data
# ------------------------------------------------------------

bear.matches <- available.files[
  grepl(
    "poisson-data",
    basename(
      available.files
    ),
    ignore.case = TRUE
  ) &
    grepl(
      "bear",
      basename(
        available.files
      ),
      ignore.case = TRUE
    )
]



# ------------------------------------------------------------
# 3. Check that a matching file was found
# ------------------------------------------------------------

if (
  length(
    bear.matches
  ) == 0
) {
  
  cat(
    "\nNo Poisson black bear CSV was automatically identified.\n"
  )
  
  
  cat(
    "\nFiles containing the word 'bear':\n"
  )
  
  
  print(
    basename(
      available.files[
        grepl(
          "bear",
          basename(
            available.files
          ),
          ignore.case = TRUE
        )
      ]
    )
  )
  
  
  stop(
    "Black bear source file not found."
  )
}



# ------------------------------------------------------------
# 4. If multiple Poisson bear files exist, stop and show them
# ------------------------------------------------------------

if (
  length(
    bear.matches
  ) > 1
) {
  
  cat(
    "\nMultiple possible Poisson black bear files were found:\n"
  )
  
  
  print(
    bear.matches
  )
  
  
  stop(
    "More than one possible black bear source file was identified."
  )
}



# ------------------------------------------------------------
# 5. Set black bear source file
# ------------------------------------------------------------

bear.file <- bear.matches[1]



cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR SOURCE FILE\n"
)

cat(
  "====================================================\n"
)


cat(
  bear.file,
  "\n"
)



# ------------------------------------------------------------
# 6. Load dataset
# ------------------------------------------------------------

bear <- read.csv(
  bear.file
)



# ============================================================
# INITIAL DATA CHECK
# ============================================================



# ------------------------------------------------------------
# 7. Print original dimensions
# ------------------------------------------------------------

cat(
  "\nOriginal dimensions:\n"
)


print(
  dim(
    bear
  )
)



# ------------------------------------------------------------
# 8. Print original column names
# ------------------------------------------------------------

cat(
  "\nOriginal columns:\n"
)


print(
  names(
    bear
  )
)



# ============================================================
# REQUIRED VARIABLES
# ============================================================



# ------------------------------------------------------------
# 9. Define variables required for modeling
# ------------------------------------------------------------

required.variables <- c(
  
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



# ------------------------------------------------------------
# 10. Check whether all required columns exist
# ------------------------------------------------------------

missing.variables <- setdiff(
  required.variables,
  names(
    bear
  )
)



if (
  length(
    missing.variables
  ) > 0
) {
  
  cat(
    "\nMissing required variables:\n"
  )
  
  
  print(
    missing.variables
  )
  
  
  stop(
    "Required model variables are missing from the dataset."
  )
}



# ============================================================
# CREATE MODEL DATA
# ============================================================



# ------------------------------------------------------------
# 11. Keep required variables
# ------------------------------------------------------------

bear.model.data <- bear[
  ,
  required.variables
]



# ------------------------------------------------------------
# 12. Record rows before removing incomplete observations
# ------------------------------------------------------------

rows.before <- nrow(
  bear.model.data
)



# ------------------------------------------------------------
# 13. Remove incomplete observations
# ------------------------------------------------------------

bear.model.data <- bear.model.data[
  complete.cases(
    bear.model.data
  ),
]



# ------------------------------------------------------------
# 14. Record rows after filtering
# ------------------------------------------------------------

rows.after <- nrow(
  bear.model.data
)



cat(
  "\nRows before removing incomplete observations:",
  rows.before,
  "\n"
)


cat(
  "Rows after removing incomplete observations:",
  rows.after,
  "\n"
)


cat(
  "Rows removed:",
  rows.before -
    rows.after,
  "\n"
)



# ============================================================
# RESTORE VARIABLE TYPES
# ============================================================



# ------------------------------------------------------------
# 15. Convert categorical variables to factors
# ------------------------------------------------------------

bear.model.data$Cell_ID <- as.factor(
  bear.model.data$Cell_ID
)


bear.model.data$Road_Type <- as.factor(
  bear.model.data$Road_Type
)


bear.model.data$Habitat_Type <- as.factor(
  bear.model.data$Habitat_Type
)


bear.model.data$Land_Mgnt <- as.factor(
  bear.model.data$Land_Mgnt
)



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR MODEL DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)



cat(
  "Camera-days:",
  nrow(
    bear.model.data
  ),
  "\n"
)



cat(
  "Camera sites:",
  length(
    unique(
      bear.model.data$Cell_ID
    )
  ),
  "\n"
)



cat(
  "Zero-detection days:",
  sum(
    bear.model.data$Daily_Count == 0
  ),
  "\n"
)



cat(
  "Positive-detection days:",
  sum(
    bear.model.data$Daily_Count > 0
  ),
  "\n"
)



cat(
  "Percent zero:",
  round(
    mean(
      bear.model.data$Daily_Count == 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



cat(
  "Total detections:",
  sum(
    bear.model.data$Daily_Count
  ),
  "\n"
)



cat(
  "Mean daily count:",
  round(
    mean(
      bear.model.data$Daily_Count
    ),
    6
  ),
  "\n"
)



# ============================================================
# PM2.5 SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "PM2.5 SUMMARY\n"
)

cat(
  "====================================================\n"
)



print(
  summary(
    bear.model.data$scale.smoke
  )
)



cat(
  "\nPM2.5 percentiles:\n"
)



print(
  quantile(
    bear.model.data$scale.smoke,
    probs = c(
      0.01,
      0.10,
      0.50,
      0.90,
      0.99
    ),
    na.rm = TRUE
  )
)



# ============================================================
# SAVE CLEANED MODEL DATA
# ============================================================



# ------------------------------------------------------------
# 16. Create output folder
# ------------------------------------------------------------

dir.create(
  "data-split",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 17. Save model dataset
# ------------------------------------------------------------

write.csv(
  
  bear.model.data,
  
  "data-split/black_bear_model_data.csv",
  
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "BLACK BEAR PREPROCESSING COMPLETE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Source file:\n"
)


cat(
  bear.file,
  "\n"
)


cat(
  "\nOutput file:\n"
)


cat(
  "data-split/black_bear_model_data.csv\n"
)


cat(
  "\nFinal dimensions:",
  nrow(
    bear.model.data
  ),
  "rows x",
  ncol(
    bear.model.data
  ),
  "columns\n"
)


cat(
  "====================================================\n"
)