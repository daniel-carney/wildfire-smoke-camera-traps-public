# ============================================================
# 42_preprocessing_cougar.R
#
# COUGAR DATA PREPARATION
#
# PURPOSE:
#
# Prepare the cougar daily camera-trap dataset for the
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
# 2. Look specifically for Poisson cougar data
# ------------------------------------------------------------

cougar.matches <- available.files[
  grepl(
    "poisson-data",
    basename(
      available.files
    ),
    ignore.case = TRUE
  ) &
    grepl(
      "cougar",
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
    cougar.matches
  ) == 0
) {
  
  cat(
    "\nNo Poisson cougar CSV was automatically identified.\n"
  )
  
  
  cat(
    "\nFiles containing the word 'cougar':\n"
  )
  
  
  print(
    basename(
      available.files[
        grepl(
          "cougar",
          basename(
            available.files
          ),
          ignore.case = TRUE
        )
      ]
    )
  )
  
  
  stop(
    "Cougar source file not found."
  )
}



# ------------------------------------------------------------
# 4. If multiple Poisson cougar files exist, stop and show them
# ------------------------------------------------------------

if (
  length(
    cougar.matches
  ) > 1
) {
  
  cat(
    "\nMultiple possible Poisson cougar files were found:\n"
  )
  
  
  print(
    cougar.matches
  )
  
  
  stop(
    "More than one possible cougar source file was identified."
  )
}



# ------------------------------------------------------------
# 5. Set cougar source file
# ------------------------------------------------------------

cougar.file <- cougar.matches[1]



cat(
  "\n====================================================\n"
)

cat(
  "COUGAR SOURCE FILE\n"
)

cat(
  "====================================================\n"
)


cat(
  cougar.file,
  "\n"
)



# ------------------------------------------------------------
# 6. Load dataset
# ------------------------------------------------------------

cougar <- read.csv(
  cougar.file
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
    cougar
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
    cougar
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
    cougar
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

cougar.model.data <- cougar[
  ,
  required.variables
]



# ------------------------------------------------------------
# 12. Record rows before removing incomplete observations
# ------------------------------------------------------------

rows.before <- nrow(
  cougar.model.data
)



# ------------------------------------------------------------
# 13. Remove incomplete observations
# ------------------------------------------------------------

cougar.model.data <- cougar.model.data[
  complete.cases(
    cougar.model.data
  ),
]



# ------------------------------------------------------------
# 14. Record rows after filtering
# ------------------------------------------------------------

rows.after <- nrow(
  cougar.model.data
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

cougar.model.data$Cell_ID <- as.factor(
  cougar.model.data$Cell_ID
)


cougar.model.data$Road_Type <- as.factor(
  cougar.model.data$Road_Type
)


cougar.model.data$Habitat_Type <- as.factor(
  cougar.model.data$Habitat_Type
)


cougar.model.data$Land_Mgnt <- as.factor(
  cougar.model.data$Land_Mgnt
)



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COUGAR MODEL DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)



cat(
  "Camera-days:",
  nrow(
    cougar.model.data
  ),
  "\n"
)



cat(
  "Camera sites:",
  length(
    unique(
      cougar.model.data$Cell_ID
    )
  ),
  "\n"
)



cat(
  "Zero-detection days:",
  sum(
    cougar.model.data$Daily_Count == 0
  ),
  "\n"
)



cat(
  "Positive-detection days:",
  sum(
    cougar.model.data$Daily_Count > 0
  ),
  "\n"
)



cat(
  "Percent zero:",
  round(
    mean(
      cougar.model.data$Daily_Count == 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



cat(
  "Total detections:",
  sum(
    cougar.model.data$Daily_Count
  ),
  "\n"
)



cat(
  "Mean daily count:",
  round(
    mean(
      cougar.model.data$Daily_Count
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
    cougar.model.data$scale.smoke
  )
)



cat(
  "\nPM2.5 percentiles:\n"
)



print(
  quantile(
    cougar.model.data$scale.smoke,
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
  
  cougar.model.data,
  
  "data-split/cougar_model_data.csv",
  
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COUGAR PREPROCESSING COMPLETE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Source file:\n"
)


cat(
  cougar.file,
  "\n"
)


cat(
  "\nOutput file:\n"
)


cat(
  "data-split/cougar_model_data.csv\n"
)


cat(
  "\nFinal dimensions:",
  nrow(
    cougar.model.data
  ),
  "rows x",
  ncol(
    cougar.model.data
  ),
  "columns\n"
)


cat(
  "====================================================\n"
)