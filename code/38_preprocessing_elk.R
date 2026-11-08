# ============================================================
# 38_preprocessing_elk.R
#
# ELK DATA PREPARATION
#
# PURPOSE:
#
# Prepare the elk daily camera-trap dataset for the
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
# 2. Look specifically for Poisson elk data
# ------------------------------------------------------------

elk.matches <- available.files[
  grepl(
    "poisson-data",
    basename(
      available.files
    ),
    ignore.case = TRUE
  ) &
    grepl(
      "elk",
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
    elk.matches
  ) == 0
) {
  
  cat(
    "\nNo Poisson elk CSV was automatically identified.\n"
  )
  
  
  cat(
    "\nFiles containing the word 'elk':\n"
  )
  
  
  print(
    basename(
      available.files[
        grepl(
          "elk",
          basename(
            available.files
          ),
          ignore.case = TRUE
        )
      ]
    )
  )
  
  
  stop(
    "Elk source file not found."
  )
}



# ------------------------------------------------------------
# 4. If multiple Poisson elk files exist, stop and show them
# ------------------------------------------------------------

if (
  length(
    elk.matches
  ) > 1
) {
  
  cat(
    "\nMultiple possible Poisson elk files were found:\n"
  )
  
  
  print(
    elk.matches
  )
  
  
  stop(
    "More than one possible elk source file was identified."
  )
}



# ------------------------------------------------------------
# 5. Set elk source file
# ------------------------------------------------------------

elk.file <- elk.matches[1]



cat(
  "\n====================================================\n"
)

cat(
  "ELK SOURCE FILE\n"
)

cat(
  "====================================================\n"
)


cat(
  elk.file,
  "\n"
)



# ------------------------------------------------------------
# 6. Load dataset
# ------------------------------------------------------------

elk <- read.csv(
  elk.file
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
    elk
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
    elk
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
    elk
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

elk.model.data <- elk[
  ,
  required.variables
]



# ------------------------------------------------------------
# 12. Record rows before removing incomplete observations
# ------------------------------------------------------------

rows.before <- nrow(
  elk.model.data
)



# ------------------------------------------------------------
# 13. Remove incomplete observations
# ------------------------------------------------------------

elk.model.data <- elk.model.data[
  complete.cases(
    elk.model.data
  ),
]



# ------------------------------------------------------------
# 14. Record rows after filtering
# ------------------------------------------------------------

rows.after <- nrow(
  elk.model.data
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

elk.model.data$Cell_ID <- as.factor(
  elk.model.data$Cell_ID
)


elk.model.data$Road_Type <- as.factor(
  elk.model.data$Road_Type
)


elk.model.data$Habitat_Type <- as.factor(
  elk.model.data$Habitat_Type
)


elk.model.data$Land_Mgnt <- as.factor(
  elk.model.data$Land_Mgnt
)



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "ELK MODEL DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)



cat(
  "Camera-days:",
  nrow(
    elk.model.data
  ),
  "\n"
)



cat(
  "Camera sites:",
  length(
    unique(
      elk.model.data$Cell_ID
    )
  ),
  "\n"
)



cat(
  "Zero-detection days:",
  sum(
    elk.model.data$Daily_Count == 0
  ),
  "\n"
)



cat(
  "Positive-detection days:",
  sum(
    elk.model.data$Daily_Count > 0
  ),
  "\n"
)



cat(
  "Percent zero:",
  round(
    mean(
      elk.model.data$Daily_Count == 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



cat(
  "Total detections:",
  sum(
    elk.model.data$Daily_Count
  ),
  "\n"
)



cat(
  "Mean daily count:",
  round(
    mean(
      elk.model.data$Daily_Count
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
    elk.model.data$scale.smoke
  )
)



cat(
  "\nPM2.5 percentiles:\n"
)



print(
  quantile(
    elk.model.data$scale.smoke,
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
  
  elk.model.data,
  
  "data-split/elk_model_data.csv",
  
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "ELK PREPROCESSING COMPLETE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Source file:\n"
)


cat(
  elk.file,
  "\n"
)


cat(
  "\nOutput file:\n"
)


cat(
  "data-split/elk_model_data.csv\n"
)


cat(
  "\nFinal dimensions:",
  nrow(
    elk.model.data
  ),
  "rows x",
  ncol(
    elk.model.data
  ),
  "columns\n"
)


cat(
  "====================================================\n"
)