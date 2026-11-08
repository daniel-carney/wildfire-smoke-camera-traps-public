# ============================================================
# 26_preprocessing_white_tailed_deer.R
#
# WHITE-TAILED DEER DATA PREPARATION
#
# PURPOSE:
#
# Prepare the white-tailed deer daily camera-trap dataset for
# the Random Forest analyses.
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
# 2. Identify the white-tailed deer dataset
# ------------------------------------------------------------
#
# This searches for common filename forms such as:
#
# white-tailed
# white_tailed
# whitetail
# white tail
# wtd
#
# ------------------------------------------------------------

wtd.matches <- available.files[
  grepl(
    "white.*tail|whitetail|white_tailed|wtd",
    basename(
      available.files
    ),
    ignore.case = TRUE
  )
]



# ------------------------------------------------------------
# 3. Check that exactly one matching file was found
# ------------------------------------------------------------

if (
  length(
    wtd.matches
  ) == 0
) {
  
  cat(
    "\nNo white-tailed deer CSV was automatically identified.\n"
  )
  
  
  cat(
    "\nFiles currently available in data-tidy:\n"
  )
  
  
  print(
    basename(
      available.files
    )
  )
  
  
  stop(
    "White-tailed deer source file not found."
  )
}



if (
  length(
    wtd.matches
  ) > 1
) {
  
  cat(
    "\nMultiple possible white-tailed deer files were found:\n"
  )
  
  
  print(
    wtd.matches
  )
  
  
  stop(
    "More than one possible source file was identified."
  )
}



# ------------------------------------------------------------
# 4. Print detected source file
# ------------------------------------------------------------

wtd.file <- wtd.matches[1]


cat(
  "\n====================================================\n"
)

cat(
  "WHITE-TAILED DEER SOURCE FILE\n"
)

cat(
  "====================================================\n"
)


cat(
  wtd.file,
  "\n"
)



# ------------------------------------------------------------
# 5. Load dataset
# ------------------------------------------------------------

wtd <- read.csv(
  wtd.file
)



# ============================================================
# INITIAL DATA CHECK
# ============================================================



# ------------------------------------------------------------
# 6. Print original dimensions
# ------------------------------------------------------------

cat(
  "\nOriginal dimensions:\n"
)


print(
  dim(
    wtd
  )
)



# ------------------------------------------------------------
# 7. Print column names
# ------------------------------------------------------------

cat(
  "\nOriginal columns:\n"
)


print(
  names(
    wtd
  )
)



# ============================================================
# REQUIRED VARIABLES
# ============================================================



# ------------------------------------------------------------
# 8. Define variables required for current modeling pipeline
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
# 9. Check whether all required columns exist
# ------------------------------------------------------------

missing.variables <- setdiff(
  required.variables,
  names(
    wtd
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
# 10. Keep required variables
# ------------------------------------------------------------

wtd.model.data <- wtd[
  ,
  required.variables
]



# ------------------------------------------------------------
# 11. Remove incomplete observations
# ------------------------------------------------------------

rows.before <- nrow(
  wtd.model.data
)


wtd.model.data <- wtd.model.data[
  complete.cases(
    wtd.model.data
  ),
]


rows.after <- nrow(
  wtd.model.data
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
# 12. Convert categorical variables to factors
# ------------------------------------------------------------

wtd.model.data$Cell_ID <- as.factor(
  wtd.model.data$Cell_ID
)


wtd.model.data$Road_Type <- as.factor(
  wtd.model.data$Road_Type
)


wtd.model.data$Habitat_Type <- as.factor(
  wtd.model.data$Habitat_Type
)


wtd.model.data$Land_Mgnt <- as.factor(
  wtd.model.data$Land_Mgnt
)



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "WHITE-TAILED DEER MODEL DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)



cat(
  "Camera-days:",
  nrow(
    wtd.model.data
  ),
  "\n"
)



cat(
  "Camera sites:",
  length(
    unique(
      wtd.model.data$Cell_ID
    )
  ),
  "\n"
)



cat(
  "Zero-detection days:",
  sum(
    wtd.model.data$Daily_Count == 0
  ),
  "\n"
)



cat(
  "Positive-detection days:",
  sum(
    wtd.model.data$Daily_Count > 0
  ),
  "\n"
)



cat(
  "Percent zero:",
  round(
    mean(
      wtd.model.data$Daily_Count == 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



cat(
  "Total detections:",
  sum(
    wtd.model.data$Daily_Count
  ),
  "\n"
)



cat(
  "Mean daily count:",
  round(
    mean(
      wtd.model.data$Daily_Count
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
    wtd.model.data$scale.smoke
  )
)



cat(
  "\nPM2.5 percentiles:\n"
)


print(
  quantile(
    wtd.model.data$scale.smoke,
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
# 13. Create output folder
# ------------------------------------------------------------

dir.create(
  "data-split",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 14. Save model dataset
# ------------------------------------------------------------

write.csv(
  
  wtd.model.data,
  
  "data-split/white_tailed_deer_model_data.csv",
  
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "WHITE-TAILED DEER PREPROCESSING COMPLETE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Output file:\n"
)


cat(
  "data-split/white_tailed_deer_model_data.csv\n"
)


cat(
  "\nFinal dimensions:",
  nrow(
    wtd.model.data
  ),
  "rows x",
  ncol(
    wtd.model.data
  ),
  "columns\n"
)


cat(
  "====================================================\n"
)