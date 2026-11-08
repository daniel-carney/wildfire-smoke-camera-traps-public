# ============================================================
# 30_preprocessing_coyote.R
#
# COYOTE DATA PREPARATION
#
# PURPOSE:
#
# Prepare the coyote daily camera-trap dataset for the
# Random Forest analyses.
#
# The output contains Daily_Count along with the environmental,
# habitat, camera, and site variables used in the current
# species-level modeling framework.
#
# ============================================================



# ------------------------------------------------------------
# 1. Load coyote source dataset
# ------------------------------------------------------------

coyote.file <- "data-tidy/poisson-data-coyote.csv"



cat(
  "\n====================================================\n"
)

cat(
  "COYOTE SOURCE FILE\n"
)

cat(
  "====================================================\n"
)


cat(
  coyote.file,
  "\n"
)



coyote <- read.csv(
  coyote.file
)



# ============================================================
# INITIAL DATA CHECK
# ============================================================



# ------------------------------------------------------------
# 2. Print original dimensions
# ------------------------------------------------------------

cat(
  "\nOriginal dimensions:\n"
)


print(
  dim(
    coyote
  )
)



# ------------------------------------------------------------
# 3. Print column names
# ------------------------------------------------------------

cat(
  "\nOriginal columns:\n"
)


print(
  names(
    coyote
  )
)



# ============================================================
# REQUIRED VARIABLES
# ============================================================



# ------------------------------------------------------------
# 4. Define variables required for current modeling pipeline
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
# 5. Check whether all required columns exist
# ------------------------------------------------------------

missing.variables <- setdiff(
  required.variables,
  names(
    coyote
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
# 6. Keep required variables
# ------------------------------------------------------------

coyote.model.data <- coyote[
  ,
  required.variables
]



# ------------------------------------------------------------
# 7. Remove incomplete observations
# ------------------------------------------------------------

rows.before <- nrow(
  coyote.model.data
)


coyote.model.data <- coyote.model.data[
  complete.cases(
    coyote.model.data
  ),
]


rows.after <- nrow(
  coyote.model.data
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
# 8. Convert categorical variables to factors
# ------------------------------------------------------------

coyote.model.data$Cell_ID <- as.factor(
  coyote.model.data$Cell_ID
)


coyote.model.data$Road_Type <- as.factor(
  coyote.model.data$Road_Type
)


coyote.model.data$Habitat_Type <- as.factor(
  coyote.model.data$Habitat_Type
)


coyote.model.data$Land_Mgnt <- as.factor(
  coyote.model.data$Land_Mgnt
)



# ============================================================
# DATA SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COYOTE MODEL DATA SUMMARY\n"
)

cat(
  "====================================================\n"
)



cat(
  "Camera-days:",
  nrow(
    coyote.model.data
  ),
  "\n"
)



cat(
  "Camera sites:",
  length(
    unique(
      coyote.model.data$Cell_ID
    )
  ),
  "\n"
)



cat(
  "Zero-detection days:",
  sum(
    coyote.model.data$Daily_Count == 0
  ),
  "\n"
)



cat(
  "Positive-detection days:",
  sum(
    coyote.model.data$Daily_Count > 0
  ),
  "\n"
)



cat(
  "Percent zero:",
  round(
    mean(
      coyote.model.data$Daily_Count == 0
    ) * 100,
    2
  ),
  "%",
  "\n"
)



cat(
  "Total detections:",
  sum(
    coyote.model.data$Daily_Count
  ),
  "\n"
)



cat(
  "Mean daily count:",
  round(
    mean(
      coyote.model.data$Daily_Count
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
    coyote.model.data$scale.smoke
  )
)



cat(
  "\nPM2.5 percentiles:\n"
)


print(
  quantile(
    coyote.model.data$scale.smoke,
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
# 9. Create output folder
# ------------------------------------------------------------

dir.create(
  "data-split",
  recursive = TRUE,
  showWarnings = FALSE
)



# ------------------------------------------------------------
# 10. Save model dataset
# ------------------------------------------------------------

write.csv(
  
  coyote.model.data,
  
  "data-split/coyote_model_data.csv",
  
  row.names = FALSE
)



# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n====================================================\n"
)

cat(
  "COYOTE PREPROCESSING COMPLETE\n"
)

cat(
  "====================================================\n"
)


cat(
  "Output file:\n"
)


cat(
  "data-split/coyote_model_data.csv\n"
)


cat(
  "\nFinal dimensions:",
  nrow(
    coyote.model.data
  ),
  "rows x",
  ncol(
    coyote.model.data
  ),
  "columns\n"
)


cat(
  "====================================================\n"
)