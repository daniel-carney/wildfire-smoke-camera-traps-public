# ============================================================
# 01_preprocessing_mule_deer.R
#
# Prepare mule deer camera-trap data for Random Forest analysis
# of wildfire-attributable PM2.5 and daily detections.
# ============================================================


# ------------------------------------------------------------
# 1. Load original data
# ------------------------------------------------------------

mule.original <- read.csv(
  "data-tidy/poisson-data-mule.csv"
)


# Check original dataset
dim(mule.original)
names(mule.original)
str(mule.original)


# ------------------------------------------------------------
# 2. Create a copy for preprocessing
# ------------------------------------------------------------
# This ensures the original CSV is never modified.

mule.clean <- mule.original


# ------------------------------------------------------------
# 3. Remove target-leakage variables
# ------------------------------------------------------------
# These variables contain information derived from Daily_Count
# or from the animal detections themselves.

mule.clean$Detected <- NULL
mule.clean$delta_times_pics <- NULL
mule.clean$npp <- NULL
mule.clean$Date <- NULL


# ------------------------------------------------------------
# 4. Remove redundant scaled variables
# ------------------------------------------------------------
# Random Forest does not require predictors to be standardized.
# We keep the original versions because they are easier
# to interpret later.

mule.clean$scale.elev <- NULL
mule.clean$scale.dtr <- NULL
mule.clean$scale.cheight <- NULL
mule.clean$scale.wadnr <- NULL
mule.clean$scale.days <- NULL


# ------------------------------------------------------------
# 5. Remove duplicate land-management variable
# ------------------------------------------------------------
# landmg2 contains the same information as Land_Mgnt.

mule.clean$landmg2 <- NULL


# ------------------------------------------------------------
# 6. Remove identifier columns that will not be predictors
# ------------------------------------------------------------
# CameraLocation and Camera_ID identify individual cameras.
# They will not be used as predictors.
#
# IMPORTANT:
# Cell_ID is intentionally kept because it identifies the
# physical site and will later be used for grouped validation.

mule.clean$CameraLocation <- NULL
mule.clean$Camera_ID <- NULL


# ------------------------------------------------------------
# 7. Remove rows missing smoke or temperature
# ------------------------------------------------------------
# Wildfire-attributable PM2.5 is our main variable of interest,
# so we do not want to impute missing smoke values.
#
# We only retain rows where both smoke and temperature exist.

mule.clean <- mule.clean[
  !is.na(mule.clean$scale.smoke) &
    !is.na(mule.clean$scale.temp),
]


# ------------------------------------------------------------
# 8. Convert categorical variables to factors
# ------------------------------------------------------------

mule.clean$Year <- as.factor(mule.clean$Year)

mule.clean$landcover <- as.factor(mule.clean$landcover)

mule.clean$Habitat_Type <- as.factor(
  mule.clean$Habitat_Type
)

mule.clean$Road_Type <- as.factor(
  mule.clean$Road_Type
)

mule.clean$Land_Mgnt <- as.factor(
  mule.clean$Land_Mgnt
)

mule.clean$conifer <- as.factor(
  mule.clean$conifer
)


# ------------------------------------------------------------
# 9. Verify target variable
# ------------------------------------------------------------

mule.clean$Daily_Count <- as.integer(
  mule.clean$Daily_Count
)


# ------------------------------------------------------------
# 10. Inspect cleaned dataset
# ------------------------------------------------------------

dim(mule.clean)

names(mule.clean)

str(mule.clean)


# Check missing values
colSums(is.na(mule.clean))


# Check Daily_Count distribution
summary(mule.clean$Daily_Count)

table(mule.clean$Daily_Count)


# Percentage of days with no mule deer detections
mean(mule.clean$Daily_Count == 0) * 100


# Number of days with at least one detection
sum(mule.clean$Daily_Count > 0)


# Total number of detections
sum(mule.clean$Daily_Count)


# Number of unique physical camera sites
length(unique(mule.clean$Cell_ID))


# Check wildfire PM2.5 distribution
summary(mule.clean$scale.smoke)


# ------------------------------------------------------------
# 11. Save cleaned modeling dataset
# ------------------------------------------------------------
# This creates a NEW file.
# The original poisson-data-mule.csv is not modified.

write.csv(
  mule.clean,
  "data-split/mule_deer_model_data.csv",
  row.names = FALSE
)


# Confirm that the new file was created
file.exists(
  "data-split/mule_deer_model_data.csv"
)