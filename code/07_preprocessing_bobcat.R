# ============================================================
# 07_preprocessing_bobcat.R
#
# PURPOSE:
# Prepare the bobcat camera-trap dataset for Random Forest
# regression.
#
# MAIN RESEARCH QUESTION:
#
# How is wildfire-attributable PM2.5 associated with the
# number of daily bobcat detections?
#
#
# WHY BOBCAT IS IMPORTANT:
#
# The parent paper reported that bobcat detections tended to
# decrease as wildfire PM2.5 increased.
#
# We are NOT changing our method to try to reproduce that
# result.
#
# Instead, we intentionally use the SAME preprocessing and
# modeling structure used for:
#
#   1. Mule deer
#   2. Moose
#   3. Bobcat
#
# This makes bobcat a useful test of whether our Random Forest
# pipeline is capable of learning a negative PM2.5 relationship.
#
#
# THIS SCRIPT DOES NOT FIT THE RANDOM FOREST.
#
# Its job is only to:
#
#   1. Load the original bobcat dataset
#   2. Remove target-leakage variables
#   3. Remove redundant variables
#   4. Remove identifiers that should not be predictors
#   5. Remove observations with missing model information
#   6. Convert categorical variables to factors
#   7. Inspect the cleaned bobcat dataset
#   8. Save the cleaned modeling dataset
#
# ============================================================



# ------------------------------------------------------------
# 1. Load the ORIGINAL bobcat dataset
# ------------------------------------------------------------
#
# The original bobcat dataset is stored in:
#
#     data-tidy/poisson-data-bobcat.csv
#
# We load this file into bobcat.original.
#
# We will NOT modify the original CSV file.

bobcat.original <- read.csv(
  "data-tidy/poisson-data-bobcat.csv"
)



# ------------------------------------------------------------
# 2. Inspect the original dataset
# ------------------------------------------------------------
#
# Before changing anything, we inspect the structure of the
# original file.
#
# dim():
#   Shows number of rows and columns.
#
# names():
#   Shows all variable names.
#
# str():
#   Shows the type of each variable.

dim(
  bobcat.original
)


names(
  bobcat.original
)


str(
  bobcat.original
)



# ------------------------------------------------------------
# 3. Create a separate working copy
# ------------------------------------------------------------
#
# We keep bobcat.original untouched.
#
# All preprocessing will happen to:
#
#     bobcat.clean
#
# This gives us an untouched reference if we ever need to
# restart or change preprocessing later.

bobcat.clean <- bobcat.original



# ============================================================
# REMOVE VARIABLES THAT SHOULD NOT BE MODEL PREDICTORS
# ============================================================



# ------------------------------------------------------------
# 4. Remove TARGET-LEAKAGE variables
# ------------------------------------------------------------
#
# Our target is:
#
#     Daily_Count
#
# Daily_Count represents the number of independent bobcat
# detections recorded at a camera during one day.
#
# Some variables in the original dataset are created directly
# from those same detection records.
#
# If we kept them, the model would indirectly receive
# information about the answer it is trying to predict.
#
# This is called TARGET LEAKAGE.



# ------------------------------------------------------------
# Remove Detected
# ------------------------------------------------------------
#
# Detected indicates whether at least one bobcat was detected:
#
#     0 = no detection
#     1 = detection
#
# Because this is directly derived from Daily_Count, it cannot
# be used to predict Daily_Count.

bobcat.clean$Detected <- NULL



# ------------------------------------------------------------
# Remove delta_times_pics
# ------------------------------------------------------------
#
# This variable is also derived from the detection records.
#
# It therefore contains information closely connected to the
# response variable.

bobcat.clean$delta_times_pics <- NULL



# ------------------------------------------------------------
# Remove npp
# ------------------------------------------------------------
#
# npp is calculated using detection-event information.
#
# It therefore should not be included as an independent
# environmental predictor of Daily_Count.

bobcat.clean$npp <- NULL



# ------------------------------------------------------------
# Remove Date
# ------------------------------------------------------------
#
# Date is problematic because it was created through the
# detection-event data.
#
# Missingness in Date can therefore reveal whether a bobcat was
# detected on that camera-day.
#
# We already retain:
#
#     Year
#     julian
#
# which safely represent temporal information.

bobcat.clean$Date <- NULL



# ------------------------------------------------------------
# 5. Remove redundant scaled site variables
# ------------------------------------------------------------
#
# Several environmental/site variables appear twice:
#
#   - one version in its original units
#   - one standardized version
#
# Random Forest models do NOT require standardized predictors.
#
# Keeping both versions would give the model duplicate
# information.
#
# We therefore keep the easier-to-interpret original variable.



# Keep elev
# Remove scale.elev

bobcat.clean$scale.elev <- NULL



# Keep Distance_to_Road
# Remove scale.dtr

bobcat.clean$scale.dtr <- NULL



# Keep Cam_Height
# Remove scale.cheight

bobcat.clean$scale.cheight <- NULL



# Keep wadnr
# Remove scale.wadnr

bobcat.clean$scale.wadnr <- NULL



# Keep julian
# Remove scale.days
#
# julian tells us where a day falls within the July-October
# study season.
#
# This is useful because wildlife activity may change through
# the season even without smoke.

bobcat.clean$scale.days <- NULL



# ------------------------------------------------------------
# 6. Remove duplicate land-management information
# ------------------------------------------------------------
#
# landmg2 duplicates Land_Mgnt.
#
# We keep Land_Mgnt because it is easier to understand and
# interpret.

bobcat.clean$landmg2 <- NULL



# ------------------------------------------------------------
# 7. Remove camera identifiers that should NOT be predictors
# ------------------------------------------------------------
#
# CameraLocation and Camera_ID identify individual cameras.
#
# We do not want the Random Forest to memorize that certain
# cameras tend to record more or fewer bobcats.
#
# We therefore remove these as predictors.

bobcat.clean$CameraLocation <- NULL

bobcat.clean$Camera_ID <- NULL



# ------------------------------------------------------------
# IMPORTANT: Keep Cell_ID
# ------------------------------------------------------------
#
# Cell_ID identifies the physical camera site.
#
# We keep it temporarily because we will use it later to perform
# grouped train/test validation.
#
# All observations from a physical site should remain together
# during model validation.
#
# Cell_ID itself will be removed before fitting the model.



# ============================================================
# HANDLE MISSING DATA
# ============================================================



# ------------------------------------------------------------
# 8. Remove rows missing PM2.5 or temperature
# ------------------------------------------------------------
#
# Our primary variable of interest is:
#
#     scale.smoke
#
# This represents wildfire-attributable PM2.5.
#
# Because smoke is the main exposure being studied, we do not
# want to guess or impute missing smoke values.
#
# We also require scale.temp because temperature is an important
# environmental control variable.
#
# Therefore, rows missing either variable are removed.

bobcat.clean <- bobcat.clean[
  !is.na(bobcat.clean$scale.smoke) &
    !is.na(bobcat.clean$scale.temp),
]



# ============================================================
# CONVERT CATEGORICAL VARIABLES
# ============================================================



# ------------------------------------------------------------
# 9. Convert Year to a factor
# ------------------------------------------------------------
#
# Year represents study years such as:
#
#     2018
#     2019
#     2020
#
# We treat these as categories rather than as a continuous
# numerical measurement.

bobcat.clean$Year <- as.factor(
  bobcat.clean$Year
)



# ------------------------------------------------------------
# 10. Convert landcover to a factor
# ------------------------------------------------------------
#
# landcover contains numerical category codes.
#
# A value such as 42 is a category label, not a meaningful
# numerical quantity.

bobcat.clean$landcover <- as.factor(
  bobcat.clean$landcover
)



# ------------------------------------------------------------
# 11. Convert Habitat_Type to a factor
# ------------------------------------------------------------

bobcat.clean$Habitat_Type <- as.factor(
  bobcat.clean$Habitat_Type
)



# ------------------------------------------------------------
# 12. Convert Road_Type to a factor
# ------------------------------------------------------------

bobcat.clean$Road_Type <- as.factor(
  bobcat.clean$Road_Type
)



# ------------------------------------------------------------
# 13. Convert Land_Mgnt to a factor
# ------------------------------------------------------------

bobcat.clean$Land_Mgnt <- as.factor(
  bobcat.clean$Land_Mgnt
)



# ------------------------------------------------------------
# 14. Convert conifer to a factor
# ------------------------------------------------------------

bobcat.clean$conifer <- as.factor(
  bobcat.clean$conifer
)



# ------------------------------------------------------------
# 15. Remove any remaining rows with missing model variables
# ------------------------------------------------------------
#
# After specifically removing missing smoke and temperature
# values, another retained predictor may still have some missing
# observations.
#
# We use complete.cases() to ensure that the final modeling
# dataset contains complete information for:
#
#   every retained predictor
#
# and
#
#   Daily_Count
#
#
# We do NOT impute categorical habitat/site characteristics
# because that would require us to guess their values.
#
# We also record how many rows are removed so the preprocessing
# remains transparent.

rows.before.complete.case <- nrow(
  bobcat.clean
)


bobcat.clean <- bobcat.clean[
  complete.cases(bobcat.clean),
]


rows.removed.complete.case <-
  rows.before.complete.case -
  nrow(bobcat.clean)


cat(
  "\nRows removed because of remaining missing values:",
  rows.removed.complete.case,
  "\n"
)



# ------------------------------------------------------------
# 16. Ensure Daily_Count is stored as an integer
# ------------------------------------------------------------
#
# Daily_Count is a count variable.
#
# It represents:
#
#     0 bobcat detections
#     1 bobcat detection
#     2 bobcat detections
#     ...
#
# Therefore, integer storage is appropriate.

bobcat.clean$Daily_Count <- as.integer(
  bobcat.clean$Daily_Count
)



# ============================================================
# CHECK THE FINAL DATASET
# ============================================================



# ------------------------------------------------------------
# 17. Check the dimensions of the cleaned dataset
# ------------------------------------------------------------

dim(
  bobcat.clean
)



# ------------------------------------------------------------
# 18. Check retained column names
# ------------------------------------------------------------
#
# We expect the same 15 retained columns used for mule deer and
# moose.

names(
  bobcat.clean
)



# ------------------------------------------------------------
# 19. Check the structure of the cleaned dataset
# ------------------------------------------------------------

str(
  bobcat.clean
)



# ------------------------------------------------------------
# 20. Check for remaining missing values
# ------------------------------------------------------------
#
# Every result below should ideally equal:
#
#     0

colSums(
  is.na(bobcat.clean)
)



# ============================================================
# INSPECT THE BOBCAT DETECTION DATA
# ============================================================



# ------------------------------------------------------------
# 21. Summarize Daily_Count
# ------------------------------------------------------------
#
# Bobcats are relatively rare in this camera-trap dataset.
#
# We therefore expect:
#
#   - many zero-detection days
#   - relatively few positive detection days

summary(
  bobcat.clean$Daily_Count
)



# ------------------------------------------------------------
# 22. Show exact Daily_Count frequencies
# ------------------------------------------------------------
#
# This tells us how many camera-days had:
#
# 0 detections
# 1 detection
# 2 detections
# etc.

table(
  bobcat.clean$Daily_Count
)



# ------------------------------------------------------------
# 23. Calculate percentage of zero-detection days
# ------------------------------------------------------------
#
# This gives us a direct measure of how sparse the bobcat
# dataset is.

zero.percent <- mean(
  bobcat.clean$Daily_Count == 0
) * 100


cat(
  "\nPercentage of zero-detection days:",
  round(
    zero.percent,
    2
  ),
  "%\n"
)



# ------------------------------------------------------------
# 24. Count positive bobcat camera-days
# ------------------------------------------------------------
#
# Positive camera-days contain at least one bobcat detection.

positive.days <- sum(
  bobcat.clean$Daily_Count > 0
)


cat(
  "Days with at least one bobcat detection:",
  positive.days,
  "\n"
)



# ------------------------------------------------------------
# 25. Count total bobcat detections
# ------------------------------------------------------------
#
# This sums all Daily_Count values.

total.detections <- sum(
  bobcat.clean$Daily_Count
)


cat(
  "Total bobcat detections:",
  total.detections,
  "\n"
)



# ------------------------------------------------------------
# 26. Count physical camera sites
# ------------------------------------------------------------
#
# Cell_ID represents the physical camera site.
#
# We will later use these IDs for grouped train/test splitting.

number.sites <- length(
  unique(
    bobcat.clean$Cell_ID
  )
)


cat(
  "Number of physical camera sites:",
  number.sites,
  "\n"
)



# ============================================================
# INSPECT THE WILDFIRE PM2.5 DATA
# ============================================================



# ------------------------------------------------------------
# 27. Summarize scale.smoke
# ------------------------------------------------------------
#
# This tells us the range and distribution of wildfire-
# attributable PM2.5 values in the cleaned bobcat dataset.
#
# Based on the previous species, we expect:
#
#   - many observations near low smoke
#   - relatively few observations at extreme smoke levels

summary(
  bobcat.clean$scale.smoke
)



# ============================================================
# SAVE THE CLEANED DATA
# ============================================================



# ------------------------------------------------------------
# 28. Save the cleaned bobcat modeling dataset
# ------------------------------------------------------------
#
# This creates:
#
#     data-split/bobcat_model_data.csv
#
# The original file remains unchanged:
#
#     data-tidy/poisson-data-bobcat.csv

write.csv(
  bobcat.clean,
  "data-split/bobcat_model_data.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 29. Confirm that the file was successfully created
# ------------------------------------------------------------
#
# This should return:
#
#     TRUE

file.exists(
  "data-split/bobcat_model_data.csv"
)