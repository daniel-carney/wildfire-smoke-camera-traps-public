# ============================================================
# 04_preprocessing_moose.R
#
# PURPOSE:
# Prepare the moose camera-trap dataset for Random Forest
# regression.
#
# Our main research question is:
#
# How is wildfire-attributable PM2.5 associated with the
# number of daily moose detections?
#
# This script DOES NOT fit the Random Forest yet.
#
# Its only job is to:
#   1. Load the original moose dataset
#   2. Remove variables that could leak the answer to the model
#   3. Remove redundant variables
#   4. Remove rows without important predictor values
#   5. Correctly format categorical variables
#   6. Save a clean modeling dataset
#
# Keeping preprocessing separate from modeling makes the
# analysis easier to understand, reproduce, and explain.
# ============================================================



# ------------------------------------------------------------
# 1. Load the ORIGINAL moose dataset
# ------------------------------------------------------------
#
# The original species datasets are stored in the data-tidy
# folder of the repository.
#
# We load the original file into an object called:
#
#     moose.original
#
# We will NEVER directly modify the original CSV file.

moose.original <- read.csv(
  "data-tidy/poisson-data-moose.csv"
)



# ------------------------------------------------------------
# 2. Inspect the original dataset
# ------------------------------------------------------------
#
# These commands allow us to make sure the file loaded
# correctly before we start changing anything.
#
# dim() shows:
#     number of rows
#     number of columns
#
# names() shows all variables.
#
# str() shows the data type of every variable.

dim(moose.original)

names(moose.original)

str(moose.original)



# ------------------------------------------------------------
# 3. Create a separate copy for preprocessing
# ------------------------------------------------------------
#
# Instead of modifying moose.original, we create:
#
#     moose.clean
#
# All preprocessing will happen to moose.clean.
#
# This means we can always return to the untouched original
# data if we make a mistake or want to change the analysis.

moose.clean <- moose.original



# ------------------------------------------------------------
# 4. Remove TARGET-LEAKAGE variables
# ------------------------------------------------------------
#
# Our target variable is:
#
#     Daily_Count
#
# Daily_Count tells us how many independent moose detections
# occurred at a camera on a particular day.
#
# The variables below contain information that is directly
# derived from the same animal detections.
#
# If we left them in the model, the Random Forest would be
# given information about the answer it is supposed to predict.
#
# This is called TARGET LEAKAGE.


# Detected equals 1 when an animal was detected and 0 otherwise.
#
# Because it is derived from Daily_Count, we cannot use it.

moose.clean$Detected <- NULL


# delta_times_pics is also derived from the detection events
# occurring on that day.
#
# It therefore contains information about our target.

moose.clean$delta_times_pics <- NULL


# npp was calculated using Daily_Count and detection-event
# information.
#
# It therefore directly contains information about the target.

moose.clean$npp <- NULL


# Date is problematic in the original construction of these
# data because it comes from the detection-event table.
#
# Missingness in Date can therefore reveal whether an animal
# was detected.
#
# We already have Year and julian to represent time safely.

moose.clean$Date <- NULL



# ------------------------------------------------------------
# 5. Remove REDUNDANT scaled variables
# ------------------------------------------------------------
#
# Several site variables appear twice:
#
#   one version in the original measurement units
#   one standardized/scaled version
#
# Random Forest does NOT require variables to be standardized.
#
# Therefore, keeping both versions would simply give the model
# duplicate information.
#
# We keep the easier-to-interpret original variables.


# Keep: elev
# Remove: scale.elev

moose.clean$scale.elev <- NULL


# Keep: Distance_to_Road
# Remove: scale.dtr

moose.clean$scale.dtr <- NULL


# Keep: Cam_Height
# Remove: scale.cheight

moose.clean$scale.cheight <- NULL


# Keep: wadnr
# Remove: scale.wadnr

moose.clean$scale.wadnr <- NULL


# Keep: julian
# Remove: scale.days
#
# julian represents where the observation falls within the
# July-October study season.
#
# This is useful because wildlife activity can naturally change
# during the season even if smoke does not change.

moose.clean$scale.days <- NULL



# ------------------------------------------------------------
# 6. Remove duplicate land-management variable
# ------------------------------------------------------------
#
# landmg2 contains the same information as Land_Mgnt.
#
# Keeping both would unnecessarily give the Random Forest the
# same information twice.
#
# We keep Land_Mgnt because its name is easier to understand.

moose.clean$landmg2 <- NULL



# ------------------------------------------------------------
# 7. Remove camera identifiers that will NOT be predictors
# ------------------------------------------------------------
#
# Camera_ID and CameraLocation identify individual cameras.
#
# We do NOT want the Random Forest to use these variables because
# it could learn that certain individual cameras tend to have
# more or fewer moose detections.
#
# That would encourage memorization rather than learning
# environmental relationships.

moose.clean$CameraLocation <- NULL

moose.clean$Camera_ID <- NULL


# IMPORTANT:
#
# We KEEP Cell_ID.
#
# Cell_ID identifies the physical camera site.
#
# Later, during model validation, Cell_ID will allow us to keep
# all observations from one physical site together.
#
# However, Cell_ID itself will NOT be used as a predictor.



# ------------------------------------------------------------
# 8. Remove rows without PM2.5 or temperature measurements
# ------------------------------------------------------------
#
# Our primary variable of interest is:
#
#     scale.smoke
#
# This represents wildfire-attributable PM2.5.
#
# Some camera-days do not have a smoke or temperature value.
#
# Because wildfire PM2.5 is the main variable we are studying,
# we do NOT want to invent or impute PM2.5 values.
#
# We therefore keep only observations where both:
#
#     scale.smoke
#     scale.temp
#
# are available.

moose.clean <- moose.clean[
  !is.na(moose.clean$scale.smoke) &
    !is.na(moose.clean$scale.temp),
]



# ------------------------------------------------------------
# 9. Convert categorical variables to factors
# ------------------------------------------------------------
#
# Some predictors represent categories rather than continuous
# numerical measurements.
#
# R uses the "factor" data type to tell the model that these
# values represent categories.


# Year represents the separate study years:
#
# 2018
# 2019
# 2020
#
# We treat these as categories rather than assuming that a
# numerical increase from 2018 to 2019 behaves like a continuous
# measurement.

moose.clean$Year <- as.factor(
  moose.clean$Year
)


# landcover contains land-cover classification codes.
#
# The numbers are category labels, not numerical quantities.

moose.clean$landcover <- as.factor(
  moose.clean$landcover
)


# Habitat_Type describes broad habitat categories.

moose.clean$Habitat_Type <- as.factor(
  moose.clean$Habitat_Type
)


# Road_Type describes whether the camera was associated with
# a road or trail.

moose.clean$Road_Type <- as.factor(
  moose.clean$Road_Type
)


# Land_Mgnt describes land-management categories such as
# public versus private land.

moose.clean$Land_Mgnt <- as.factor(
  moose.clean$Land_Mgnt
)


# conifer identifies whether the site is categorized as
# conifer or another habitat type.

moose.clean$conifer <- as.factor(
  moose.clean$conifer
)



# ------------------------------------------------------------
# 10. Remove any remaining rows with missing model variables
# ------------------------------------------------------------
#
# Earlier, we specifically removed observations missing:
#
#     scale.smoke
#     scale.temp
#
# After inspecting the moose dataset, we found that a small
# number of observations were also missing Habitat_Type.
#
# Random Forest models should be built from observations where
# all predictors and the target are available.
#
# Rather than guessing or imputing a habitat category, we use
# complete.cases() to remove rows with any remaining missing
# values.
#
# This gives us a simple rule that we can apply consistently
# across species:
#
#     The final modeling dataset contains complete observations
#     for every retained predictor and the target.


# Save the number of rows BEFORE removing incomplete cases.

rows.before.complete.case <- nrow(moose.clean)


# Keep only rows with no missing values in any retained column.

moose.clean <- moose.clean[
  complete.cases(moose.clean),
]


# Calculate how many rows were removed.

rows.removed.complete.case <-
  rows.before.complete.case - nrow(moose.clean)


cat(
  "\nRows removed because of remaining missing values:",
  rows.removed.complete.case,
  "\n"
)



# ------------------------------------------------------------
# 11. Verify that Daily_Count is an integer
# ------------------------------------------------------------
#
# Daily_Count is our TARGET.
#
# It represents the number of independent moose detections
# recorded at a camera during one day.
#
# Possible values include:
#
# 0, 1, 2, 3, ...
#
# It should therefore be stored as an integer.

moose.clean$Daily_Count <- as.integer(
  moose.clean$Daily_Count
)



# ------------------------------------------------------------
# 12. Inspect the CLEANED dataset
# ------------------------------------------------------------
#
# We now verify that preprocessing worked as expected.


# Number of rows and columns

dim(moose.clean)


# Names of retained variables

names(moose.clean)


# Data types of retained variables

str(moose.clean)



# ------------------------------------------------------------
# 13. Check for remaining missing values
# ------------------------------------------------------------
#
# Every value printed below should now be 0.
#
# This confirms that the final modeling dataset contains no
# missing values.

colSums(
  is.na(moose.clean)
)



# ------------------------------------------------------------
# 14. Examine the Daily_Count distribution
# ------------------------------------------------------------
#
# Wildlife camera-trap datasets contain many zero-detection
# days.
#
# Moose are especially sparse in this dataset, so it is
# important to understand how often detections actually occur.

summary(
  moose.clean$Daily_Count
)


# Show exactly how many camera-days had:
#
# 0 detections
# 1 detection
# 2 detections
# etc.

table(
  moose.clean$Daily_Count
)



# ------------------------------------------------------------
# 15. Calculate percentage of zero-detection days
# ------------------------------------------------------------
#
# This tells us what percentage of all usable camera-days had
# no moose detections.

zero.percent <- mean(
  moose.clean$Daily_Count == 0
) * 100


cat(
  "\nPercentage of zero-detection days:",
  round(zero.percent, 2),
  "%\n"
)



# ------------------------------------------------------------
# 16. Count days with at least one moose detection
# ------------------------------------------------------------
#
# This gives us the number of camera-days that contain actual
# positive detection information.

positive.days <- sum(
  moose.clean$Daily_Count > 0
)


cat(
  "Days with at least one moose detection:",
  positive.days,
  "\n"
)



# ------------------------------------------------------------
# 17. Count total moose detections
# ------------------------------------------------------------
#
# This sums Daily_Count across every camera-day in the cleaned
# dataset.

total.detections <- sum(
  moose.clean$Daily_Count
)


cat(
  "Total moose detections:",
  total.detections,
  "\n"
)



# ------------------------------------------------------------
# 18. Count physical camera sites
# ------------------------------------------------------------
#
# Cell_ID represents the physical camera site.
#
# These site IDs will later be used for grouped train/test
# validation so that one site cannot appear in both the
# training and testing data.

number.sites <- length(
  unique(moose.clean$Cell_ID)
)


cat(
  "Number of physical camera sites:",
  number.sites,
  "\n"
)



# ------------------------------------------------------------
# 19. Examine wildfire PM2.5 distribution
# ------------------------------------------------------------
#
# This gives us an idea of how frequently low, moderate, and
# extreme smoke conditions appear in the cleaned dataset.
#
# As with mule deer, we expect most observations to occur at
# low smoke levels, with relatively few observations during
# extreme smoke events.

summary(
  moose.clean$scale.smoke
)



# ------------------------------------------------------------
# 20. Save the cleaned moose modeling dataset
# ------------------------------------------------------------
#
# This creates a NEW CSV file:
#
#     data-split/moose_model_data.csv
#
# It does NOT overwrite:
#
#     data-tidy/poisson-data-moose.csv
#
# Our Random Forest script will load this cleaned file instead
# of repeating all preprocessing every time.

write.csv(
  moose.clean,
  "data-split/moose_model_data.csv",
  row.names = FALSE
)



# ------------------------------------------------------------
# 21. Confirm that the cleaned file was created
# ------------------------------------------------------------
#
# If everything worked correctly, this should print:
#
#     TRUE

file.exists(
  "data-split/moose_model_data.csv"
)