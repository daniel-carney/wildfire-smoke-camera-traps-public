# Wildfire Smoke Camera-Trap Analysis

## Project Overview

This project examines how wildfire-attributable PM2.5 is associated with daily wildlife detections from camera traps in eastern Washington. The final analysis uses the same Random Forest framework for each species so that results can be compared consistently.

The final modeling setup uses:

* Random Forest regression with the `ranger` package
* 500 trees
* `mtry = 3`
* minimum node size = 20
* Poisson splitting rule
* grouped train/test/validation splitting by camera site
* Accumulated Local Effects (ALE) for PM2.5 interpretation
* 30-run PM2.5 permutation testing

## Before Running the Code

1. Download or clone this repository.
2. Open RStudio.
3. Set the repository root as the working directory.
4. Make sure the `data-tidy`, `data-split`, `code`, and `results` folders are present.
5. Install the required R packages if needed:

```r
install.packages("ranger")
install.packages("ggplot2")
```

The scripts use relative file paths, so they should be run with the repository root as the working directory.

\---

# Final Script Order by Species

Only the scripts listed below are part of the final analysis workflow. Older scripts in the repository document earlier development and testing and do not need to be run to reproduce the final results.

## Mule Deer

Run these scripts in this order:

1. `01\_preprocessing\_mule\_deer.R`
2. `20\_random\_forest\_daily\_count\_mule\_deer.R`
3. `21\_mule\_deer\_daily\_count\_pm25\_ale.R`
4. `22\_mule\_deer\_pm25\_daily\_count\_permutation.R`

The preprocessing script creates:

`data-split/mule\_deer\_model\_data.csv`

The ALE script creates the final mule deer PM2.5 figures and summary files, including:

* `results/Mule\_Deer\_Daily\_Count\_PM25\_ALE.png`
* `results/Mule\_Deer\_Observed\_Daily\_Counts\_By\_PM25.png`

Expected final mule deer ALE summary:

* Overall ALE change: approximately `0.088361`
* Upper-smoke ALE change: approximately `0.057271`
* Overall permutation p-value: approximately `0.2258`
* Upper-smoke permutation p-value: approximately `0.4194`

\---

## Moose

Run these scripts in this order:

1. `04\_preprocessing\_moose.R`
2. `17\_random\_forest\_daily\_count\_moose.R`
3. `18\_moose\_daily\_count\_pm25\_ale.R`
4. `19\_moose\_daily\_count\_pm25\_permutation.R`

The ALE script creates:

* `results/Moose\_Daily\_Count\_PM25\_ALE.png`
* `results/Moose\_Observed\_Daily\_Counts\_By\_PM25.png`

Expected overall ALE change:

* approximately `0.011196`

\---

## Bobcat

Run these scripts in this order:

1. `07\_preprocessing\_bobcat.R`
2. `23\_random\_forest\_daily\_count\_bobcat.R`
3. `24\_bobcat\_daily\_count\_pm25\_ale.R`
4. `25\_bobcat\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.003146`

\---

## White-Tailed Deer

Run these scripts in this order:

1. `26\_preprocessing\_white\_tailed\_deer.R`
2. `27\_random\_forest\_daily\_count\_white\_tailed\_deer.R`
3. `28\_white\_tailed\_deer\_daily\_count\_pm25\_ale.R`
4. `29\_white\_tailed\_deer\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.011201`

\---

## Coyote

Run these scripts in this order:

1. `30\_preprocessing\_coyote.R`
2. `31\_random\_forest\_daily\_count\_coyote.R`
3. `32\_coyote\_daily\_count\_pm25\_ale.R`
4. `33\_coyote\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.015290`

\---

## Black Bear

Run these scripts in this order:

1. `34\_preprocessing\_black\_bear.R`
2. `35\_random\_forest\_daily\_count\_black\_bear.R`
3. `36\_black\_bear\_daily\_count\_pm25\_ale.R`
4. `37\_black\_bear\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.008661`

\---

## Elk

Run these scripts in this order:

1. `38\_preprocessing\_elk.R`
2. `39\_random\_forest\_daily\_count\_elk.R`
3. `40\_elk\_daily\_count\_pm25\_ale.R`
4. `41\_elk\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.009890`

\---

## Cougar

Run these scripts in this order:

1. `42\_preprocessing\_cougar.R`
2. `43\_random\_forest\_daily\_count\_cougar.R`
3. `44\_cougar\_daily\_count\_pm25\_ale.R`
4. `45\_cougar\_daily\_count\_pm25\_permutation.R`

Expected overall ALE change:

* approximately `0.008090`

\---

## Important Reproducibility Notes

* Do not change the random seeds used in the scripts.
* Do not change the Random Forest settings.
* Run the scripts from the repository root so the relative file paths work correctly.
* Zero-detection camera-days are valid observations and are retained unless another required model variable is missing.
* `scale.smoke` is standardized wildfire-attributable PM2.5. Negative values indicate below-average PM2.5, not negative smoke concentrations.
* The ALE plots represent modeled associations and do not establish causation.

## Final Analysis Files

The scripts listed in the species sections above are the final scripts used for the research paper. Other scripts may remain in the repository because they document earlier model development, testing, and alternative approaches. Those older files are not required to reproduce the final reported results.

