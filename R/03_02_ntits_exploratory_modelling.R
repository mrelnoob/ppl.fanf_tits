









###### EXPLORATORY MODELS ######
###### EXPLORATORY MODELS ######
###### EXPLORATORY MODELS ######
# For the exploratory models:
summary(ttFS_zibbin_glmm1h) # AIC = 1366.7 and both R2_glmm = 0.79.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), laying_day (--), and
# year2022 (+++), and also [urban_intensity (+), year2021 and year2022 (---) for the ZI component]!
## Almost significant variables: [min_t_between (+) for the ZI component]!
summary(ttFS_zibbin_glmm2h) # AIC = 1363.8 and both R2_glmm = 0.8.
## Significant variables: clutch_size (-), manag_high (--), light_pollution (-), laying_day (--)
# and year2022 (+++), and the INTERACTION EFFECT (--); and also [urban_intensity (+), year2021
# and year2022 (---) for the ZI component]!
## Almost significant variables: the F-metric (+) and also [min_t_between (+) for the ZI component]!
## Hypothesis 2 likely validated with a possible cross-over interaction!
# Diagnostics for these models were mostly ok, there was no outliers, deviations, dispersion or
# distributional problems.
# Predictions were fairly ok but the models still made too narrow predictions and failed to
# correctly predict total successes and failures.

# I also tried removing "traffic" and it didn't change things much. When we used the "F-metric"
# along with "urban_intensity" to model the ZI-part of the model, the effect of the "F-metric"
# utterly disappeared suggesting that connectivity does not influence total fledging failures and
# its effect in "F models" was actually a surrogate effect from "urban_intensity".

## Since the interaction effects turned out significant, we have to explore them graphically to
# interpret them!
