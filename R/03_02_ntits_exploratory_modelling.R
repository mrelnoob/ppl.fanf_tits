# ---------------------------------------------------------------------------------- #
#####                      Code for all exploratory modelling                    #####
# ---------------------------------------------------------------------------------- #

# The code of this R file is meant to wrap all exploratory modelling that were conducted after our
# formal inferential modelling phase. These "exploratory models" were built in order to try and fine
# tune some modelling components (e.g. zero-inflation) or to ascertain the robustness of our formal
# results by using different proxies for our variables of interest.


##### Rmd ?????? ----
##### Rmd ?????? ----
##### Rmd ?????? ----
##### Rmd ?????? ----
##### Rmd ?????? ----
##### Rmd ?????? ----
##### Rmd ?????? ----

is related to additional
# questions and perspectives that have, by nature, far less support as the data have
# already been used for formal testing (so type-I error rates aren't properly controlled for anymore)!





########################## *****************************************###############################
# ----------------------- #
##### 0. Preparation  #####
# ----------------------- #

library(magrittr) # The only library that I truly need to load (in order to be able to use pipes).
.pardefault <- par()


### Loading the data ___________________________________________________________
ntits <- readr::read_csv2(here::here("output", "tables", "ndata_final.csv"), col_names = TRUE,
                          col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                  id_patch = readr::col_factor(),
                                                  site = readr::col_factor(),
                                                  year = readr::col_factor(),
                                                  species = readr::col_factor(),
                                                  laying_date = readr::col_date(),
                                                  flight_date = readr::col_date(),
                                                  clutch_size = readr::col_integer(),
                                                  brood_size = readr::col_integer(),
                                                  fledgling_nb = readr::col_integer(),
                                                  manag_intensity = readr::col_factor(
                                                    ordered = TRUE,
                                                    levels = c("0", "1", "2"),
                                                    include_na = FALSE))) %>%
  dplyr::mutate(dplyr::across(where(is.matrix), as.numeric),
                dplyr::across(where(is.character), as.factor)) %>%
  dplyr::mutate(year = stats::relevel(x = year, ref = 3), # Assign 2019 as the reference group.
                species = stats::relevel(x = species, ref = 2)) %>% # Assign PM as the
  # reference group.
  dplyr::mutate(manag_low = ifelse(manag_intensity == "0", "1", "0"),
                manag_mid = ifelse(manag_intensity == "1", "1", "0"),
                manag_high = ifelse(manag_intensity == "2", "1", "0"))
# NOTE: I have to load the "tits" final dataset exported by 'tfinal_EDAta()' in order to avoid
# running the former function that is a bit long to run (but you can if you want).
ntits %>% dplyr::mutate(dplyr::across(where(is.character), as.factor)) -> ntits





########################## *****************************************###############################
# -------------------------------- #
##### 1. Modelling clutch size #####
# -------------------------------- #









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
