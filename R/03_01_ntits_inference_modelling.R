# ---------------------------------------------------------------------------------- #
#####                      Code for all inferential modelling                    #####
# ---------------------------------------------------------------------------------- #

# The code of this R file is meant to wrap all formal inference modelling related to both species
# together. We have indeed pooled together the observations of both species to increase the overall
# sample size and enable more robust inference. Here, we speak about "formal inference modelling"
# as opposed to "exploratory data (EDA) modelling" (cf. previous scripts) and "exploratory
# modelling" that was performed afterwards; i.e. after we have formally tested our research
# hypotheses in a robust inferential framework. The so-called "exploratory modelling" is related
# to additional questions and perspectives that have, by nature, far less support as the data have
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

##### * 1.1. Clutch size: COM-Poisson GLMM -------------------------------------
# ---------------------------------------------------------------------------- #
### ** 1.1.1. Initial model fit ----
# __________________________________

## To remove probable outliers:
ntits3 <- ntits[-c(111,156,170,181,210,227,326,362,374,379),] # The very first diagnostics I ran
# on the models that included these observations suggested that they were true outliers (i.e. generated
# by another process than the one investigated here, such as predation, parental abandonment, etc.).
# So I remove them, but feel free to keep them and check the diagnostcis yourselves.


## Fitting a regular Poisson regression:
ttCy_glm1 <- stats::glm(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                          urban_intensity + manag_mid + manag_high +
                          light_pollution + noise_m + traffic +
                          cumdd_30 + laying_day + year,
                        data = ntits3, family = "poisson")

## Fitting a regular Poisson GLMM:
ttCy_glmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                 urban_intensity + manag_mid + manag_high +
                                 light_pollution + noise_m + traffic +
                                 cumdd_30 + laying_day + year + (1|id_nestbox),
                               data = ntits3, family = "poisson")


## Fitting a regular Conway-Maxwell (COM) Poisson regression (GLM):
ttCy_comglm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_mid + manag_high +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + laying_day + year,
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Intercept only 'nu' (default).
# # OR:
# ttCy_comglm1b <- COMPoissonReg::glm.cmp(formula.lambda =
#                                           clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                           urban_intensity + manag_mid + manag_high +
#                                           light_pollution + noise_m + traffic +
#                                           cumdd_30 + laying_day + year,
#                                      data = ntits3, formula.nu = ~1) # Intercept only 'nu' (default).

## Fitting a regular Conway-Maxwell (COM) Poisson mixed model (GLMM):
ttCy_comglmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                    urban_intensity + manag_mid + manag_high +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + laying_day + year + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.

## Fitting the interaction model (COM-Poisson GLMM):
ttCy_comglmm2 <- glmmTMB::glmmTMB(clutch_size ~
                                    c.log_patch_area * c.log_F_metric_d2b1 + species +
                                    urban_intensity + manag_mid + manag_high +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + laying_day + year + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.
summary(ttCy_glm1) # AIC = 1707.9.
summary(ttCy_glmm1) # AIC = 1709.9.
summary(ttCy_comglm1) # AIC = 1384.8.
summary(ttCy_comglmm1) # AIC = 1384.
summary(ttCy_comglmm2) # AIC = 1386.
# It thus seems that the interaction worsen the fit!
# Overall, if the inclusion of a random effect (RE) did not seem to help the models much, accounting
# for the likely underdispersion quite strongly improved the fit! Interestingly, including
# "laying_day" quite strongly improves the model while the interactive model does not (suggesting
# that our proposed interaction is not supported by our data).
# I will thus carry on with 'ttCy_comglmm1' to the diagnostic part and assess whether the use of
# the RE is truly justified or not and if the model behaves as expected.





### ** 1.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 1.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttCy_comglmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Ok.
# performance::check_outliers(ttCy_comglmm1) # Does not work for this type of model.
ntits3[which(resid < -3),] # Lowest residuals are nestboxes with very small clutch sizes.

# To further investigate patterns, I can plot the residuals against all predictors:
plot(x = ntits3$year, y = resid) # Seems rather ok.
# plot(ttCy_comglmm1b, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttCy_comglmm1b, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttCy_comglmm1, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and
# only works for {lme4} formulations). It is useful for testing dispersion (see below) but can be
# omitted eventually.
plot(simu.resid) # Ok-ish but some slight (non-significant) deviations exist.
DHARMa::outliers(simu.resid) # No potential outliers.
# ntits2[c(156,170,181,210,227,314,362,367),] # [Note made before removing the outliers - see
# beginning of the script]. They have surprisingly low clutch sizes with regards to their locations
# and their adjacent observations. They may well be true outliers (whose clutch sizes are function
# of other processes than those investigated here)[hence their removal].

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok.
# There's a slight spatial autocorrelation according to Moran's I, but it is expected as we ARE
# truly modelling connectivity (that is = spatial autocorrelation). Incidentally, adding a "site" RE
# doesn't give better results.
performance::check_autocorrelation(ttCy_comglmm1) # Ok.
performance::check_collinearity(ttCy_comglmm1) # Ok-ish, but "Fmetric" > 4.
stats::vcov(ttCy_comglmm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = ntits3$laying_day)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)



### *** 1.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = ttCy_glm1, alternative = c("less")) # Significant underdispersion (note
# that this is the Poisson model that is tested, hence the later use of a COM-Poisson distribution)!
# So it is fine.
DHARMa::testDispersion(simu.resid, alternative = "less") # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rcmp(n = nrow(ntits3), lambda = mean(ntits3$clutch_size), nu = 1.05)
# The 'nu' parameter should be chosen by trial-and-errors.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(ntits3, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter
# nu~1.1!

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = ttCy_comglmm1, type = "response") # Extract the predicted
# counts.
par(mfrow= c(1,2))
hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
hist(ntits3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The model
# predictions are very similar and relatively acceptable (although too narrow). The difference is
# likely due to the fact that we pooled species together.



### *** 1.1.2.3. Linearity ----
# For the sake of further exploration, I also plot variants of our predictors:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_30, laying_day, min_t_before) -> mydata
predictors <- colnames(mydata)
# Bind log(Y) and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(log_y = log(ntits3$clutch_size)) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.



### *** 1.1.2.4. Model goodness-of-fit (GOF) and performances ----
# GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttCy_comglmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttCy_comglmm1)) # p = 0.84, indicating that there
# is no significant lack of fit. Keep in mind though that GOF measures for mixed models are an
# extremely complicated topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(ttCy_comglmm1) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.12.
# NOTE: it does not work if we tune the dispersion parameter of the COM-Poisson model.

## Likelihood-based evaluation of effects inclusion:
# For the "site" random-effects (RE):
ttCy_comglmm1ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                      urban_intensity + manag_mid + manag_high +
                                      light_pollution + noise_m + traffic +
                                      cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
                                    data = ntits3, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1) # Rather long to fit (~3-4 min)!
summary(ttCy_comglmm1ac) # AIC = 1383.1.
# The non-mixed model gives AIC = 1384.8, so very similar to the mixed-model (AIC = 1384) with only
# "id_nestbox" as RE. The one with both RE gives AIC = 1383.1, so it seems like the use of a mixed
# model is not truly supported by the data.

## For the whole model:
ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1)
res.LRT_null <- stats::anova(object = ttCy_comglmm0, ttCy_comglmm1, test = "LRT")
# The test is significant, confirming that the model is useful to explain the data.



### *** 1.1.2.5. Posterior predictive simulations ----
# Predicted counts:
par(.pardefault)
obsprop <- prop.table(table(ntits3$clutch_size))
sims <- stats::simulate(ttCy_comglmm1, nsim = 1000)
nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim4)),
     ylab="Probability", xlab="Number of fours (true == 1)")
(obs4 <- sum(ntits3$clutch_size == 4))
points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!

nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim9)),
     ylab="Probability", xlab="Number of nines (true == 86)")
(obs9 <- sum(ntits3$clutch_size == 9))
points(obs9, 0.22, col="red", pch=16, cex=2)

nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
par(las=1,bty="l")
plot(pt <- prop.table(table(nsim14)),
     ylab="Probability", xlab="Number of fourteens (true == 5)")
(obs14 <- sum(ntits3$clutch_size == 14))
points(obs14, 0.013, col="red", pch=16, cex=2)
# These three examples confirm that the model tends to overpredict a bit at the lower boundary of
# the data, but that's quite acceptable.





### ** 1.1.3. Inference and predictions ----
# __________________________________________

### *** 1.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## Parametric bootstrap to test the additive effect of the connectivity metric:
ttCy_comglmm0 <- stats::update(ttCy_comglmm1, .~. -log_F_metric_d2b1)
summary(ttCy_comglmm0)$AIC # AIC = 1383.2 vs 1384 (hypothesis likely not validated)!

## Regular LRT (temporary results):
res.LRT_hypo1 <- stats::anova(object = ttCy_comglmm0, ttCy_comglmm1, test = "LRT")
res.LRT_hypo2 <- stats::anova(object = ttCy_comglmm1, ttCy_comglmm2, test = "LRT")
# Both are non-significant.

## Parametric bootstrap LRT (permanent results [NOTE that I leave it in "comments" as it takes
# ages to run (like SEVERAL DAYS), so it should not be launched by mistake as it requires parallel
# computing (or running on a separate powerful machine), and that needs specific parameterisation])
# to test the additive effect of the connectivity metric:
#' tictoc::tic("Parametric bootstrap LRT for the additive effect")
#' res.LRT_addeff <- DHARMa::simulateLRT(m0 = ttCy_comglmm0, m1 = ttCy_comglmm1, n = 500, seed = 10)
#' tt <- as.data.frame(cbind(res.LRT_addeff$method,
#'                           res.LRT_addeff$data.name,
#'                           res.LRT_addeff$statistic,
#'                           res.LRT_addeff$p.value))
#' rownames(tt) <- NULL
#' tt %>% dplyr::rename("Method" = V1,
#'                      "Models" = V2,
#'                      "Log Likelihood (M1/M0)" = V3,
#'                      "p-value" = V4) -> tt
#' readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ttCy_LRT_addeff.csv"))
#' tictoc::toc() # DISCLAIMER: took >40h to run!
#' # NOTE: initially, normally I would use the more efficient 'pbkrtest::PBmodcomp()' instead of the
#' #'DHARMa::simulateLRT()' function, but it doesn't work with {glmmTMB} objects.


## Parametric bootstrap to test the interactive effect of the connectivity metric:
#' tictoc::tic("Parametric bootstrap LRT for the additive effect")
#' res.LRT_inteff <- DHARMa::simulateLRT(m0 = ttCy_comglmm1, m1 = ttCy_comglmm2, n = 500, seed = 55)
#' tt <- as.data.frame(cbind(res.LRT_inteff$method,
#'                           res.LRT_inteff$data.name,
#'                           res.LRT_inteff$statistic,
#'                           res.LRT_inteff$p.value))
#' rownames(tt) <- NULL
#' tt %>% dplyr::rename("Method" = V1,
#'                      "Models" = V2,
#'                      "Log Likelihood (M1/M0)" = V3,
#'                      "p-value" = V4) -> tt
#' readr::write_csv2(x = tt, file = here::here("output", "tables", "res.ttCy_LRT_inteff.csv"))
#' tictoc::toc() # DISCLAIMER: took >40h to run!



### *** 1.1.3.2. Confidence intervals for estimated parameters ----
res.ttCy_addeff_CI <- confint(ttCy_comglmm1)
tt <- as.data.frame(res.ttCy_addeff_CI)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttCy_regCI_addeff.csv"))



### *** 1.1.3.3. Plotting results ----
tt2 <- tibble::as_tibble(tt)
tt2 %>% tibble::add_row("2.5 %" = 0, "97.5 %" = 0, parameters = "clutch_size",.after = 4) %>%
  as.data.frame() -> tt2

uu2 <- tibble::as_tibble(ttCy_comglmm1$fit$par[1:15])
uu2 %>% tibble::add_row(value = 0,.after = 4) %>%
  as.data.frame() -> uu2

base_data <- tibble::tibble(
  parameters = c("Intercept", "Local patch area", "Flux metric", "Species (Cyanistes caeruleus)",
                 "Clutch size", "Urban intensity", "Management intensity (moderate)",
                 "Management intensity (intensive)", "Light pollution", "Noise pollution",
                 "Traffic", "Temperature", "Laying day", "Year (2020)", "Year (2021)",
                 "Year (2022)"),
  mean = c(uu2[1:16,1]),
  lower = c(tt2[1:16,1]),
  upper = c(tt2[1:16,2]))

base_data |>
  forestplot::forestplot(labeltext = parameters,
                         clip = c(-0.15,0.35),
                         boxsize = 0.2) |>
  forestplot::fp_set_style(box = "royalblue",
                           line = "darkblue",
                           summary = "royalblue") |>
  forestplot::fp_add_header(parameters = "Variables") |>
  forestplot::fp_set_zebra_style("#EFEFEF")



### *** 1.1.3.4. Conclusion ----
# For the initial model:
summary(ttCy_comglmm1) # AIC = 1384 and Marg_R2_glmm = 0.105; Cond_R2_glmm = 0.12.
# Diagnostics ran for 'ttCy_comglmm1' indicated that the model fit the data relatively well.
# There are no major assumption violations, the use of a COM-Poisson distribution was warranted
# by the data while the use of a RE wasn't really. Still, the model tends to over-predict a bit.
# Note that the first diagnostics (ran for the models based on 'ntits' and not 'ntits3') indicated
# the presence of outliers likely produced by other ecological processes than the one our variables
# can account for (such as predation, parental care abandonment, etc.). So they were removed.
## Significant variables: patch_area (-), speciesCC (++), urban_intensity (+), noise_m (+),
# traffic (-), laying_day (-), and year2020 (-).
# Note however, that most effect sizes are quite small.
## Hypothesis 1 likely not validated (AIC = 1383.2 vs 1384), and hypothesis 2 neither!





########################## *****************************************###############################
# -------------------------------------- #
##### 2. Modelling fledging survival #####
# -------------------------------------- #

##### * 2.1. Fledging survival: Beta-binomial GLMM -----------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Initial model fit ----
# __________________________________

# In accordance with preliminary diagnostics, I remove observations with 'brood_size = 0' since
# they likely are true outliers (possibly linked to parental desertion, nest depredation, or other
# stochastic events that cannot be accounted for by our data):
ntits3 <- ntits[-c(which(ntits$brood_size == 0)),]
# Preliminary diagnostics for the initial models (not shown here but easily reproducible) also
# identified significant overdispersion for fledging survival. To account for that, Harrison (2014 -
# links: https://doi.org/10.7717/peerj.616) recommends to compare the use of an observation-level
# random effect (OLRE) with the use of a beta-binomial model, so that's what we did (see below):
ntits3$id_obs <- as.factor(1:nrow(ntits3)) # To create an observation-level RE (OLRE).

# Note also that in these models, we included "clutch_size" as a predictor because it is likely an
# important predictor of fledging survival. However, as could have been expected, it lead to
# pretty strong collinearity issues with "species" as it is one of the strongest predictors of
# tits' clutch size. We thus removed "species" from the list of predictors for the benefit of
# "clutch_size" as the removal of "species" had a lesser effect to reduce collinearity.
# Finally, note also that we replaced "cumdd_30" by "min_t_between" for two reasons: i) it seems
# theoretically sounder to use a proxy of extreme weather (such as "min_t_between") rather than a
# proxy of the amount of energy accumulated over a period of time (such as "cumdd_30" or
# "cumdd_between") to explain the success of tits' chicks rearing (the latter proxy being more
# relevant for initiating breeding) and; ii) because much of the information contained by the
# "cumdd_*" variables is also contained in "laying_day" and we thus avoid collinearity issues
# while retaining part of the desired information.

# Finally, note also that I left much of the code of the section below in "comments" to avoid
# overloading the script (it's easier/cleaner that way to run the whole script). Therefore, I only
# leave the code for the retained models, i.e. zero-inflated beta-binomial GLMMs (ZIBBIN models).
# But feel free to run the whole code if you like, you'll see that models in the comments (e.g.
# regular binomial GLMs or GLMMs, models without zero-inflation) perform more poorly than the
# retained models (i.e. ZIBBIN GLMMs).



# ### Fitting models without zero-inflation components:
# ## Fitting a regular binomial (BIN) GLM:
# ttFS_bin_glm1 <- stats::glm(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                            clutch_size +
#                            urban_intensity + manag_mid + manag_high +
#                            light_pollution + noise_m + traffic +
#                            min_t_between + laying_day + year,
#                          weights = brood_size, # Prior weights!
#                          data = ntits3, family = "binomial") # Weights should not be forgotten.
# # Otherwise, the formulation should be: Y = cbind(fledgling_nb, brood_size-fledgling_nb)!
#
# ## Fitting a beta-binomial (BBIN) GLM:
# ttFS_bbin_glm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                clutch_size +
#                                urban_intensity + manag_mid + manag_high +
#                                light_pollution + noise_m + traffic +
#                                min_t_between + laying_day + year,
#                              weights = brood_size, # Prior weights!
#                              data = ntits3,
#                              family = glmmTMB::betabinomial(link = "logit"))
#
#
# ## Fitting a binomial (BIN) GLMM:
# ttFS_bin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                   clutch_size +
#                                   urban_intensity + manag_mid + manag_high +
#                                   light_pollution + noise_m + traffic +
#                                   min_t_between + laying_day + year + (1|id_nestbox),
#                                 weights = brood_size, data = ntits3,
#                                 family = "binomial")
#
# ## Fitting a beta-binomial (BBIN) GLMM:
# ttFS_bbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                clutch_size +
#                                urban_intensity + manag_mid + manag_high +
#                                light_pollution + noise_m + traffic +
#                                min_t_between + laying_day + year + (1|id_nestbox),
#                              weights = brood_size, # Prior weights!
#                              data = ntits3,
#                              family = glmmTMB::betabinomial(link = "logit"))
#
# ## Fitting a binomial (BIN) GLMM with an OLRE:
# ttFS_bin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
#                                           log_F_metric_d2b1 + clutch_size +
#                                   urban_intensity + manag_mid + manag_high +
#                                   light_pollution + noise_m + traffic +
#                                   min_t_between + laying_day + year + (1|id_obs) + (1|id_nestbox),
#                                 weights = brood_size, data = ntits3,
#                                 family = "binomial")
# summary(ttFS_bin_glm1) # AIC = 2105.8.
# summary(ttFS_bbin_glm1) # AIC = 1475.5, so the beta-binomial GLM >> binomial GLM.
# summary(ttFS_bin_glmm1) # AIC = 1674.9, so the binomial GLMM >> binomial GLM.
# summary(ttFS_bbin_glmm1) # AIC = 1476.3, so the beta-binomial GLMM >> binomial GLM or GLMM, but it
# # doesn't change much compared to the beta-binomial GLM (useless RE?).
# summary(ttFS_bin_glmm1_olre) # AIC = 1476, so the OLRE binomial GLMM is worth the beta-binomial
# # models.
# ## So BBIN GLM(M) or BIN GLMM with OLRE seem better.
#
#
# ### Including a zero-inflation component to the models:
# ## Fitting a zero-inflated binomial (ZIBIN) GLMM:
# ttFS_zibin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area+
#                                        log_F_metric_d2b1 + clutch_size +
#                                     urban_intensity + manag_mid + manag_high +
#                                     light_pollution + noise_m + traffic +
#                                     min_t_between + laying_day + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1) # Intercept only.
#
# # Fitting a ZIBIN GLMM with OLRE:
# ttFS_zibin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
#                                             log_F_metric_d2b1 + clutch_size +
#                                     urban_intensity + manag_mid + manag_high +
#                                     light_pollution + noise_m + traffic +
#                                     min_t_between + laying_day + year + (1|id_obs)+(1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1) # Intercept only.

## Fitting a zero-inflated beta-binomial (ZIBBIN) GLM:
ttFS_zibbin_glm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
                                       log_F_metric_d2b1 + clutch_size +
                                    urban_intensity + manag_mid + manag_high +
                                    light_pollution + noise_m + traffic +
                                    min_t_between + laying_day + year,
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1) # Intercept only.

## Fitting a ZIBBIN GLMM:
ttFS_zibbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_mid + manag_high +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
# summary(ttFS_zibin_glmm1) # AIC = 1415.1 (compared to 2105.8 without ZI).
# summary(ttFS_zibin_glmm1_olre) # AIC = 1382.3 (compared to 1476 without ZI), so the OLRE still
# # improves the fit compared to the GLM.
summary(ttFS_zibbin_glm1) # AIC = 1377.5 (compared to 1475.5 without ZI), so the ZIBBIN GLM does
# slightly better than the BIN GLMM with OLRE.
summary(ttFS_zibbin_glmm1) # AIC = 1379.5 (compared to 1476.3 without ZI), so the ZIBBIN GLMM does
# not better than the ZIBBIN GLM (AIC are quite close), but it still performs better than any
# ZIBIN model.


# ### Adding the interaction effect:
# ## Fitting an interactive (mediated) ZIBIN GLMM:
# ttFS_zibin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
#                                        c.log_patch_area * c.log_F_metric_d2b1 +
#                                        clutch_size +
#                                        urban_intensity + manag_mid + manag_high +
#                                        light_pollution + noise_m + traffic +
#                                        min_t_between + laying_day + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1)
#
# ## Fitting an interactive ZIBIN GLMM with OLRE:
# ttFS_zibin_glmm2_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
#                                             c.log_patch_area * c.log_F_metric_d2b1 +
#                                             clutch_size +
#                                             urban_intensity + manag_mid + manag_high +
#                                             light_pollution + noise_m + traffic +
#                                             min_t_between + laying_day + year + (1|id_obs) +
#                                             (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1)

## Fitting an interactive ZIBBIN GLM:
ttFS_zibbin_glm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                       c.log_patch_area * c.log_F_metric_d2b1 +
                                       clutch_size +
                                       urban_intensity + manag_mid + manag_high +
                                       light_pollution + noise_m + traffic +
                                       min_t_between + laying_day + year,
                                  weights = brood_size, data = ntits3,
                                  family = glmmTMB::betabinomial(link = "logit"),
                                  ziformula = ~1)

## Fitting an interactive ZIBBIN GLMM:
ttFS_zibbin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                        c.log_patch_area * c.log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_mid + manag_high +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1)
# summary(ttFS_zibin_glmm2) # 1409.5 (compared to 1415.1 without ZI), so the interaction improves
# # the fit (and is significant)!
# summary(ttFS_zibin_glmm2_olre) # 1379.7 (compared to 1382.3 without ZI), so the interaction
# # improves the fit (and is significant) but to a lesser extent than without the OLRE!
summary(ttFS_zibbin_glm2) # 1374.5 (compared to 1377.5 without ZI), so the interaction improves
# the fit (and is significant) and this model beats the ZIBIN ones!
summary(ttFS_zibbin_glmm2) # 1376.5 (compared to 1379.5 without ZI), so it doesn't change much
# compared to the ZIBBIN GLM, yet it is still better than any ZIBIN model.

## It seems that, the inclusion of a random effect (RE) strongly improved the fit when the ZI and
# overdispersion are not accounted for but not otherwise. Accounting for the ZI and overdispersion
# in the response, whether by using an OLRE or using a beta-binomial distribution, further
# improved the fit but it is not yet clear which one is best although there's a slight advantage
# for the ZIBBIN models in terms of AIC.
# Consequently, we will run the ZIBIN GLMM with OLRE as well as the ZIBBIN GLM(M) through the
# diagnostic phase to better understand their differences and select which ones will be used for
# further analyses. Note that for the sake of brevity, I will not always show the code and
# comments for the diagnostics of all models (but, as usual, it's fairly easy for you to do it by
# simply switching the models name in the code chunks below).

## UPDATE: Diagnostics showed that using a beta-binomial distribution was the correct way to go and
# not using an OLRE.





### ** 2.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttFS_zibbin_glmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # There seems to be 2 groups of residuals (linked to ZI?).
# performance::check_outliers(ttFS_zibbin_glmm1) # Does not work for this type of model.
ntits3[which(resid < -0.4),] # Nestboxes with the lowest residuals = ~0% fledging survival!

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$noise_m, y = resid) # Seems rather ok although we once again find patterns linked
# to the sometimes odd distribution of some predictors. However, be reminded that simulated
# residuals will be more useful).
# plot(ttFS_zibbin_glmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttFS_zibbin_glmm1, site~stats::resid(.)) # Does not work for this type of model.


## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm1,
                                        n = 1000, re.form = NULL) # The 're.form' argument is to
# base simulations on the model unconditional of the random effects (and only works for {lme4}
# formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm2,
                                        n = 1000, re.form = NULL)
plot(simu.resid) # Ok.
plot(simu.resid2) # Ok.
DHARMa::outliers(simu.resid) # Ok.
DHARMa::outliers(simu.resid2) # Ok.


## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Spatial
# autocorrelation detected only for the OLRE models!
performance::check_autocorrelation(ttFS_zibbin_glmm1) # Ok.
performance::check_collinearity(ttFS_zibbin_glmm1) # Ok for the beta-binomial models (highest VIF
# value for the first ZIBBIN = 3.44 for the F-metric while it was 4.03 for the second model).
# For the OLRE models however, alarming multicollinearity issues have been detected with a
# moderate correlation linked to "clutch_size" being present, several VIF values > 4 and even 8.5
# for the "F-metric" (even though things were a bit better for the interactive model).
# Interestingly, no multicollinearity was detected for the BBIN or the BIN_OLRE models either.
stats::vcov(ttFS_zibbin_glmm1) # Ok.


## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_perim)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_woody_vw)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$Rr_metric_d2c1)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$clutch_size)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity) # Deviation detected (for OLRE)!
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_between)
DHARMa::plotResiduals(simu.resid, form = ntits3$min_t_between)
DHARMa::plotResiduals(simu.resid, form = ntits3$laying_day)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
# For the ZIBBIN models, no deviations were detected. For the models with OLRE, worrying deviations
# have been found for "urban_intensity" (likely linked to the "built volume").



### *** 2.1.2.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
DHARMa::testDispersion(simu.resid) # Ok for all models.
performance::check_overdispersion(x = ttFS_zibbin_glmm1) # Overdispersion detected (NOTE: for the
# regular non BBIN models)! However, note that this test is known to be inaccurate for ZI-models
# (see the function's help page). If I run it on the non-ZI model (i.e. 'ttFS_bbin_glmm1'), the
# dispersion ratio is much lower, yet we may still need to account for that.
# ATTENTION: classical overdispersion tests cannot be used to detect overdispersion when OLRE is
# used to account for it, and the same is likely true for beta-binomial models. So I'll ignore
# it now.


## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = ttFS_zibbin_glmm1, type = "response") # Extract the
# predicted probabilities.
par(mfrow= c(1,2))
hist(probabilities, main = "Predicted proportions", xlab = "fledging survival")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
par(.pardefault)
# The ZIBBIN models globally fail to correctly predict the data. Prediction ranges are too narrow,
# the mode is around 0.7 instead of 1 and models do not predict total successes or failures (even
# though the interaction model does a little better). The models with OLRE predict slightly better
# than the ZIBBIN (mode at 0.8) but it's still not very satisfactory.
# Interestingly, the overdispersed binomial GLMM without ZI ('ttFS_bin_glmm1') does a better job
# at predicting the observed data than any other model, and the binomial GLMM without ZI but with
# OLRE ('ttFS_bin_glmm1_olre') predicts the data very well! HOWEVER, predictions are not THAT off,
# we deem safer to favour sound diagnostics over accurate predictions, and thus stick with the
# ZIBBIN models (ZI and overdispersion are unarguable)!


## Zero-inflation:
DHARMa::testZeroInflation(simu.resid) # Significant for the non-ZI models!



### *** 2.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         clutch_size,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_between, min_t_between, laying_day) -> mydata
predictors <- colnames(mydata)
probabilities <- stats::predict(object = ttFS_zibbin_glmm1,
                                type = "response") # Extract probabilities!
# Bind the logit and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)
# Create scatterplot:
ggplot2::ggplot(mydata, ggplot2::aes(y = logit, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity is globally respected except for
# one or two predictors that are U-shaped (a transformation could perhaps be used).



### *** 2.1.2.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
# dat.resid <- sum(stats::resid(ttFS_zibbin_glmm1, type = "pearson")^2)
# 1 - stats::pchisq(dat.resid, stats::df.residual(ttFS_zibbin_glmm1)) # Does work for this kind of
# # model.

## Computing a pseudo-R2:
performance::r2_nakagawa(ttFS_zibbin_glmm1, tolerance = 0.0000000000001)
# [Additive model]: Marg_R2_glmm = 0.19; Cond_R2_glmm = 0.19 (but may be unreliable due to
# difficulties to compute RE variances)!
performance::r2_nakagawa(ttFS_zibbin_glmm2, tolerance = 0.0000000000001)
# [Interactive model]: Marg_R2_glmm = 0.19; Cond_R2_glmm = 0.19 (but may be unreliable due to
# difficulties to compute RE variances)!

## Likelihood-based evaluation of effects inclusion:
zzz <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_mid + manag_high +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year + (1|site),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
zzz2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_mid + manag_high +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year +
                           (1|id_nestbox) + (1|site),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
summary(zzz) # 1379.1.
summary(zzz2) # 1381.1.
# Beta-binomial models: The non-mixed model gives AIC = 1377.5 while mixed-models yield an AIC
# of 1379.5 (with "id_nestbox"), 1379.1. (with "site"), or 1381.1. (with both). It thus appears
# that the the use of RE is not useful here! We'll still use the GLMM_1 for further analyses, just
# in case (and because some consider that you should always include RE when you can, although this
# is rather a question of statistical philosophy, I reckon).

# Importance of the fixed effects:
ttFS_zibbin_glmm0 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1)
summary(ttFS_zibbin_glmm0) # AIC = 1462.9 vs 1379.5, so the full model is clearly far better!





### ** 2.1.3. Inference and predictions ----
# __________________________________________

### *** 2.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttFS_zibbin_glmm0 <- stats::update(ttFS_zibbin_glmm1, .~. -log_F_metric_d2b1)
summary(ttFS_zibbin_glmm0) # AIC = 1383.3 vs 1379.5 (hypothesis 1 likely validated)!
ttFS_zibbin_glm0 <- stats::update(ttFS_zibbin_glm1, .~. -log_F_metric_d2b1)
summary(ttFS_zibbin_glm0) # AIC = 1381.3 vs 1377.5 (hypothesis 1 likely validated)!

## Regular LRT (temporary results):
res.LRT_hypo1 <- stats::anova(object = ttFS_zibbin_glmm0, ttFS_zibbin_glmm1, test = "LRT")
res.LRT_hypo2 <- stats::anova(object = ttFS_zibbin_glmm1, ttFS_zibbin_glmm2, test = "LRT")
# stats::anova(object = ttFS_zibbin_glm0, ttFS_zibbin_glm1, test = "LRT")
# stats::anova(object = ttFS_zibbin_glm1, ttFS_zibbin_glm2, test = "LRT")
## As, in this case, results from the GLMs give exactly the same results as those from GLMMs, the
# use of a parametric-bootstrap LRT does not seem useful here. So we'll simply stick with the
# regular LRT results, which are clearly significant thus validating both our hypotheses.



### *** 2.1.3.2. Confidence intervals for estimated parameters ----
res.ttFSy_addeff_CI <- confint(ttFS_zibbin_glmm1)
tt <- as.data.frame(res.ttFSy_addeff_CI)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttFSy_regCI_addeff.csv"))



### *** 2.1.3.3. Plotting results ----
## Plotting the confidence intervals:
tt2 <- tibble::as_tibble(tt)
tt2 %>% tibble::add_row("2.5 %" = 0, "97.5 %" = 0, parameters = "species",.after = 3) %>%
  as.data.frame() -> tt2

uu2 <- tibble::as_tibble(ttFS_zibbin_glmm1$fit$par[1:15])
uu2 %>% tibble::add_row(value = 0,.after = 3) %>%
  as.data.frame() -> uu2

base_data <- tibble::tibble(
  parameters = c("Intercept", "Local patch area", "Flux metric", "Species (Cyanistes caeruleus)",
                 "Clutch size", "Urban intensity", "Management intensity (moderate)",
                 "Management intensity (intensive)", "Light pollution", "Noise pollution",
                 "Traffic", "Temperature", "Laying day", "Year (2020)", "Year (2021)",
                 "Year (2022)"),
  mean = c(uu2[1:16,1]),
  lower = c(tt2[1:16,1]),
  upper = c(tt2[1:16,2]))

base_data |>
  forestplot::forestplot(labeltext = parameters,
                         clip = c(-1,2),
                         boxsize = 0.2) |>
  forestplot::fp_set_style(box = "royalblue",
                           line = "darkblue",
                           summary = "royalblue") |>
  forestplot::fp_add_header(parameters = "Variables") |>
  forestplot::fp_set_zebra_style("#EFEFEF")


## Creating a 2D plot for the interaction effect (with PATCH_AREA as X):
sjPlot::plot_model(ttFS_zibbin_glmm2, type = "pred",
                   terms = c("c.log_patch_area [all]",
                             "c.log_F_metric_d2b1 [-1.82, -0.3, 0, 0.32, 1.12]"),
                   mdrt.values = "quart", # Only useful if `type = "int"` is used (i.e. automatic
                   # plotting of interaction effects). If used, only plots the 3 quartile values
                   # for the moderator.
                   bias_correction = TRUE,
                   title = "", # If I don't let it blank (and delete it), it sets a title by default.
                   axis.title = c("Local patch area",
                                  "Predicted fledging survival rate"),
                   legend.title = "Flux metric",
                   colors = c("darkred", "white", "darkorange", "darkolivegreen3",
                              "chartreuse4"), # For a reason I don't understand, the 2nd colour in
                   # the vector is not taken into account!
                   show.data = TRUE, # Not available, I don't know why.
                   line.size = 1)


## Creating a 2D plot for the interaction effect (with F-METRIC as X):
sjPlot::plot_model(ttFS_zibbin_glmm2, type = "pred",
                   terms = c("c.log_F_metric_d2b1 [all]",
                             "c.log_patch_area [-2.26, -0.62, 0, 0.56, 1.18]"),
                   mdrt.values = "quart", # Only useful if `type = "int"` is used (i.e. automatic
                   # plotting of interaction effects). If used, only plots the 3 quartile values
                   # for the moderator.
                   bias_correction = TRUE,
                   title = "", # If I don't let it blank (and delete it), it sets a title by default.
                   axis.title = c("Connectivity (F-metric)",
                                  "Predicted fledging survival rate"),
                   legend.title = "Patch area",
                   colors = c("darkred", "white", "darkorange", "darkolivegreen3",
                              "chartreuse4"), # For a reason I don't understand, the 2nd colour in
                   # the vector is not taken into account!
                   show.data = TRUE, # Not available, I don't know why.
                   line.size = 1)


## Creating a 3D interaction plot:
x_tilde <- expand.grid(c.log_patch_area = seq(-2.26,1.184, length.out=15),
                       c.log_F_metric_d2b1 = seq(-1.822,1.125, length.out=15), # Computes every
                       # combination for the input variables (here a sequence of 15 values roughly
                       # ranging the actual values of the two predictors involved in the modeled
                       # interaction).
                       clutch_size = 9,
                       urban_intensity = 0,
                       manag_intensity = "0",
                       light_pollution = 0,
                       noise_m = 5.3,
                       traffic = 2.1,
                       min_t_between = 0,
                       laying_day = 9.3,
                       year = "2019",
                       brood_size = 8,
                       id_nestbox = "DIJ-071") # Here, I used approximately median values for all
# the other predictors or for selected factors (e.g. DIJ-071 is for a nestbox in the old city).
x_tilde$fledging_rate <- predict(ttFS_zibbin_glmm2, x_tilde, type="response")

lattice::trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))
lattice::wireframe(fledging_rate ~ c.log_patch_area + c.log_F_metric_d2b1, data=x_tilde,
                   main = "Predicted fledging rate",
                   drape = TRUE,
                   colorkey = TRUE,
                   zlab = list("Fledging survival rate", cex=1.3, rot=90),
                   xlab = list("Patch area", cex=1.3, rot=-40),
                   ylab = list("F-metric", cex=1.3, rot=17),
                   scales = list(arrows=FALSE, cex=1, tick.number = 10,
                                 z = list(arrows=F),
                                 distance = c(1, 1, 1)), # Control the distance of axis labels.
                   light.source = c(10,0,10),
                   col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100,
                                         alpha = .5), # Controls the transparency.
                   screen = list(z = -60, x = -75)) # Z controls the rotation.



### *** 2.1.3.3. Conclusion ----
# For the initial model:
summary(ttFS_zibbin_glmm1) # AIC = 1379.5 and both R2_glmm = 0.19!
summary(ttFS_zibbin_glmm2) # AIC = 1376.5 and both R2_glmm = 0.19!
# Diagnostics ran for these models indicated that the model fit the data relatively well although
# prediction ranges are too narrow and the models fail to predict total failures and successes!
# It is possible that the ZI forces the models to predict medium values.u
# Interestingly and inexplicably, some of the models without ZI (especially the binomial GLMM
# with OLRE) predicted the observed data far better but yielded worrying diagnostics and had lower
# AIC values than the retained models. Remember that the retained models use a beta-binomial
# family as the response displayed overdispersion without it. Models that did not account for this
# overdispersion also raised other red flags. As recommended by Harrison (2014), we compared the
# beta-binomial models with models including OLRE but the latter models also displayed some
# problematic issues such as concerning multicollinearity, autocorrelation and quantile deviations.
# Diagnostics from the beta-binomial models were all satisfactory if we excluded "species" from
# the models as it is collinear with "clutch_size". We thus chose to keep "clutch_size" as
# it is a more informative predictor and yielded better diagnostics. However, it should be noted
# that there was a species effect as blue tits tended to have a lower fledging survival than
# Parus major:
ntits3 %>% dplyr::mutate(fledging_success = fledgling_nb/clutch_size) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(mean_fs = mean(fledging_success)) # Note that this mean fledging survival does
# not account for the effect of the covariates unlike a coefficient estimate (you could
# alternatively fit a model replacing "clutch_size" by "species" or using both, to estimate a
# more accurate effect).
# Finally, it also appears that for this response variable the use of mixed models was not truly
# required. We kept the GLMM just in case, but the results are the same without random effects.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), and laying_day (--).
## For the interactive (moderated) model, the INTERACTION TERM was also significant (--) but the
# F-metric was not anymore, which could be a sign of a cross-over interaction. Furthermore,
# light_pollution (-) also turned out significant in the interactive model (with only a trend in
# the additive one)!
## Hypothesis 1 and 2 possibly validated (AIC = 1383.3 vs 1379.5)!


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






########################## *****************************************###############################
# ------------------------------------------------ #
##### 3. Modelling the mass of tits fledglings #####
# ------------------------------------------------ #

##### * 3.1 Mass: LMM ---------------------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 3.1.1. Initial model fit ----
# __________________________________

ntits %>% dplyr::filter(is.na(mass) == FALSE) -> ntits3 # Only 317 observations left.

## Fitting a regular linear model:
ttMA_lm1 <- glmmTMB::glmmTMB(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                               urban_intensity + manag_mid + manag_high +
                               light_pollution + noise_m + traffic +
                               min_t_between + laying_day + year,
                             data = ntits3, family = "gaussian")

## Fitting an additive LMM:
ttMA_lmm1 <- glmmTMB::glmmTMB(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                                urban_intensity + manag_mid + manag_high +
                                light_pollution + noise_m + traffic +
                                min_t_between + laying_day + year + (1|id_nestbox),
                              data = ntits3, family = "gaussian")
ttMA_lmm1b <- lme4::lmer(mass ~ log_patch_area + log_F_metric_d2b1 + species + clutch_size +
                           urban_intensity + manag_mid + manag_high +
                           light_pollution + noise_m + traffic +
                           min_t_between + laying_day + year + (1|id_nestbox), data = ntits3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5))) # Same model but
# fitted with {lme4} for comparison sake! Note however, that it is fitted by REML and, if not,
# it is singular!


## Fitting interactive (mediated) LMMs:
ttMA_lmm2 <- glmmTMB::glmmTMB(mass ~ c.log_patch_area * c.log_F_metric_d2b1 + species +
                                clutch_size + urban_intensity + manag_mid + manag_high +
                                light_pollution + noise_m + traffic +
                                min_t_between + laying_day + year + (1|id_nestbox),
                              data = ntits3, family = "gaussian")
ttMA_lmm2b <- lme4::lmer(mass ~ c.log_patch_area * c.log_F_metric_d2b1 + species + clutch_size +
                           urban_intensity + manag_mid + manag_high +
                           light_pollution + noise_m + traffic +
                           min_t_between + laying_day + year + (1|id_nestbox), data = ntits3,
                         control=lme4::lmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=2e5)))
AIC(ttMA_lm1) # AIC = 1141.3.
summary(ttMA_lmm1) # AIC = 1143.3.
summary(ttMA_lmm1b) # REML = 1146.2 (not a true AIC).
summary(ttMA_lmm2) # AIC = 1144.7, does not seem supported by the data!
summary(ttMA_lmm2b) # REML = 1147, does not seem supported by the data!
# Note that for both LMM, RE variance is extremely low (nearly singular fit) which, with the AIC,
# suggests that the use of a mixed-model is not warranted by the data.






### ** 3.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 3.1.2.1. Residuals extraction, autocorrelation and collinearity ----
## Extracting residuals (with the {redres} package):
raw_cond <- redres::compute_redres(ttMA_lmm1b) # Computes the raw conditional residuals
# (conditional on the random effects (RE)).
pearson_mar <- redres::compute_redres(ttMA_lmm1b, type = "pearson_mar") # Computes the Pearson
# marginal (not accounting for the RE) residuals.
std_cond <- redres::compute_redres(ttMA_lmm1b, type = "std_cond") # Computes the studentised
# conditional ones.
# Joins the residuals to the data:
xxx <- cbind(ntits3, raw_cond, pearson_mar, std_cond)

## Simulation-based scaled residuals computation (DHARMa method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttMA_lmm1b, n = 1000, plot = FALSE)
plot(simu.resid) # One outlier detected!
DHARMa::outliers(simu.resid) # Obs. number 192.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok.
performance::check_autocorrelation(ttMA_lmm1b) # Ok.
performance::check_collinearity(ttMA_lmm1b) # Ok-ish but some VIF > 4-5 and moderate correlation
# for "species" in the interaction model (possibly linked to clutch size?), but as the latter
# seems invalidated by the data, it can be disregarded.
stats::vcov(ttMA_lmm1b) # Ok.



### *** 3.1.2.2. Distribution and homoscedasticity ----
## Assessing the normality of the residuals:
stats::shapiro.test(xxx$raw_cond) # Ok. But plotting would be better:
redres::plot_resqq(ttMA_lmm1b) # As expected, the plot shows a substantial departure from
# normality at the extreme ends of the quantiles, that is at the border of the parameters space.
# Overall, as almost all points stay within the 95% CI, it's quite ok.
# Plotting them against each numeric predictor:
xxx %>%
  tidyr::gather(key = "type", value = "residual",
                c(log_patch_area, log_F_metric_d2b1, clutch_size, urban_intensity,
                  light_pollution, noise_m, traffic, min_t_between, laying_day)) %>%
  ggplot2::ggplot(ggplot2::aes(x = residual)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_grid(. ~ type, scales = "free") +
  ggplot2::theme_bw() # Residuals vs predictor plots look rather ok, even though some strange
# patterns exist that reflect the distribution of the predictors.

## Assessing the normality in the random effect:
redres::plot_ranef(ttMA_lmm1b) # Ok-ish, but quite a strong departure from normality for the last
# residual.

## Assessing homogeneity of variance and influential observations:
plot(ttMA_lmm1b, type=c("p","smooth"), col.line = 2, id = 0.05, idLabels = ~.obs,
     ylab = "Pearson's residuals", xlab = "Fitted values") # It's ok but there are a few possible
# outliers:
ntits3[c(112,102,32,194,155),] # High residuals ~= heavy PM fledglings. Ok.
ntits3[c(173,26,264,296,198,152,170,192),] # Low residuals ~= light PM fledglings. Ok.

# Residuals vs leverage:
par(.pardefault)
plot(ttMA_lmm1b, stats::rstudent(.) ~ stats::hatvalues(.))
cd <- stats::cooks.distance(ttMA_lmm1b)
plot(cd, ylab = "Cook's distance")
ntits3[which(cd>0.4),] # Ok, all observations are < 0.5, so no overly influential points.
ntits3[which(cd>0.1),] # Even with very conservative values, it's ok!

## Residuals vs predictors:
redres::plot_redres(ttMA_lmm1b, xvar = "min_t_between") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Residual vs predictor") # Ok-ish.
# plot(ntits3$log_F_metric_d2b1, stats::residuals(ttMA_lmm1b)) # Same plot (I should create a
# custom function).
redres::plot_redres(ttMA_lmm1b, type = "raw_mar", xvar = "year") # Ok.

## Distribution of the predicted values:
par(.pardefault)
predictions <- stats::predict(object = ttMA_lmm1b, type = "response") # Predicted values extraction.
par(mfrow= c(1,2))
hist(predictions, main = "Predicted mass", xlab = "Nestling mass (g)")
plot(ecdf(predictions), main = "Predicted CDF", xlab = "Nestling mass (g)")
fitdistrplus::plotdist(data = ntits3$mass, histo = TRUE, demp = TRUE) # Rather ok.



### *** 3.1.2.3. Linearity ----
## Plotting the response on the logit scale (= log odds) against predictors:
# Format data:
ntits3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
                         log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
                         log_F_metric_d2b1,
                         Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
                         clutch_size, brood_size,
                         urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
                         light_pollution, noise_m,
                         cumdd_between, min_t_between) -> mydata
predictors <- colnames(mydata)
# Bind 'mass' and tidying the data for plot (ggplot2, so long format):
mydata <- mydata %>%
  dplyr::mutate(mass = ntits3$mass) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -mass)
# Create scatterplot
ggplot2::ggplot(mydata, ggplot2::aes(y = mass, x = predictor.value))+
  ggplot2::geom_point(size = 0.5, alpha = 0.5) +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap(~predictors, scales = "free_x") # Ok.



### *** 3.1.2.4. Goodness-of-fit (GOF) and performances ----
## Computing a pseudo-R2:
performance::r2_nakagawa(ttMA_lmm1) # [Additive model]: Marg_R2_lmm = 0.81; Cond_R2_lmm = 0.81.
performance::r2_nakagawa(ttMA_lmm2) # [Interact. model]: Marg_R2_lmm = 0.81; Cond_R2_lmm = 0.81.

## Likelihood-based evaluation of effects inclusion:
# Importance of the "id_nestbox" random-effect (RE):
tictoc::tic("Parametric bootstrap LRT for RE inclusion")
res.LRT_re <- DHARMa::simulateLRT(m0 = ttMA_lm1, m1 = ttMA_lmm1, n = 1000, seed = 547)
tictoc::toc() # Took ~5 min to run.
# The LRT is not significant, suggesting that M1 does NOT describe the data better than M0,
# thus invalidating the importance of the random effect! Yet, as usual, we will keep using the
# GLMM just in case.

# Importance of the fixed effects (only using the LM):
ttMA_lm0 <- glmmTMB::glmmTMB(mass ~ 1, data = ntits3, family = "gaussian")
res.LRT_null <- stats::anova(object = ttMA_lm0, ttMA_lm1, test = "LRT")
# The test is highly significant, confirming that the model is useful to explain the data.





### ** 3.1.3. Inference and predictions ----
# __________________________________________

### *** 3.1.3.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttMA_lmm0b <- stats::update(ttMA_lmm1b, .~. -log_F_metric_d2b1)

res.LRT_addeff <- pbkrtest::PBmodcomp(ttMA_lmm1b,
                                      ttMA_lmm0b, nsim = 1000, seed = 182) # Took ~41s to run!
readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
                                                             "res.ttMA_LRT_addeff.csv"))
# The LRT is significant, indicating that our connectivity metric does improve the description
# of the data.


## For the interaction effect:
res.LRT_inteff <- pbkrtest::PBmodcomp(ttMA_lmm2b,
                                      ttMA_lmm1b, nsim = 1000, seed = 279) # Took ~46s to run!
readr::write_csv2(x = res.LRT_inteff$test, file = here::here("output", "tables",
                                                             "res.ttMA_LRT_inteff.csv"))
# The LRT is NOT significant, indicating that our hypothesis of an interaction effect is not
# supported by the data. Note however that there was a lot of extreme samples.



### *** 3.1.3.2. Confidence intervals for estimated parameters ----
tictoc::tic("CI for additive LMM parameters")
res.ttMA_addeff_CI_boot <- confint(ttMA_lmm1b)
tt <- as.data.frame(res.ttMA_addeff_CI_boot)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttMA_CI_addeff.csv"))
tictoc::toc() # DISCLAIMER: took ~11s to run!





### *** 3.1.3.3. Plotting results ----
base_data <- tibble::tibble(
  parameters = c("Intercept", "Local patch area", "Flux metric", "Species (Cyanistes caeruleus)", "Clutch size",
                 "Urban intensity", "Management intensity (moderate)", "Management intensity (intensive)",
                 "Light pollution", "Noise pollution", "Traffic", "Temperature", "Laying day", "Year (2020)",
                 "Year (2021)", "Year (2022)"),
  mean = c(ttMA_lmm1b@beta),
  lower = c(tt[3:18,1]),
  upper = c(tt[3:18,2]))

base_data |>
  forestplot::forestplot(labeltext = parameters,
                         clip = c(-1,2),
                         boxsize = 0.2) |>
  forestplot::fp_set_style(box = "royalblue",
                           line = "darkblue",
                           summary = "royalblue") |>
  forestplot::fp_add_header(parameters = "Variables") |>
  forestplot::fp_set_zebra_style("#EFEFEF")





### *** 3.1.3.4. Conclusion ----
# For the initial model:
summary(ttMA_lmm1b) # AIC = 1147.3 and both R2_lmm = 0.81.
summary(ttMA_lmm2b) # AIC = 1148.1 and both R2_lmm = 0.81.
# Diagnostics ran for 'ttMA_lmm1b' and 'ttMA_lmm2b' indicated that the models do fit the data
# relatively well and that there are no major cause for concern:
# - All sorts of residuals are globally fine. Even though some patterns exist that are linked to the
#   imperfect sampling of the predictor space. There's not much we could do about it except, perhaps, try
#   some more advanced variable transformations.
# - Assumptions all seem validated, although there is a mild multicollinearity with some VIF > 4-5
#   and moderate correlation for "species" in the interactive model. However, as this model quite
#   largly rejects our working hypothesis, we will reject the model altogether.
# - No overly influential observations although some potential outliers may exist: brood 192.
## Significant variables: F-metric (++), speciesCC (---), clutch_size (-), high_manag (-),
#  laying_day (-), year2020 (++) and 2022 (++).
## Hypothesis 1 is validated (PB-based LRT = 5.833, df=1, p = 0.011) but hypothesis 2 is rejected
#  (PB-based LRT = 0.653, df=1, p = 0.52)!

