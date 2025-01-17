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
# NOTE: I
# have to load the "tits" final dataset exported by 'tfinal_EDAta()' in order to avoid running
# the former function that is a bit long to run (but you can if you want).





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
                          urban_intensity + manag_intensity +
                          light_pollution + noise_m + traffic +
                          cumdd_30 + laying_day + year,
                        data = ntits3, family = "poisson")

## Fitting a regular Poisson GLMM:
ttCy_glmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                 urban_intensity + manag_intensity +
                                 light_pollution + noise_m + traffic +
                                 cumdd_30 + laying_day + year + (1|id_nestbox),
                               data = ntits3, family = "poisson")


## Fitting a regular Conway-Maxwell (COM) Poisson regression (GLM):
ttCy_comglm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                   urban_intensity + manag_intensity +
                                   light_pollution + noise_m + traffic +
                                   cumdd_30 + laying_day + year,
                                 data = ntits3, family = glmmTMB::compois(link = "log"),
                                 dispformula = ~1) # Intercept only 'nu' (default).
# # OR:
# ttCy_comglm1b <- COMPoissonReg::glm.cmp(formula.lambda =
#                                           clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                           urban_intensity + manag_intensity +
#                                           light_pollution + noise_m + traffic +
#                                           cumdd_30 + laying_day + year,
#                                      data = ntits3, formula.nu = ~1) # Intercept only 'nu' (default).

## Fitting a regular Conway-Maxwell (COM) Poisson mixed model (GLMM):
ttCy_comglmm1 <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                    urban_intensity + manag_intensity +
                                    light_pollution + noise_m + traffic +
                                    cumdd_30 + laying_day + year + (1|id_nestbox),
                                  data = ntits3, family = glmmTMB::compois(link = "log"),
                                  dispformula = ~1) # Rather long to fit.

## Fitting the interaction model (COM-Poisson GLMM):
ttCy_comglmm2 <- glmmTMB::glmmTMB(clutch_size ~
                                    c.log_patch_area * c.log_F_metric_d2b1 + species +
                                    urban_intensity + manag_intensity +
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
                                      urban_intensity + manag_intensity +
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



### *** 1.1.3.2. Bootstrapped confidence intervals for estimated parameters ----
res.ttCy_addeff_CI <- confint(ttCy_comglmm1)
tt <- as.data.frame(res.ttCy_addeff_CI)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttCy_regCI_addeff.csv"))

# ## Bootstrapped CI (as for the LRT, it takes hours to run, so I leave it in "comments" to avoid
# crashing my R session by mistake):
# tictoc::tic("Bootstrap CI for the additive COM-Poisson GLMM parameters")
# res.ttCy_addeff_CI_boot <- confint(ttCy_comglmm1, method="boot")
# tt <- as.data.frame(res.ttCy_addeff_CI_boot)
# tt$parameters <- rownames(tt)
# readr::write_csv2(x = tt,
#                   file = here::here("output", "tables", "res.ttCy_bootCI_addeff.csv"))
# tictoc::toc() # DISCLAIMER: took ~4h10 to run!



### *** 1.1.3.3. Plotting results ----
tt2 <- tibble::as_tibble(tt) # Mind the 'tt' version you use (the regular CI or the boostrap one),
# but they give pretty similar results, as expected.
tt2 %>% tibble::add_row("2.5 %" = 0, "97.5 %" = 0, parameters = "clutch_size",.after = 4) %>%
  as.data.frame() -> tt2

uu2 <- tibble::as_tibble(ttCy_comglmm1$fit$par[1:15])
uu2 %>% tibble::add_row(value = 0,.after = 4) %>%
  as.data.frame() -> uu2

base_data <- tibble::tibble(
  parameters = c("Intercept", "Local patch area", "Flux metric", "Species (Cyanistes caeruleus)", "Clutch size",
                 "Urban intensity", "Management intensity (moderate)", "Management intensity (intensive)",
                 "Light pollution", "Noise pollution", "Traffic", "Temperature", "Laying day", "Year (2020)",
                 "Year (2021)", "Year (2022)"),
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





########################## ************************************************* ###############################
# -------------------------------------- #
##### 2. Modelling fledging survival #####
# -------------------------------------- #

##### * 2.1. Fledging survival: Beta-binomial GLMM -----------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Initial model fit ----
# __________________________________

# In accordance with previous diagnostics, I remove observations with brood_size = 0 since they
# likely are true outliers (see comments in the previous sections):
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
# overloading the script, but you can perfectly run it if you want. I thus only leave the code for
# the retained models.

# ## Fitting a regular binomial GLM:
# ttFS_bin_glm1 <- stats::glm(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                            clutch_size +
#                            urban_intensity + manag_intensity +
#                            light_pollution + noise_m + traffic +
#                            min_t_between + laying_day + year,
#                          weights = brood_size, # Prior weights!
#                          data = ntits3, family = "binomial") # Weights should not be forgotten.
# # Otherwise, the formulation should be: Y = cbind(fledgling_nb, brood_size-fledgling_nb)!
#
# ## Fitting a beta-binomial GLM:
# ttFS_bbin_glm1 <- stats::glm(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                clutch_size +
#                                urban_intensity + manag_intensity +
#                                light_pollution + noise_m + traffic +
#                                min_t_between + laying_day + year,
#                              weights = brood_size, # Prior weights!
#                              data = ntits3,
#                              family = glmmTMB::betabinomial(link = "logit"))
#
#
# ## Fitting a binomial GLMM:
# ttFS_bin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                   clutch_size +
#                                   urban_intensity + manag_intensity +
#                                   light_pollution + noise_m + traffic +
#                                   min_t_between + laying_day + year + (1|id_nestbox),
#                                 weights = brood_size, data = ntits3,
#                                 family = "binomial")
#
# ## Fitting a binomial GLMM with an OLRE:
# ttFS_bin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
#                                           log_F_metric_d2b1 + clutch_size +
#                                   urban_intensity + manag_intensity +
#                                   light_pollution + noise_m + traffic +
#                                   min_t_between + laying_day + year + (1|id_obs) + (1|id_nestbox),
#                                 weights = brood_size, data = ntits3,
#                                 family = "binomial")
#
# ## Fitting a beta-binomial GLMM:
# ttFS_bbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                   clutch_size +
#                                   urban_intensity + manag_intensity +
#                                   light_pollution + noise_m + traffic +
#                                   min_t_between + laying_day + year + (1|id_nestbox),
#                                 weights = brood_size, data = ntits3,
#                                 family = glmmTMB::betabinomial(link = "logit"))
#
#
# ## Fitting a zero-inflated (ZI) binomial GLMM:
# ttFS_zibin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
#                                     clutch_size +
#                                     urban_intensity + manag_intensity +
#                                     light_pollution + noise_m + traffic +
#                                     min_t_between + laying_day + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1) # Intercept only.
#
## Fitting a zero-inflated (ZI) binomial GLMM with OLRE:
# ttFS_zibin_glmm1_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
#                                             log_F_metric_d2b1 + clutch_size +
#                                     urban_intensity + manag_intensity +
#                                     light_pollution + noise_m + traffic +
#                                     min_t_between + laying_day + year + (1|id_obs) + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1) # Intercept only.

## Fitting a zero-inflated (ZI) beta-binomial GLMM:
ttFS_zibbin_glmm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1) # Intercept only.
# ntits3 %>% dplyr::mutate(c.clutch_size = clutch_size-stats::median(clutch_size)) -> ntits3
# ttFS_zibbin_glmm1_cent <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area +
#                                              c.log_F_metric_d2b1 +
#                                              c.clutch_size +
#                                              urban_intensity + manag_low + manag_high +
#                                              light_pollution + c.noise_m + c.traffic +
#                                              c.min_t_between + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = glmmTMB::betabinomial(link = "logit"),
#                                   ziformula = ~1) # Intercept only.
#
# ## Fitting a zero-inflated (ZI) beta-binomial GLM:
# ttFS_zibbin_glm1 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area +
#                                        log_F_metric_d2b1 + clutch_size +
#                                     urban_intensity + manag_intensity +
#                                     light_pollution + noise_m + traffic +
#                                     min_t_between + laying_day + year,
#                                   weights = brood_size, data = ntits3,
#                                   family = glmmTMB::betabinomial(link = "logit"),
#                                   ziformula = ~1) # Intercept only.
#
#
# ## Fitting an interactive (mediated) ZI-binomial GLMM:
# ttFS_zibin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
#                                        c.log_patch_area * c.log_F_metric_d2b1 +
#                                        clutch_size +
#                                        urban_intensity + manag_intensity +
#                                        light_pollution + noise_m + traffic +
#                                        min_t_between + laying_day + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1)
#
# ## Fitting an interactive (mediated) ZI-binomial GLMM with OLRE:
# ttFS_zibin_glmm2_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
#                                             c.log_patch_area * c.log_F_metric_d2b1 +
#                                             clutch_size +
#                                             urban_intensity + manag_intensity +
#                                             light_pollution + noise_m + traffic +
#                                             min_t_between + laying_day + year + (1|id_obs) + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = "binomial",
#                                   ziformula = ~1)

## Fitting an interactive (mediated) ZI-beta-binomial GLMM:
ttFS_zibbin_glmm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
                                        c.log_patch_area * c.log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1)
# ## Fitting an interactive (mediated) ZI-beta-binomial GLMM:
# ttFS_zibbin_glm2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~
#                                        c.log_patch_area * c.log_F_metric_d2b1 +
#                                        clutch_size +
#                                        urban_intensity + manag_intensity +
#                                        light_pollution + noise_m + traffic +
#                                        min_t_between + laying_day + year,
#                                   weights = brood_size, data = ntits3,
#                                   family = glmmTMB::betabinomial(link = "logit"),
#                                   ziformula = ~1)
# ttFS_zibbin_glmm2_cent <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area *
#                                              c.log_F_metric_d2b1 +
#                                             c.clutch_size +
#                                             urban_intensity + manag_low + manag_high +
#                                             light_pollution + c.noise_m + c.traffic +
#                                             c.min_t_between + year + (1|id_nestbox),
#                                   weights = brood_size, data = ntits3,
#                                   family = glmmTMB::betabinomial(link = "logit"),
#                                   ziformula = ~1)
# summary(ttFS_bin_glm1) # AIC = 2104.5.
# summary(ttFS_bbin_glm1) # AIC = NA (why?).
# summary(ttFS_bin_glmm1) # AIC = 1673.8.
# summary(ttFS_bin_glmm1_olre) # AIC = 1475.6.
# summary(ttFS_bbin_glmm1) # AIC = 1475.9.
#
# summary(ttFS_zibin_glmm1) # AIC = 1415.1.
# summary(ttFS_zibin_glmm1_olre) # AIC = 1382.3.
summary(ttFS_zibbin_glmm1) # AIC = 1379.5.
# summary(ttFS_zibbin_glmm1_cent)
# summary(ttFS_zibbin_glm1) # AIC = 1377.5.
#
# summary(ttFS_zibin_glmm2) # AIC = 1409.5 and significant interaction!
# summary(ttFS_zibin_glmm2_olre) # AIC = 1379.7 and significant interaction!
summary(ttFS_zibbin_glmm2) # AIC = 1376.5 and significant interaction!
# summary(ttFS_zibbin_glm2) # AIC = 1374.5 and significant interaction (and similar estimates to the GLMM too)!
# summary(ttFS_zibbin_glm2_cent)
# It seems that, the inclusion of a random effect (RE) strongly improved the fit when the ZI and
# overdispersion were not accounted for but not when they were. Accounting for the ZI and for overdispersion
# in the response, whether by using an OLRE or using a beta-binomial distribution, further improved the fit
# but it is not yet clear which one is best. Differences will have to be assessed through diagnostics. I will
# thus diagnose them all but, for the sake of brevity, I will not always show the code and comments for all.

# UPDATE: Diagnostics showed that using a beta-binomial distribution was the correct way to go and not using
# an OLRE. Since, as before, we explored a few reasonable variations of the same model by trying different
# proxies of the same variable or by tuning the ZI component of the model, we only try it for the beta-
# binomial specifications of the model, and mostly without RE to avoid convergence issues.
# Below, code and comments may be related to these exploratory models, but you can re-run the diagnostics for
# any models by replacing the model name in the code chunks.





### ** 2.1.2. Exploratory modelling ----
# ______________________________________

## Tuning the ZI part of the model:
ttFS_zibbin_glm1e <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between) # Intercept only.

ttFS_zibbin_glm1f <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~log_F_metric_d2b1) # Intercept only.

ttFS_zibbin_glm1g <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~log_patch_area) # Intercept only.

ttFS_zibbin_glm1h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between+urban_intensity+year) # Intercept only.

ttFS_zibbin_glmm1h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                         clutch_size +
                                         urban_intensity + manag_intensity +
                                         light_pollution + noise_m + traffic +
                                         min_t_between + laying_day + year + (1|id_nestbox),
                                       weights = brood_size, data = ntits3,
                                       family = glmmTMB::betabinomial(link = "logit"),
                                       ziformula = ~min_t_between+urban_intensity+year) # Intercept only.

ttFS_zibbin_glm1i <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~laying_day+urban_intensity+year) # Intercept only.
summary(ttFS_zibbin_glm1e) # AIC = 1374.5 vs 1377.5.
summary(ttFS_zibbin_glm1f) # AIC = 1375.2.
summary(ttFS_zibbin_glm1g) # AIC = 1376.8.
summary(ttFS_zibbin_glm1h) # AIC = 1364.7.
summary(ttFS_zibbin_glmm1h) # AIC = 1366.7 (mind the RE's uselessness).
summary(ttFS_zibbin_glm1i) # AIC = 1367.
# I also tried removing "traffic" and it doesn't change things much. When we use the "F-metric" along with
# "urban_intensity" to model the ZI-part of the model, the effect of the "F-metric" utterly disappear
# suggesting that connectivity does not influence total fledging failures and its effect in "F models"
# is actually a surrogate effect from "urban_intensity". So I'll only diagnose the "H models".
# Similarly, the surprising effects of the temperature proxies might actually be linked to humidity, a
# variable we could not measure.

ttFS_zibbin_glm2h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area * c.log_F_metric_d2b1 +
                                        clutch_size +
                                        urban_intensity + manag_intensity +
                                        light_pollution + noise_m + traffic +
                                        min_t_between + laying_day + year,
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~min_t_between+urban_intensity+year)
ttFS_zibbin_glmm2h <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ c.log_patch_area * c.log_F_metric_d2b1 +
                                         clutch_size +
                                         urban_intensity + manag_intensity +
                                         light_pollution + noise_m + traffic +
                                         min_t_between + laying_day + year + (1|id_nestbox),
                                       weights = brood_size, data = ntits3,
                                       family = glmmTMB::betabinomial(link = "logit"),
                                       ziformula = ~min_t_between+urban_intensity+year)
summary(ttFS_zibbin_glm2h) # AIC = 1361.8 vs 1374.5.
summary(ttFS_zibbin_glmm2h) # AIC = 1363.8 vs 1376.5.
# Note also that replacing "cumdd_before" by "min_t_before" does not change things much!


## Fitting a model without both CIMMDD_BETWEEN, MIN_T_BETWEEN and LAYING_DAY (just in case):
ttFS_zibbin_glm1_notemp <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                                              clutch_size +
                                              urban_intensity + manag_intensity +
                                              light_pollution + noise_m + traffic +
                                              year,
                                            weights = brood_size, data = ntits3,
                                            family = glmmTMB::betabinomial(link = "logit"),
                                            ziformula = ~1) # Intercept only.
summary(ttFS_zibbin_glm1_notemp) # AIC = 1381.2 vs 1377.5, so it worsen the fit (but makes "urban_intensity"
# significant)!





### ** 2.1.3. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.3.1. Residuals extraction, autocorrelation and collinearity ----
## Traditional residuals:
par(.pardefault)
resid <- stats::resid(ttFS_zibbin_glmm1, type = 'response')
plot(resid, id = 0.05, idLabels = ~.obs) # Strange distribution of residuals with 2 groups.
# performance::check_outliers(ttFS_zibbin_glmm1) # Does not work for this type of model.
ntits3[which(resid < -0.4),] # Nestboxes with the lowest residuals = ~0% fledging survival!

# To further investigate patterns, I can plot the residuals against some predictors:
plot(x = ntits3$noise_m, y = resid) # Seems rather ok although we once again find patterns linked to the
# sometimes odd distribution of some predictors. However, be reminded that simulated residuals will be
# more useful).
# plot(ttHSy_ziglmm1, id_nestbox~stats::resid(.)) # Does not work for this type of model.
# plot(ttHSy_ziglmm1, site~stats::resid(.)) # Does not work for this type of model.

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm1, n = 1000, re.form = NULL)
simu.residh <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm1h, n = 1000, re.form = NULL)
simu.resid2 <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm2, n = 1000, re.form = NULL)
simu.resid2h <- DHARMa::simulateResiduals(fittedModel = ttFS_zibbin_glmm2h, n = 1000, re.form = NULL)
simu.resido <- DHARMa::simulateResiduals(fittedModel = ttFS_zibin_glmm1_olre,
                                         n = 1000, re.form = NULL)
simu.resido2 <- DHARMa::simulateResiduals(fittedModel = ttFS_zibin_glmm2_olre,
                                          n = 1000, re.form = NULL) # The 're.form' argument is to base
# simulations on the model unconditional of the random effects (and only works for {lme4} formulations). It
# is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Ok.
plot(simu.residh) # Ok.
plot(simu.resid2) # Ok.
plot(simu.resid2h) # Ok.
plot(simu.resido) # Ok.
plot(simu.resido2) # Ok.
DHARMa::outliers(simu.resid) # Ok.
DHARMa::outliers(simu.residh) # Ok.
DHARMa::outliers(simu.resid2) # Ok.
DHARMa::outliers(simu.resid2h) # Ok.
DHARMa::outliers(simu.resido) # Ok.
DHARMa::outliers(simu.resido2) # Ok.

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Spatial auto-
# correlation detected only for the OLRE models!
performance::check_autocorrelation(ttFS_zibbin_glmm1) # Ok.
performance::check_collinearity(ttFS_zibbin_glmm1h) # Ok for the beta-binomial models (highest VIF value for
# the first ZIBBIN = 3.44 for the F-metric while it was 4.03 for the second model), even the exploratory ones.
# For the OLRE models however, alarming multicollinearity issues have been detected with a moderate correlation
# linked to "clutch_size" being present, several VIF values > 4 and even 8.5 for the "F-metric" (even though
# things were a bit better for the interactive model).
# Interestingly, no multicollinearity was detected for the BBIN or the BIN_OLRE models either.
stats::vcov(ttFS_zibbin_glmm1) # But values of the covariance matrix seem ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_patch_perim)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_woody_area)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_woody_vw)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resido2, form = ntits3$Rr_metric_d2c1)
DHARMa::plotResiduals(simu.resido2, form = ntits3$species)
DHARMa::plotResiduals(simu.resido2, form = ntits3$clutch_size)
DHARMa::plotResiduals(simu.resido2, form = ntits3$urban_intensity) # Deviation detected (for OLRE model1)!
DHARMa::plotResiduals(simu.resido2, form = ntits3$manag_intensity)
DHARMa::plotResiduals(simu.resido2, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resido2, form = ntits3$sqrt_built_vol) # Deviation detected (for OLRE model1)!
DHARMa::plotResiduals(simu.resido2, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resido2, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resido2, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resido2, form = ntits3$cumdd_30)
DHARMa::plotResiduals(simu.resido2, form = ntits3$cumdd_between)
DHARMa::plotResiduals(simu.resido2, form = ntits3$min_t_between)
DHARMa::plotResiduals(simu.resido2, form = ntits3$laying_day)
DHARMa::plotResiduals(simu.resido2, form = ntits3$year)
# For the ZIBBIN models, no deviations were detected. For the models with OLRE, worrying deviations have been
# found for "urban_intensity" (likely linked to the "built volume").
# For the exploratory models, everything seems fine.



### *** 2.1.3.2. Distribution and dispersion ----
## Assessing over or under-dispersion:
DHARMa::testDispersion(simu.resid) # Ok for all models.
performance::check_overdispersion(x = ttFS_zibbin_glmm1) # Overdispersion detected! However, note that this test
# is known to be inaccurate for ZI-models (see the function's help page). If I run it on the non-ZI model
# (i.e. 'ttFS_bbin_glmm1'), the dispersion ratio is much lower, yet we may still need to account for that.
# ATTENTION: Classical overdispersion tests cannot be used to detect overdispersion when OLRE is used to
# account for it, and the same is likely true for beta-binomial models. So I'll ignore it now.

## Distribution of the predicted probabilities:
probabilities <- stats::predict(object = ttFS_zibbin_glmm2h, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(probabilities, main = "Predicted proportions", xlab = "fledging survival")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
par(.pardefault)
# The ZIBBIN models globally fail to correctly predict the data. Prediction ranges are too narrow, the mode is
# around 0.7 instead of 1 and models do not predict total successes or failures (even though the interaction
# model does a little better). The models with OLRE predict slightly better than the ZIBBIN (mode at 0.8) but
# it's still not very satisfactory.
# Interestingly, the overdispersed binomial GLMM without ZI ('ttFS_bin_glmm1') does a better job at predicting
# the observed data than any other model, and the binomial GLMM without ZI but with OLRE ('ttFS_bin_glmm1_olre')
# predicts the data very well (see below)!

## Zero-inflation:
DHARMa::testZeroInflation(simu.resid_nozi) # Significant for the non-ZI models!
# Testing with the non-ZI GLMM:
simu.resid_nozi <- DHARMa::simulateResiduals(fittedModel = ttFS_bin_glmm1_olre, n = 1000)
plot(simu.resid_nozi) # Slight or strong deviations detected for the non-ZI models (depending on the model)!
noziprobabilities <- stats::predict(object = ttFS_bin_glmm1_olre, type = "response") # Extract the predicted
# probabilities.
par(mfrow= c(1,2))
hist(noziprobabilities, main = "Predicted proportions (wo ZI)", xlab = "fledging survival")
hist(ntits3$fledgling_nb/ntits3$brood_size, main = "Observed proportions", xlab = "Fledging rate")
# Surprisingly, the model seems to better fit the observed proportions, even though it still under-predicts
# the proportion of failures.



### *** 2.1.3.3. Linearity ----
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
probabilities <- stats::predict(object = ttFS_zibbin_glmm1, type = "response") # Extract probabilities!
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
# "min_t_between" which is U-shaped (a transformation could perhaps be used).



### *** 2.1.3.4. Goodness-of-fit (GOF) and performances ----
## GOF test of Pearson's Chi2 residuals:
dat.resid <- sum(stats::resid(ttFS_zibbin_glmm1, type = "pearson")^2)
1 - stats::pchisq(dat.resid, stats::df.residual(ttFS_zibbin_glmm1)) # p = 0 (mistake?).

## Computing a pseudo-R2:
performance::r2_nakagawa(ttFS_zibbin_glmm1, tolerance = 0.00000001)
# [Additive model]: Marg_R2_glmm = 0.8; Cond_R2_glmm = 0.8 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibbin_glmm1h, tolerance = 0.00000001)
# [Additive model]: Marg_R2_glmm = 0.79; Cond_R2_glmm = NA (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibbin_glmm2, tolerance = 0.00000001)
# [Interactive model]: Marg_R2_glmm = 0.8; Cond_R2_glmm = 0.8 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibbin_glmm2h, tolerance = 0.00000001)
# [Interactive model]: Marg_R2_glmm = 0.8; Cond_R2_glmm = 0.8 (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibin_glmm1_olre, tolerance = 0.00000001)
# [Additive model]: Marg_R2_glmm = 0.17; Cond_R2_glmm = NA (but may be unreliable due to difficulties to
# compute RE variances)!
performance::r2_nakagawa(ttFS_zibin_glmm2_olre, tolerance = 0.00000001)
# [Interactive model]: Marg_R2_glmm = 0.16; Cond_R2_glmm = 0.16 (but may be unreliable due to difficulties to
# compute RE variances)!

## Likelihood-based evaluation of effects inclusion:
zzz <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                          clutch_size +
                          urban_intensity + manag_intensity +
                          light_pollution + noise_m + traffic +
                          cumdd_between + year,
                        weights = brood_size, data = ntits3,
                        family = glmmTMB::betabinomial(link = "logit"),
                        ziformula = ~1) # Intercept only.
zzz2 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ log_patch_area + log_F_metric_d2b1 +
                           clutch_size +
                           urban_intensity + manag_intensity +
                           light_pollution + noise_m + traffic +
                           cumdd_between + year + (1|id_obs),
                         weights = brood_size, data = ntits3,
                         family = "binomial",
                         ziformula = ~1) # Intercept only.
summary(zzz) # Beta-binomial models: The non-mixed model gives AIC = 1378.2 while the mixed-model gave an AIC
# of 1380.2 (with "id_nestbox"), or 1380.2 (with "id_patch"), or 1380 (with "site"), or 1382 (with "site"
# and "id_nestbox") or 1382.2 (with "id_patch" and "id_nestbox")! It thus appears that the the use of
# RE is not useful here!
summary(zzz2) # OLRE models: The model without other RE than the OLRE gives AIC = 1379.6 while the RE-model
# gave an AIC of 1381.6 (with "id_nestbox"), or 1381.6 (with "id_patch"), or 1381.6 (with "site"), or 1383.6
# (with "site" and "id_nestbox") or 1383.6 (with "id_patch" and "id_nestbox")! It thus appears that the the use
# of additional RE is not useful here!

# Importance of the fixed effects:
ttFS_zibbin_glmm0 <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox),
                                      weights = brood_size, data = ntits3,
                                      family = glmmTMB::betabinomial(link = "logit"),
                                      ziformula = ~1)
ttFS_zibin_glmm0_olre <- glmmTMB::glmmTMB(fledgling_nb/brood_size ~ 1 + (1|id_nestbox) + (1|id_obs),
                                          weights = brood_size, data = ntits3,
                                          family = "binomial",
                                          ziformula = ~1)
summary(ttFS_zibbin_glmm0) # AIC = 1462.9 vs 1380.2, so the full model is clearly far better!
summary(ttFS_zibin_glmm0_olre) # AIC = 1473.5 vs 1381.6, so the full model is clearly far better!





### ** 2.1.4. Inference and predictions ----
# __________________________________________

##### TO BE RUN §§§ -----

### *** 2.1.4.1. Hypotheses testing: LRT for the additive and interactive effect of the F-metric ----
## For the additive effect of the connectivity metric:
ttFS_zibbin_glmm0 <- stats::update(ttFS_zibbin_glmm1, .~. -log_F_metric_d2b1)
summary(ttFS_zibbin_glmm0) # AIC = 1383.3 vs 1379.5 (hypothesis 1 likely validated)!

## Regular LRT (temporary results):
res.LRT_hypo1 <- stats::anova(object = ttFS_zibbin_glmm0, ttFS_zibbin_glmm1, test = "LRT")
res.LRT_hypo2 <- stats::anova(object = ttFS_zibbin_glmm1, ttFS_zibbin_glmm2, test = "LRT")

## I do not run PB-based LRT for now as they take too long to run.

# res.LRT_addeff <- pbkrtest::PBmodcomp(ttFS_ziglmm1,
#                                       ttFS_ziglmm0, nsim = 500, seed = 56) # Took ~??? to run!
# readr::write_csv2(x = res.LRT_addeff$test, file = here::here("output", "tables",
#                                                              "res.ntitsFSy_LRT_addeff.csv"))
# # The LRT is not significant, indicating that our connectivity metric does not improve the description of
# # the data here.

## For the interaction effect:
# Since even the additive model is NOT SIGNIFICANT, there is no point in testing the effect of the
# interactive (mediated) model.



### *** 2.1.4.2. Bootstrapped confidence intervals for estimated parameters ----
res.ttFSy_addeff_CI <- confint(ttFS_zibbin_glmm1)
tt <- as.data.frame(res.ttFSy_addeff_CI)
tt$parameters <- rownames(tt)
readr::write_csv2(x = tt,
                  file = here::here("output", "tables", "res.ttFSy_regCI_addeff.csv"))
#
# tictoc::tic("Bootstrap CI for additive GLMM parameters")
# res.ntitsFSy_addeff_CI_boot <- confint(ttFS_zibbin_glmm1, method="boot")
# tt <- as.data.frame(res.ntitsFSy_addeff_CI_boot)
# tt$parameters <- rownames(tt)
# readr::write_csv2(x = tt,
#                   file = here::here("output", "tables", "res.ntitsFSy_bootCI_addeff.csv"))
# tictoc::toc() # DISCLAIMER: took ~1h45 to run!



### *** XXXXXXX Plotting results ----
# Plots without INTERCEPT!
tt2 <- tibble::as_tibble(tt)
tt2 %>% tibble::add_row("2.5 %" = 0, "97.5 %" = 0, parameters = "species",.after = 3) %>%
  as.data.frame() -> tt2

uu2 <- tibble::as_tibble(ttFS_zibbin_glmm1$fit$par[1:15])
uu2 %>% tibble::add_row(value = 0,.after = 3) %>%
  as.data.frame() -> uu2

base_data <- tibble::tibble(
  parameters = c("Intercept", "Local patch area", "Flux metric", "Species (Cyanistes caeruleus)", "Clutch size",
                 "Urban intensity", "Management intensity (moderate)", "Management intensity (intensive)",
                 "Light pollution", "Noise pollution", "Traffic", "Temperature", "Laying day", "Year (2020)",
                 "Year (2021)", "Year (2022)"),
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



### *** 2.1.4.3. Conclusion ----
# For the initial model:
summary(ttFS_zibbin_glmm1) # AIC = 1379.5 and both R2_glmm = 0.8!
summary(ttFS_zibbin_glmm2) # AIC = 1376.5 and both R2_glmm = 0.8!
# Diagnostics ran for these initial models indicated that the model fit the data relatively well although
# prediction ranges are too narrow and the models fail to predict total failures and successes! It is
# possible that the ZI forces the models to predict medium values.
# Interestingly and inexplicably, some of the models without ZI (especially the binomial GLMM with OLRE)
# predicted the observed data far better but yielded worrying diagnostics and had lower AIC values than the
# retained models.Remember that the retained models use a beta-binomial family as the response displayed
# overdispersion without it. Models that did not account for this overdispersion also raised other red flags.
# As recommended by Harrison (2014), we compared the beta-binomial models with models including OLRE but the
# latter models also displayed some problematic issues such as concerning multicollinearity, autocorrelation
# and quantile deviations. Diagnostics from the beta-binomial models were all satisfactory if we excluded
# "species" from the models as it is collinear with "clutch_size". We thus chose to keep "clutch_size" as
# it is a more informative predictor and yielded better diagnostics. However, it should be noted that there
# was a species effect as blue tits tended to have a lower fledging survival than Parus major:
ntits3 %>% dplyr::mutate(fledging_success = fledgling_nb/clutch_size) %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(mean_fs = mean(fledging_success)) # Note that this mean fledging survival does not account
# for the effect of the covariates unlike a coefficient estimate (you could alternatively fit a model
# replacing "clutch_size" by "species" or using both, to estimate a more accurate effect).
# Finally, it also appears that for this response variable the use of mixed models was not necessary.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), laying_day (--), and year2022 (+++).
## For the interactive (moderated) model, the INTERACTION TERM was also significant (--) but the F-metric was
# not anymore, which could be a sign of a cross-over interaction. Furthermore, light_pollution (-) also
# turned out significant in the interactive model (with only a trend in the additive one)!
## Hypothesis 1 and 2 possibly validated (AIC = 1383.3 vs 1379.5)!


# For the exploratory models:
summary(ttFS_zibbin_glmm1h) # AIC = 1366.7 and both R2_glmm = 0.79.
## Significant variables: F-metric (++), clutch_size (-), manag_high (--), laying_day (--), and year2022 (+++),
# and also [urban_intensity (+), year2021 and year2022 (---) for the ZI component]!
## Almost significant variables: [min_t_between (+) for the ZI component]!
summary(ttFS_zibbin_glmm2h) # AIC = 1363.8 and both R2_glmm = 0.8.
## Significant variables: clutch_size (-), manag_high (--), light_pollution (-), laying_day (--) and
# year2022 (+++), and the INTERACTION EFFECT (--); and also [urban_intensity (+), year2021 and year2022 (---)
# for the ZI component]!
## Almost significant variables: the F-metric (+) and also [min_t_between (+) for the ZI component]!
## Hypothesis 2 likely validated with a possible cross-over interaction!
# Diagnostics for these models were mostly ok, there was no outliers, deviations, dispersion or
# distributional problems.
# Predictions were fairly ok but the models still made too narrow predictions and failed to correctly
# predict total successes and failures.

# I also tried removing "traffic" and it didn't change things much. When we used the "F-metric" along with
# "urban_intensity" to model the ZI-part of the model, the effect of the "F-metric" utterly disappeared
# suggesting that connectivity does not influence total fledging failures and its effect in "F models" was
# actually a surrogate effect from "urban_intensity".

## Since the interaction effects turned out significant, we have to explore them graphically to interpret them!



### *** 2.1.4.4. Interaction model exploration ----
## Creating a 2D interaction plot:
sjPlot::plot_model(ttFS_zibbin_glmm2, type = "pred",
                   terms = c("c.log_patch_area [all]",
                             "c.log_F_metric_d2b1 [-1.82, -0.3, 0, 0.32, 1.12]"),
                   mdrt.values = "quart", # Only useful if `type = "int"` is used (i.e. automatic plotting
                   # of interaction effects). If used, only plots the 3 quartile values for the moderator.
                   title = "", # If I don't let it blank (and delete it), it sets a title by default.
                   axis.title = c("Local patch area",
                                  "Predicted fledging survival"),
                   legend.title = "Flux metric",
                   colors = c("darkred", "white", "darkorange", "darkolivegreen3", "chartreuse4"), # For
                   # a reason I don't understand, the 2nd colour in the vector is not taken into account!
                   show.data = TRUE, # Not available, I don't know why.
                   line.size = 1)
summary(ttFS_zibbin_glmm2)
summary(ntits3)


## Creating a 3D interaction plot:
x_tilde <- expand.grid(c.log_patch_area = seq(-2.26,1.184, length.out=15),
                       c.log_F_metric_d2b1 = seq(-1.822,1.125, length.out=15), # Computes every combination
                       # for the input variables (here a sequence of 15 values roughly ranging the actual
                       # values of the two predictors involved in the modelled interaction).
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
                       id_nestbox = "DIJ-071") # Here, I used approximately median values for all the other predictors
# or for selected factors (e.g. DIJ-071 is for a nestbox in the old city).
x_tilde$fledging_rate <- predict(ttFS_zibbin_glmm2, x_tilde, type="response")

lattice::trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))
lattice::wireframe(fledging_rate ~ c.log_patch_area + c.log_F_metric_d2b1, data=x_tilde,
                   main = "Predicted fledging rate",
                   drape = TRUE,
                   colorkey = TRUE,
                   zlab = list("Fledging rate", cex=1.3, rot=90),
                   xlab = list("Patch area", cex=1.3, rot=-40),
                   ylab = list("F-metric", cex=1.3, rot=17),
                   scales = list(arrows=FALSE, cex=1, tick.number = 10,
                                 z = list(arrows=F),
                                 distance = c(1, 1, 1)), # Control the distance of axis labels.
                   light.source = c(10,0,10),
                   col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100,
                                         alpha = .5), # Controls the transparency.
                   screen = list(z = -60, x = -75)) # Z controls the rotation.


