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
plot(simu.resid) # Ok-ish but some slight deviations exist.
DHARMa::outliers(simu.resid) # No potential outliers.
# ntits2[c(156,170,181,210,227,314,362,367),] # [Note made previously to outliers removal] They have
# surprisingly low clutch sizes with regards to their locations and their adjacent observations. They may
# well be true outliers (whose clutch sizes are function of other processes).

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = ntits3$coord_x, y = ntits3$coord_y, plot = TRUE) # Ok-ish.
performance::check_autocorrelation(ttCy_comglmm1) # Ok.
performance::check_collinearity(ttCy_comglmm1) # Ok-ish, but "Fmetric" > 4.
stats::vcov(ttCy_comglmm1) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_patch_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$log_F_metric_d2b1)
DHARMa::plotResiduals(simu.resid, form = ntits3$urban_intensity)
DHARMa::plotResiduals(simu.resid, form = ntits3$manag_intensity) # Slight heteroscedasticity detected.
DHARMa::plotResiduals(simu.resid, form = ntits3$log_herb_area)
DHARMa::plotResiduals(simu.resid, form = ntits3$light_pollution)
DHARMa::plotResiduals(simu.resid, form = ntits3$noise_m)
DHARMa::plotResiduals(simu.resid, form = ntits3$traffic)
DHARMa::plotResiduals(simu.resid, form = ntits3$cumdd_30) # Slight deviation (likely due to the mild
# collinearity with "laying_day").
DHARMa::plotResiduals(simu.resid, form = ntits3$laying_day)
DHARMa::plotResiduals(simu.resid, form = ntits3$species)
DHARMa::plotResiduals(simu.resid, form = ntits3$year)
# Overall, these plots are ok. The two deviations detected are quite mild and disappear (while keeping
# quite similar coefficient estimates and standard errors) if we respecify the model, which lead me to
# think that they are acceptable.



### *** 1.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = ttCy_glm1, alternative = c("less")) # Significant underdispersion (note that
# this is the Poisson model that is tested, hence the later use of a COM-Poisson distribution)!
DHARMa::testDispersion(simu.resid, alternative = "less") # Ok.

## Theoretical count distribution:
theo_count <- COMPoissonReg::rcmp(n = nrow(ntits3), lambda = mean(ntits3$clutch_size), nu = 1.05) # The 'nu'
# parameter should be chosen by trial-and-errors.
tc_df <- data.frame(theo_count)

ggplot2::ggplot(ntits3, ggplot2::aes(clutch_size)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!

## Distribution of the predicted counts:
pred_counts <- stats::predict(object = ttCy_comglmm1, type = "response") # Extract the predicted counts.
par(mfrow= c(1,2))
hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
hist(ntits3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# are very similar and relatively acceptable (although too narrow).



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
1 - stats::pchisq(dat.resid, stats::df.residual(ttCy_comglmm1)) # p = 0.83, indicating that there is no
# significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# topic and interpretations are not straightforward.

# Computing a pseudo-R2:
performance::r2_nakagawa(ttCy_comglmm1) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.11.
# Does not work if we tune the dispersion parameter of the COM-Poisson model.

## Likelihood-based evaluation of effects inclusion:
# For the "site" random-effects (RE):
ttCy_comglmm1ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
                                      urban_intensity + manag_intensity +
                                      light_pollution + noise_m + traffic +
                                      cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
                                    data = ntits3, family = glmmTMB::compois(link = "log"),
                                    dispformula = ~1) # Rather long to fit (~3-4 min)!
summary(ttCy_comglmm1ac) # AIC = 1390.6.
# The non-mixed model gives AIC = 1391.7, so similar to the mixed-model (AIC = 1391.3) with only
# "id_nestbox" as RE. The one with both RE gives AIC = 1390.6., so it seems like the use of a mixed model
# is not truly supported by the data.

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
par(las=4,bty="l")
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
# These three examples confirm that the model tends to overpredict a bit.

