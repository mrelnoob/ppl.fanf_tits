# ---------------------------------------------------------------------------------- #
#####         Functions to clean and prepare my datasets for analysis            #####
# ---------------------------------------------------------------------------------- #

# The functions of this R file are meant to prepare datasets: e.g. aggregate observations,
# delete or add variables, etc. But NOT to directly explore and cure data for analyses!


### Script preparations ________________________________________________________
# I first need to source (call) my previous R scripts to be able to call their functions:
source(file = here::here("R/01_01_importing_data.R"))
source(file = here::here("R/01_02_utility_functions.R"))

library(magrittr) # The only library that I truly need to load (in order to be able to use pipes).

utils::globalVariables("where") # This is necessary for now as tidyselect::where is not an
# exported function!



### ____________________________________________________________________
#' Update and export the tits dataset with temperature related variables
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{first} of a series of functions meant to update and complete
#' the tits dataset with its \emph{independent variables} (i.e. predictors and covariates).
#' The `tdata_upD_temp` function modifies the \emph{aggregated tits dataset} in several ways:
#' * First, it assigns to each observation (nestbox for a given year) the temperature station
#' it is paired with (giving the new column "\emph{temp_station_id}").
#' * Second, it reorganizes the date columns (deleting 2 and updating the others).
#' * Third, it creates a new `factor` variable called "\emph{breeding window}" that indicates
#' whether the reproduction event occurred at the beginning or at the end of the breeding season.
#' * Fourth, it computes seven \strong{temperature related} variables. These variables are
#' \emph{cumdd_30} (the cumulative day-degrees for the 30 days prior to the laying date);
#' \emph{cumdd_60} (the cumulative day-degrees for the 60 days prior to the laying date);
#' \emph{cumdd_between} (the cumulative day-degrees for the period between the laying and the
#' flight dates); \emph{min_t_before} and \emph{min_t_between} (which are the minimum temperature
#' recorded during the 30 days prior to the laying date and between the laying and the flight
#' dates, respectively); \emph{mean_winter_t} and \emph{sd_winter_t}) (which are the mean
#' recorded temperature during the four month of winter across 2019-2022 and its standard
#' deviation, respectively).
#'
#' @param myboxtemp_data The path to the nestbox-temperature stations pairing dataset (.csv).
#' @param mytemp_data The path to the temperature records dataset (.csv).
#'
#' @return An updated version of the tits dataset.
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_double
#' @importFrom readr write_csv2
#' @importFrom here here
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr relocate
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom stringr str_detect
#' @importFrom degday dd_calc
#'
#' @examples
#' \dontrun{
#' mydata <- tdata_upD_temp()
#' }
tdata_upD_temp <- function(myboxtemp_data = here::here("data", "paired_boxtemp.csv"),
                           mytemp_data = here::here("data", "temp_data_20192022.csv")){
  ##### Attribute ID to nest_years and improve formatting
  # _____________________________________________________

  ### Setting the seed for random number generations____________________________#
  set.seed(93)

  ### Assigning temp_stations to each observation and improving dates format____#
  loco <- readr::read_csv2(myboxtemp_data, col_names = TRUE, na = "NA",
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   lcz1 = readr::col_factor(),
                                                   dist_lcz = readr::col_double(),
                                                   temp_id_lcz = readr::col_factor(),
                                                   dist_pp = readr::col_double(),
                                                   temp_id_pp = readr::col_factor(),
                                                   temp_id_final = readr::col_factor()))
  tits <- aggreg_by_nest() # To load the tits data.
  tits$temp_station_id <- loco$temp_id_final

  tits %>%
    tidyr::separate(laying_date, c('day2', 'month2', 'year2'), sep = "/") %>%
    tidyr::unite(laying_date, c(year2, month2, day2), sep = "-", na.rm = TRUE) %>%
    tidyr::separate(date, c('day3', 'month3', 'year3'), sep = "/") %>%
    tidyr::unite(flight_date, c(year3, month3, day3), sep = "-") %>%
    dplyr::mutate(laying_date = as.Date(x = laying_date, optional = TRUE),
                  flight_date = as.Date(x = flight_date, optional = TRUE)) -> tits

  # Correcting wrong dates:
  tits[which(tits$id_nestbox == "DIJ-102" & tits$year == "2021"),
       "laying_date"] <- as.Date("2021-04-04")
  tits[which(tits$id_nestbox == "DIJ-200" & tits$year == "2022"),
       "laying_date"] <- as.Date("2022-04-04")



  ### Creation of the "breeding_window" random factor___________________________#
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mid_date = stats::median(laying_date, na.rm = TRUE)) -> median_laydate
  # NOTE: this object as well as the following (mrd) are not chronological! They're based on
  # the order of appearance of years in "tits": so the order is (currently) 2020, 2021, 2019 and
  # 2022. If we import data from additional years (that could change the order), I will have
  # to carefully check that my code works as expected! Because of this error, I call these
  # objects (median_laydate and mrd) in non-chronological order in the next code lines!

  # Imputation of the laying_date missing values (n=9) and correcting wrong "flight_date" for
  # clutches that failed:
  tits %>% dplyr::group_by(year) %>%
    dplyr::summarise(mean_repro_duration = mean(
      flight_date, na.rm = TRUE) - mean(laying_date, na.rm = TRUE)) -> mrd

  tits %>% dplyr::mutate(laying_date = dplyr::case_when(
    year == "2019" & is.na(laying_date) == TRUE ~
      as.Date(flight_date - mrd$mean_repro_duration[3]),
    year == "2019" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2020" & is.na(laying_date) == TRUE ~
      as.Date(flight_date - mrd$mean_repro_duration[1]),
    year == "2020" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2021" & is.na(laying_date) == TRUE ~
      as.Date(flight_date - mrd$mean_repro_duration[2]),
    year == "2021" & is.na(laying_date) == FALSE ~ laying_date,
    year == "2022" & is.na(laying_date) == TRUE ~
      as.Date(flight_date - mrd$mean_repro_duration[4]),
    year == "2022" & is.na(laying_date) == FALSE ~ laying_date)) -> tits

  tits %>% dplyr::mutate(
    flight_date = dplyr::case_when(
      brood_size != 0 ~ as.Date(flight_date),
      brood_size == 0 ~ as.Date(laying_date + 30))) -> tits

  # Random factor generation
  tits %>% dplyr::mutate(breeding_window = dplyr::case_when(
    year == "2019" & laying_date >= as.Date(median_laydate$mid_date[3]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2019" & laying_date < as.Date(median_laydate$mid_date[3]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2020" & laying_date >= as.Date(median_laydate$mid_date[1]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2020" & laying_date < as.Date(median_laydate$mid_date[1]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2021" & laying_date >= as.Date(median_laydate$mid_date[2]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2021" & laying_date < as.Date(median_laydate$mid_date[2]) ~ paste("early", year,
                                                                               sep = "_"),
    year == "2022" & laying_date >= as.Date(median_laydate$mid_date[4]) ~ paste("late", year,
                                                                                sep = "_"),
    year == "2022" & laying_date < as.Date(median_laydate$mid_date[4]) ~ paste("early", year,
                                                                               sep = "_"))) %>%
    dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
    dplyr::relocate(breeding_window, .after = year) %>%
    dplyr::relocate(laying_date, .after = breeding_window) %>%
    dplyr::relocate(flight_date, .after = hatching_date) %>%
    dplyr::select(-incubation_date, -hatching_date) -> tits





  ##### Format TEMPERATURE data and compute daily mean temperatures and range
  # _________________________________________________________________________

  ### Import of TEMP data and creation of the subtables_________________________#
  temp <- readr::read_csv2(mytemp_data, col_names = TRUE,
                           col_types = readr::cols(time = readr::col_factor()))
  temp %>%
    tidyr::separate(time, c('date', 'hour'), sep = " ") %>%
    tidyr::separate(date, c('day', 'month', 'year'), sep = "/") %>%
    dplyr::select(-s69) %>% # Empty column
    dplyr::mutate(dplyr::across(where(is.character), factor)) %>%
    dplyr::filter(month == "12" | month == "01" | month == "02" | month == "03" |
                    month == "04" | month == "05" | month == "06") %>%
    tidyr::unite(date, c(year, month, day), sep = "-", remove = FALSE) %>%
    dplyr::mutate(date = as.Date(date)) -> temp

  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(min_t = min,
                                          max_t = max))) -> daily_t_range
  # Globally, this code computes the daily temperature min and max. More precisely:
  #     - I grouped the hourly temperature values (readings) for each day of the month and each
  #       month of the year (=date).
  #     - I then SUMMARISED the daily MIN and MAX temperature values ACROSS all NUMERIC columns
  #       while assigning suffixes to each temperature station for the MIN and MAX values,
  #       respectively (doubling the number of columns).
  # NOTE: however, I could not figure out how to disregard missing values so there are more NAs
  # than expected (if there was any NA hourly reading for a given day, the daily value will also
  # be NA).
  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(min_t = min))) -> daily_t_min
  temp %>% dplyr::group_by(date) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), .fns =
                                     list(max_t = max))) -> daily_t_max

  # Function to compute daily mean for all stations:
  daily_t_mean <- daily_t_range[,1]
  station_name <- c("date", colnames(temp[,6:ncol(temp)]))

  dname <- "date"

  for(k in 1:70){

    k_name <- station_name[1+k]

    jjj <- cbind(daily_t_range[,1], daily_t_min[,1+k], daily_t_max[,1+k])
    colnames(jjj) <- c("date", "min_t", "max_t")

    jjj %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(mean_t = mean(c(min_t, max_t))) -> kkk
    daily_t_mean[,1+k] <- kkk[,2]

    dname[1+k] <- paste(k_name, "mean_t", sep = "_")
    colnames(daily_t_mean) <- dname
  }





  ##### Computing averages, cumulative day-degrees, minimum temperatures, etc.
  # __________________________________________________________________________

  # ______
  ### List of the variables that will be created___#
  cumdd_30d <- NULL
  cumdd_60d <- NULL
  cumdd_between <- NULL
  min_t_before <- NULL
  min_t_between <- NULL
  mean_winter_t <- NULL
  sd_winter_t <- NULL # For simplicity, I'll reorganize them later.

  # Subtable for MEAN_WINTER_T and SD_WINTER_T (mean winter temperature across 2019-2022):
  daily_t_mean %>%
    tidyr::separate(date, c('year', 'month', 'day'), sep = "-") %>%
    dplyr::filter(month == "01" | month == "02" | month == "03" |
                    month == "12") -> mean_t_winter
  # Note: I place this here because, contrarily to the other subtables, this one does
  # not require a "i" (it doesn't depend on a specific date) so it can be computed just once!


  # _______
  ### Heavy "for-loop" to compute each variable for each line (1...i...n)___#
  for(i in 1:nrow(tits)){

    # ____________
    # ____________
    date_i <- tits[i, 4] # Laying date.
    fdate_i <- tits[i, 5] # Fledging date.
    year_i <- tits[i, 2]
    station_i <- as.character(as.matrix(tits[i, 16])) # Temperature station ID. Note: bloody R
    # doesn't want to extract the factor's value directly!
    nn <- which(stringr::str_detect(string = station_name, pattern = station_i)) # To extract
    # the position (in the 'station_name' string) of the i-th temperature station.


    # ____________
    # ____________
    ### Sub-tables for CUMDD_30D and MIN_T_BEFORE (it's actually the same table)
    # Daily temperature minimal values for the 30 days preceding the i-th laying date:
    daily_t_min %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-31) -> mw30_min

    # Daily temperature maximal values for the 30 days preceding the i-th laying date:
    daily_t_max %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-31) -> mw30_max

    # ____________
    ### Sub-tables for CUMDD_60D
    # Daily temperature minimal values for the 60 days preceding the i-th laying date:
    daily_t_min %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-61) -> mw60_min

    # Daily temperature maximal values for the 60 days preceding the i-th laying date:
    daily_t_max %>%
      dplyr::filter(date < date_i) %>%
      dplyr::filter(date > date_i-61) -> mw60_max

    # ____________
    ### Sub-tables for CUMDD_BETWEEN and MIN_T_BETWEEN (it's actually the same table)
    # Daily temperature minimal values between the i-th laying and flight dates:
    daily_t_min %>%
      dplyr::filter(date > date_i) %>%
      dplyr::filter(date < fdate_i) -> mwbetween_min

    # Daily temperature maximal values between the i-th laying and flight dates:
    daily_t_max %>%
      dplyr::filter(date > date_i) %>%
      dplyr::filter(date < fdate_i) -> mwbetween_max


    # ____________
    # ____________
    ### Extracting values from the right station
    ttt30 <- cbind(mw30_min[, nn], mw30_max[, nn])
    ttt60 <- cbind(mw60_min[, nn], mw60_max[, nn])
    tttbet <- cbind(mwbetween_min[, nn], mwbetween_max[, nn])
    tttwinter <- mean_t_winter[, 2+nn] # "2+" because this table has 2 more front columns.
    colnames(ttt30) <- c("mw30_min_t", "mw30_max_t")
    colnames(ttt60) <- c("mw60_min_t", "mw60_max_t")
    colnames(tttbet) <- c("mwbet_min_t", "mwbet_max_t")
    colnames(tttwinter) <- "winter_mean_t"

    # _________
    # Computing CUMDD_30D and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= length(ttt30$mw30_min_t)*0.9) { # I consider "NA" if there
      # are more than 90% of observations missing because imputing missing values would be too random
      # otherwise.
      cumdd_30d[i] <- "NA"
    } else if (sum(is.na(ttt30$mw30_max_t)) >= length(ttt30$mw30_max_t)*0.9) {
      cumdd_30d[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = ttt30$mw30_min_t, daily_max = ttt30$mw30_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_30d[i] <- cumdd[30]
    }

    # _________
    # Computing CUMDD_60D and handling NAs:
    if (sum(is.na(ttt60$mw60_min_t)) >= length(ttt60$mw60_min_t)*0.9) {
      cumdd_60d[i] <- "NA"
    } else if (sum(is.na(ttt60$mw60_max_t)) >= length(ttt60$mw60_max_t)*0.9) {
      cumdd_60d[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = ttt60$mw60_min_t, daily_max = ttt60$mw60_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_60d[i] <- cumdd[60]
    }

    # _________
    # Computing CUMDD_BETWEEN and handling NAs:
    if (sum(is.na(tttbet$mwbet_min_t)) >= length(tttbet$mwbet_min_t)*0.9) {
      cumdd_between[i] <- "NA"
    } else if (sum(is.na(tttbet$mwbet_max_t)) >= length(tttbet$mwbet_max_t)*0.9) {
      cumdd_between[i] <- "NA"
    } else {
      degday::dd_calc(daily_min = tttbet$mwbet_min_t, daily_max = tttbet$mwbet_max_t, thresh_low = 0,
                      thresh_up = 100, method = "sng_sine", cumulative = TRUE, no_neg = TRUE,
                      interpolate_na = TRUE) -> cumdd # This function doesn't work if the
      # temperature vectors are all NA's or if there is only one value that is NOT NA (because it
      # cannot interpolate missing values in such a case).
      cumdd_between[i] <- cumdd[length(cumdd)]
    }

    # _________
    # Computing MIN_T_BEFORE, MIN_T_BETWEEN and handling NAs:
    if (sum(is.na(ttt30$mw30_min_t)) >= length(ttt30$mw30_min_t)*0.67) { # This time, I consider NA
      # if more than 2/3 of observations are missing because later missing data imputation would probably
      # be more reliable if the proportion of missing "min_t" observations is not too high!
      min_t_before[i] <- "NA"
    } else if (all(is.na(ttt30$mw30_min_t))) {
      min_t_before[i] <- "NA"
    } else {
      min_t_before[i] <- min(ttt30$mw30_min_t, na.rm = TRUE)
    }

    if (sum(is.na(tttbet$mwbet_min_t)) >= length(tttbet$mwbet_min_t)*0.67) {
      min_t_between[i] <- "NA"
    } else if (all(is.na(tttbet$mwbet_min_t))) {
      min_t_between[i] <- "NA"
    } else {
      min_t_between[i] <- min(tttbet$mwbet_min_t, na.rm = TRUE)
    }

    # _________
    # Computing MEAN_WINTER_T, SD_WINTER_T and handling NAs:
    mean_winter_t[i] <- mean(tttwinter$winter_mean_t, na.rm = TRUE)
    sd_winter_t[i] <- stats::sd(tttwinter$winter_mean_t, na.rm = TRUE)

  }

  tits$cumdd_30 <- as.numeric(cumdd_30d)
  tits$cumdd_60 <- as.numeric(cumdd_60d)
  tits$cumdd_between <- as.numeric(cumdd_between)
  tits$min_t_before <- as.numeric(min_t_before)
  tits$min_t_between <- as.numeric(min_t_between)
  tits$mean_winter_t <- as.numeric(mean_winter_t)
  tits$sd_winter_t <- as.numeric(sd_winter_t)


  # # To dismiss notes regarding "visible binding for global variables" during the CMD Check
  # # (only useful for package building):
  # laying_date <- year2 <- month2 <- day2 <- flight_date <- year3 <- month3 <- day3 <-
  #   year <- breeding_window <- hatching_date <- incubation_date <- time <- s69 <- month <-
  #   day <- min_t <- max_t <- NULL


  ##### To export the updated table
  # _______________________________
  readr::write_csv2(x = tits, file = here::here("output", "tables", "ndata_temp.csv"))
  # return(here::here("output", "tables", "ndata_temp.csv"))
  # As this function is pretty long to run (because of the heavy for-loop), I do not make it
  # return the resulting data in a tibble/dataframe, but as a CSV table that could thus be
  # loaded without actually calling the function.
  # _______________________________

}





### ________________________________________________________________________________
#' Update and export the tits dataset with all its "raw" independent variables (IVs)
#'
#' @description (THEORETICALLY MEANT FOR INTERNAL USE ONLY, use with caution!) \cr
#' The function is the \strong{second} of a series of functions meant to update and
#' complete the tits dataset with its independent variables (i.e. predictors and covariates).
#' The `tdata_upD_rawiv` function loads and modifies the dataset generated by the previous
#' update function (\code{\link[ppl.tits:tdata_upD_temp]{tdata_upD_temp}}) in several ways:
#' * First, it performs a left join between both input datasets (i.e. `my_tdata` and
#' `my_iv_data` - see below).
#' * Second, it computes two new variables: namely \emph{woody_area} and \emph{open_area} from
#' already existing columns.
#' * Third, it reorganises the dataset for improved clarity.
#' When it's done processing, the function exports the updated dataset.
#'
#' @param my_tdata The path to the dataset generated by
#' \code{\link[ppl.tits:tdata_upD_temp]{tdata_upD_temp}}. Note that this argument is not
#' necessarily required but is strongly advised.
#' @param my_iv_data The path to the dataset (.csv) generated by PostGIS (cf. ppl_log.docx***) and
#' containing the "raw" independent variables (IV = predictors and covariates) meant to describe the local
#' environment around each nestbox (= zonal statistics). It is called "raw" as opposed to the
#' more "advanced" IV that will be computed by the next functions to generate the "clean" and "complete"
#' tits dataset (`ntits_clean` and `ntits_complete`, respectively).
#'
#' @return A list containing 1) the updated dataset (a tibble accessible with
#' `tdata_upD_rawiv()$dataset`), and 2) the path to the exported .csv file of the updated
#' dataset (accessible with `tdata_upD_rawiv()$path`).
#' @export
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_integer
#' @importFrom readr write_csv2
#' @importFrom here here
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' mydata <- tdata_upD_rawiv()$dataset
#' path_to_csv <- tdata_upD_rawiv()$path
#' }
tdata_upD_rawiv <- function(my_tdata = here::here("output", "tables", "ndata_temp.csv"),
                            my_iv_data = here::here("input_raw_data", "tits_predictors.csv")){

  ##### Import data files
  # _____________________
  tits <- readr::read_csv2(my_tdata, col_names = TRUE,
                           col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                   year = readr::col_factor(),
                                                   breeding_window = readr::col_factor(),
                                                   laying_date = readr::col_date(),
                                                   flight_date = readr::col_date(),
                                                   clutch_size = readr::col_integer(),
                                                   brood_size = readr::col_integer(),
                                                   fledgling_nb = readr::col_integer(),
                                                   success_manipulated = readr::col_factor(),
                                                   father_id = readr::col_factor(),
                                                   mother_id = readr::col_factor(),
                                                   species = readr::col_factor())) # NOTE: I have
  # to generate "tits" by reading the exported "ndata_temp" dataset in order for {targets} to be
  # able to follow modifications. Otherwise, I could just have used ppl.tits::tdata_upD_temp().


  tpred <- readr::read_csv2(my_iv_data, col_names = TRUE, na = "NA",
                            col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                    lsource_vs150_m = readr::col_integer(),
                                                    age_class = readr::col_factor(
                                                      ordered = TRUE,
                                                      levels = c("0", "1", "2"),
                                                      include_na = TRUE),
                                                    site = readr::col_factor(),
                                                    strata_div = readr::col_factor(
                                                      ordered = TRUE,
                                                      levels = c("0", "1", "2", "3", "4"),
                                                      include_na = TRUE)))





  ##### Actual data join, new predictors computation and data reorganisation
  # ________________________________________________________________________
  ntits <- dplyr::left_join(tits, tpred, by = "id_nestbox")
  ntits %>% dplyr::mutate(woody_area = vegetation_area - herbaceous_area,
                          open_area = dplyr::case_when(
                            dist == 50 ~ 7850 - c(vegetation_area + built_area + water_area),
                            dist == 100 ~ 31400 - c(vegetation_area + built_area + water_area),
                            dist == 150 ~ 70650 - c(vegetation_area + built_area + water_area),
                            dist == 200 ~ 125600 - c(vegetation_area + built_area + water_area))) %>% # The
    # values are the total surface area of the buffers computed by PostGIS as a
    # function of their radius!
    dplyr::select(-temp_station_id) %>%
    dplyr::relocate(site, .after = id_nestbox) %>%
    dplyr::relocate(dist, .after = year) %>%
    dplyr::relocate(lsource_vs150_m, .after = lflux_10_iq) %>%
    dplyr::relocate(lsource_vs150_iq, .after = lsource_vs150_m) %>%
    dplyr::relocate(soft_manag_area, .after = herbaceous_area) %>%
    dplyr::relocate(built_area, .after = soft_manag_area) %>%
    dplyr::relocate(build_volume, .after = built_area) %>%
    dplyr::relocate(build_sd, .after = build_volume) %>%
    dplyr::relocate(woody_area, .before = vegetation_area) %>%
    dplyr::relocate(open_area, .after = build_sd) %>%
    dplyr::relocate(water_area, .after = open_area) %>%
    dplyr::relocate(age_class, .after = trafic) %>%
    dplyr::relocate(strata_div, .after = age_class) -> ntits
  ntits[which(ntits$id_nestbox == "DIJ-188B" & ntits$year == "2022"), "strata_div"] <- "4" # I obviously forgot
  # to assign a strata_div value for this nestbox, so here it is (NOTE: as I assign it here, it
  # means I did not update it in the GIS layers nor in the database)!

  # To dismiss notes regarding "visible binding for global variables" during the CMD Check:
  id_nestbox <- year <- breeding_window <- laying_date <- flight_date <- clutch_size <- brood_size <-
    fledgling_nb <- success_manipulated <- father_id <- mother_id <- species <-
    vegetation_area <- herbaceous_area <- built_area <- temp_station_id <- site <-
    lsource_vs150_m <- lflux_10_iq <- lsource_vs150_iq <- soft_manag_area <- woody_area <-
    open_area <- age_class <- trafic <- strata_div <- water_area <- dist <- build_volume <- build_sd <- NULL



  ##### To export the updated table
  # _______________________________
  readr::write_csv2(x = ntits, file = here::here("output", "tables", "ndata_rawiv.csv"))

  output <- list(dataset = ntits,
                 path = here::here("output", "tables", "ndata_rawiv.csv"))
  return(output)
  # _______________________________
}




