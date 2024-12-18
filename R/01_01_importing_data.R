# ---------------------------------------------------------------------------------- #
#####                 Function(s) to import raw datasets (.csv)                  #####
# ---------------------------------------------------------------------------------- #

library(magrittr) # The only library that I truly need to load (in order to be able to use pipes).

### __________________________
#' Import the raw tits dataset
#'
#' Imports the raw dataset for the "tits" part of the PubPrivLands project, and drops lines
#' with no information on 'success' as well as one useless column. It also correct a few mistakes
#' and deletes two late reproduction events that would otherwise bias the analyses. \cr To avoid
#' errors, if new data are added using this function, they should be formatted according to the
#' first table I tailored this function for (i.e. same columns, no special characters, no spaces,
#' etc.; the only tolerated differences may be different rows and cell values)!
#'
#' @param mypath The absolute path to the raw data .csv file.
#'
#' @return The path to `raw_tits_data`, an exported tibble (i.e. a kind of improved data.frame) of
#' the raw tits data. For further information on tibbles, please refer to the `tidyverse` or
#' \link[readr]{readr} documentation.
#' @export
#'
#' @importFrom readr read_csv2
#' @importFrom readr cols
#' @importFrom readr col_factor
#' @importFrom readr col_double
#' @importFrom here here
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom usethis use_data
#'
#' @examples
#' \dontrun{
#' mydata <- import_raw_tits_data(mypath = "C:/path/to/my/data.csv") # This example cannot work,
#' # it needs an existing path!
#' }
import_raw_tits_data <- function(mypath = here::here("data", "ppl_dijon_tits_data.csv")){

  ### Raw data import___________________________________________________________#
  aaa <- readr::read_csv2(file = mypath, col_names = TRUE, na = "NA",
                          col_types = readr::cols(id_nestbox = readr::col_factor(),
                                                  site = readr::col_factor(),
                                                  year = readr::col_factor(),
                                                  date = readr::col_factor(),
                                                  laying_date = readr::col_factor(),
                                                  incubation_date = readr::col_factor(),
                                                  hatching_date = readr::col_factor(),
                                                  success = readr::col_factor(),
                                                  success_manipulated = readr::col_factor(),
                                                  father_id = readr::col_factor(),
                                                  mother_id = readr::col_factor(),
                                                  id_bird = readr::col_factor(),
                                                  id_ring = readr::col_factor(),
                                                  species = readr::col_factor(),
                                                  sex = readr::col_factor(),
                                                  age = readr::col_factor(
                                                    ordered = TRUE,
                                                    levels = c("nestling", "one", "more_than_one",
                                                               "two", "more_than_two"),
                                                    include_na = TRUE),
                                                  adult_mass = readr::col_double(),
                                                  adult_tarsus_l = readr::col_double(),
                                                  adult_wing_l = readr::col_double(),
                                                  adult_aggress = readr::col_factor(
                                                    ordered = TRUE,
                                                    levels = c("0,0", "0,5", "1,0", "1,5", "2,0",
                                                               "2,5", "3,0"),
                                                    include_na = TRUE),
                                                  nestling_mass = readr::col_double(),
                                                  nestling_tarsus_l = readr::col_double(),
                                                  nestling_wing_l = readr::col_double()))


  ### Data corrections__________________________________________________________#
  # To delete 'site' and only keep observations with a success evaluation:
  aaa %>% dplyr::select(-site) %>%
    dplyr::filter(!is.na(success)) -> xxx


  # To correct the erroneous "bird_id" (i.e. 'BRFAIPM1120'):
  xxx$id_bird <- as.character(xxx$id_bird) # As R cannot tolerate adding new levels to a factor
  # variable, I have to first convert the variable as a character one!
  xxx[xxx$id_bird == "BRFAIPM1120" &
        xxx$id_ring == "8877045", "id_bird"] <- "THPAGPM095X"
  xxx$id_bird <- as.factor(xxx$id_bird) # And I convert it back into a factor.

  # To correct the 'laying_date' of the female tits with 'id_bird' = CLPECPM0005:
  xxx$laying_date <- as.character(xxx$laying_date) # Idem.
  xxx[xxx$id_bird == "CLPECPM0005", "laying_date"] <- "09/04/2019"
  xxx$laying_date <- as.factor(xxx$laying_date)

  # To correct wrong father_id values for DIJ-272_2021 and DIJ-134_2022:
  xxx$father_id <- as.character(xxx$father_id) # Idem.
  xxx %>%
    dplyr::mutate(
      father_id = dplyr::case_when(
        father_id != "8876210" | year != "2021" ~ father_id, # Here, I have to say that "everything
        # that is not my selection stays as is"! If I don't (and only run the next line), I will
        # get NA for all other observations. NOTE also that as this is a "negative selection"
        # (using the != operator), I need to reverse the "&" as well, however confusing it may be.
        father_id == "8876210" & year == "2021" ~ "8876510")) %>%
    dplyr::mutate( # Note also that I have to run each modification as a separate "mutate"!
      father_id = dplyr::case_when(
        father_id != "8877594" | year != "2022" ~ father_id,
        father_id == "8877594" & year == "2022" ~ "8876594")) -> xxx
  # NOTE: Here, I changed my way of correcting mistakes because there is a bug somewhere in R
  # preventing me from doing it as I used to. My former code was:
  # xxx$father_id <- as.character(xxx$father_id) # Idem.
  # xxx[xxx$father_id == "8876210" &
  #       xxx$year == "2021", "father_id"] <- "8876510"
  # xxx[xxx$father_id == "8877594" &
  #       xxx$year == "2022", "father_id"] <- "8876594"
  # And it used to work fine. But, at some point (weeks after I wrote these lines), it stopped
  # working and generated an error saying: "Can't use NA as row index in a tibble for assignment".
  # It probably has something to do with an update somewhere and a bug between R and Windows. It
  # doesn't make sense otherwise. The problem is linked to how R handles NA in factors but, trust
  # me, it TRULY doesn't make sense (I simply don't have time to report it properly here)!
  xxx$father_id <- as.factor(xxx$father_id)

  # To correct wrong id_ring values for the females of DIJ-133_2022 and DIJ-179_2020:
  xxx$id_ring <- as.character(xxx$id_ring) # Idem and same problem here!
  xxx$mother_id <- as.character(xxx$mother_id)
  xxx %>%
    dplyr::mutate(
      id_ring = dplyr::case_when(
        id_ring != "8877950" | is.na(id_ring) == "TRUE" ~ id_ring,
        id_ring == "8877950" & year == "2022" ~ "8877590")) %>%
    dplyr::mutate(
      mother_id = dplyr::case_when(
        mother_id != "8581599" | is.na(mother_id) == "TRUE" ~ mother_id,
        mother_id == "8581599" & year == "2020" ~ "8381599")) -> xxx
  # NOTE: here, there is another bug as it doesn't work unless I specify observations without NA
  # while the exact same procedure work above for "father_id" which contains NA as well...
  xxx$id_ring <- as.factor(xxx$id_ring)
  xxx$mother_id <- as.factor(xxx$mother_id)

  # To correct the gender/sex of the presumed father of DIJ-212_2022:
  xxx$sex <- as.character(xxx$sex) # Idem and same problem here!
  xxx %>%
    dplyr::mutate(
      sex = dplyr::case_when(
        id_ring != "8877107" | is.na(id_ring) == "TRUE" ~ sex,
        id_ring == "8877107" ~ "male")) -> xxx
  xxx$sex <- as.factor(xxx$sex)

  # To delete late reproduction events (i.e. for DIJ-175 in 2019 and DIJ-044 in 2022):
  xxx %>% dplyr::filter(date != "28/06/2022") %>%
    dplyr::filter(laying_date != "09/05/2019" | is.na(laying_date) == "TRUE") -> xxx
  # NOTE: here again, there is this bug in how R handles negative selections when there are NAs!

  # To convert "adult_aggress" into numeric variables:
  xxx %>% dplyr::mutate(
    adult_aggress = as.numeric(gsub(x = adult_aggress, pattern = ",", replacement = "."))) -> xxx
  # As "adult_aggress" contained commas instead of decimal points, I had to use the 'gsub()'
  # function to convert them (it's a base function close to 'grep()').



  # ### To avoid warnings during the R CMD check (only appropriate for package building)___________#
  # site <- success <- date <- laying_date <- adult_aggress <- age <- id_ring <- sex <- year <- NULL
  # # This is necessary for all not globally defined variables, that is variables that are called
  # # without "$" (such as calls from {dplyr}; e.g. if your function uses something like
  # # dplyr::filter(myvar == "something"), then the variable called "myvar" should either be globally
  # # defined or set to NULL, otherwise you'll get warnings when you'll build your package).



  # ##### To export the updated table (only appropriate for package building)_____________#
  # raw_tits_data <- xxx
  #
  # usethis::use_data(raw_tits_data, overwrite = TRUE) # Creates/updates data/raw_tits_data.rda and, the
  # # first time it is run, modifies the DESCRIPTION file to add `LazyData: true` and `Depends:
  # # R (>= 2.10)`!
  # return(here::here("data", "raw_tits_data.rda"))

  # As I'm not building a package here, I simply end my function with:
  return(xxx)

}




### __________________________________
#' Aggregate the raw tits data by nest
#'
#' @description The `aggreg_by_nest()` function aggregates the raw tits data to create a table
#' summarizing tits' breeding success for each nestbox per year where breeding occurred. As such,
#' it only keeps information on nestlings/juveniles and disregards the lines about adult tits.
#'
#' @param myrawdata The object containing the raw tits data. Cf.
#' \code{\link[ppl.tits:import_raw_tits_data]{import_raw_tits_data}}.
#'
#' @return The aggregated nestling tits dataset.
#'
#' @export
#' @importFrom readr write_csv2
#' @importFrom here here
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr first
#'
#' @examples
#' \dontrun{
#' aggreg_by_nest(myrawdata = raw_tits_data)
#' }

### I need an internal object, no???
aggreg_by_nest <- function(myrawdata){

  ##### Data preparation
  # ____________________
  rtits <- import_raw_tits_data()

  rtits %>% dplyr::filter(age == "nestling") %>%
    dplyr::group_by(id_nestbox, year) %>% # Aggregates by nestbox and year!
    dplyr::summarise(date = dplyr::first(date),
                     laying_date = dplyr::first(laying_date),
                     incubation_date = dplyr::first(incubation_date),
                     hatching_date = dplyr::first(hatching_date),
                     clutch_size = max(clutch_size),
                     brood_size = max(brood_size),
                     fledgling_nb = max(fledgling_nb),
                     success_manipulated = dplyr::first(success_manipulated),
                     father_id = dplyr::first(father_id),
                     mother_id = dplyr::first(mother_id),
                     species = dplyr::first(species),
                     mass = mean(nestling_mass),
                     tarsus_length = mean(nestling_tarsus_l),
                     wing_length = mean(nestling_wing_l)) -> xxx

  # # To dismiss R CMD checks warnings for unbound variables (only useful for package building):
  # age <- id_nestbox <- year <- laying_date <- incubation_date <- hatching_date <- clutch_size <- NULL
  # brood_size <- fledgling_nb <- success_manipulated <- father_id <- mother_id <- species <- NULL
  # nestling_mass <- nestling_tarsus_l <- nestling_wing_l <- NULL

  return(xxx)
}
