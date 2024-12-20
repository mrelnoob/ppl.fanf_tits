

## TO LOAD THE DATA:

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
                                                   include_na = FALSE))) # NOTE: I
# have to load the "tits" final dataset exported by 'tfinal_EDAta()' in order to avoid running
# the former function that is a bit long to run (but you can if you want).

# PENSER A REMPLACER ntits3 par ntits§§§§ pour la suite ;)
