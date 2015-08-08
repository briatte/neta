rm(list = ls())
source("load.r")

# update ongoing legislature
# set_update(14)

# set generic SQL settings
sql = c(user = "opendata",
        password = "",
        host = "localhost",
        port = 5432, drv = "PostgreSQL")  # CREATE USER opendata;
sqla = as.list(c(dbname = "ameli",  sql)) # CREATE DATABASE ameli;
sqlb = as.list(c(dbname = "dosleg", sql)) # CREATE DATABASE dosleg;

# Assemblée nationale data
get_deputes(                   sessions =  1:14) # sponsors
get_dossiers("projets"       , sessions =  8:14) # government bills
get_dossiers("propositions"  , sessions =  8:14) # MP bills
get_dossiers("resolutions"   , sessions =  8:14) # EU resolutions
get_dossiers("resol-art34-1" , sessions =  8:14) # other resolutions
get_dossiers("depots"        , sessions =  8:14) # all other files
get_bills(                     sessions =  8:14) # bills
get_amendments(                sessions = 12:14) # amendments

# Sénat data
get_senateurs(          sessions =  1:14) # sponsors
get_dosleg(sql = sqlb , sessions =  8:14) # bills
get_ameli(sql = sqla  , sessions = 11:14) # amendments

# # split series networks
# get_networks(  8:14 , file = "bi_an") # National Assembly bills
# get_networks(  8:14 , file = "bi_se") # Senate bills
# get_networks( 12:14 , file = "am_an") # National Assembly amendments
# get_networks( 11:14 , file = "am_se") # Senate amendments

# network model, estimated over legislatures
get_networks(8:14, file = "an", export = FALSE)  # National Assembly
get_networks(8:14, file = "se", export = FALSE)  # Senate
get_models("ergm") # bootstrapped TERGMs

# network model, estimated over legislature-years
# get_networks("yr", file = "an", verbose = TRUE)  # National Assembly
# get_networks("yr", file = "se", verbose = FALSE) # Senate
# get_models("ergm") # bootstrapped TERGM

# # zip raw data
# zip_files(verbose = TRUE)

# have a nice day
