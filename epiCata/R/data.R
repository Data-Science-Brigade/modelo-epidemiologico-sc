# REQUIRED #
# sudo apt-get install libcurl4-openssl-dev libssl-dev
# sudo apt-get install libxml2-dev

get_data_folder <- function(){
  paste0(system.file(package="epiCata"), "/extdata")
}
#### CASES AND DEATHS ####

#
# TODO: Document it with Roxygen2
#
read_covid_data <- function(selected_date, start_pandemic=30, reference_date){
  require(tidyverse)
  require(lubridate)
  FILE_FORMAT = "%s/%s_compilado.csv"

  if(missing(selected_date)){
    files <- list.files(get_data_folder(), pattern="*compilado*")
    selected_date <- ymd(max(sapply(files, function(x){substr(x, 1, 10)})))
  }

  selected_date <- strftime(selected_date, format="%Y_%m_%d")
  #### READ DATA FROM CSV FILES ####
  cities_df <- read_csv(sprintf(FILE_FORMAT, get_data_folder(), selected_date))
  cities_df$data_ocorrencia <- ymd(cities_df$data_ocorrencia)

  # Handle incorrect input: Assume any future dates were input incorrectly and are assigned to reference date
  if(any(cities_df$data_ocorrencia > (reference_date - days(1)), na.rm=T)){
    cities_df[which(cities_df$data_ocorrencia > (reference_date - days(1))), "data_ocorrencia"] <- reference_date - days(1)
  }

  min_date_df <- cities_df %>% group_by(nom_municipio) %>%
    summarise(min_date=min(data_ocorrencia, na.rm=T), .groups="drop")
  cities_df <- cities_df %>% merge(min_date_df) %>%
    mutate(data_ocorrencia=ifelse(is.na(data_ocorrencia), min_date, data_ocorrencia))
  cities_df <- cities_df %>% select(-min_date)
  cities_df$data_ocorrencia <- as_date(cities_df$data_ocorrencia)

  cities_df <- cities_df %>% group_by(nom_regional, nom_municipio, data_ocorrencia) %>%
    summarise(casos=sum(casos), obitos=sum(obitos))

  #### COMPILE casos AND obitos FOR EACH CITY, REGION AND STATE ####
  macrorregions_df <- cities_df %>% group_by(nom_regional, data_ocorrencia) %>% summarise(casos=sum(casos), obitos=sum(obitos))

  state_df <- macrorregions_df %>% group_by(data_ocorrencia) %>% summarise(casos=sum(casos), obitos=sum(obitos))

  #### GROUP DATASETS ####
  cities_df <- cities_df %>% ungroup %>%
    mutate(location_name=paste0("SC_MUN_", gsub(" ", "_", nom_municipio))) %>%
    select(location_name, data_ocorrencia, casos, obitos)
  macrorregions_df <- macrorregions_df %>% ungroup %>%
    mutate(location_name=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
    select(location_name, data_ocorrencia, casos, obitos)
  state_df <- state_df %>% ungroup %>%
    mutate(location_name="SC_ESTADO") %>%
    select(location_name, data_ocorrencia, casos, obitos)

  df <- bind_rows(cities_df, macrorregions_df, state_df)

  #### FIX DATES ####
  # Dates that are NULL are attributed to the beginning of the epidemic in that location
  df$data_ocorrencia <- ymd(df$data_ocorrencia)

  if(nrow(df %>% filter(is.na(data_ocorrencia), obitos > 0)) > 0){
    stop("There is an error in the input data. Some deaths were reported without a proper date.")
  }

  all_dates <- seq(min(df$data_ocorrencia, na.rm=T) - days(start_pandemic), ymd(selected_date), by = '1 day')
  all_dates_df <- expand.grid(unique(df$location_name), all_dates)
  colnames(all_dates_df) <- c("location_name", "data_ocorrencia")
  all_dates_df <- all_dates_df %>% arrange(location_name, data_ocorrencia)
  df <- df %>% merge(all_dates_df, by=c("location_name", "data_ocorrencia"), all=T) %>%
    mutate(casos=ifelse(is.na(casos), 0, casos),
           obitos=ifelse(is.na(obitos), 0, obitos))

  df
}

download_latest_data <- function(){
  # TODO: Set up connection to Cloudera's Impala from Ubuntu
  # https://stackoverflow.com/questions/33551542/connect-r-and-impala
  # sudo apt-get update && sudo apt-get install unixodbc-dev
  # export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libodbcinst.so
  stop("Function not yet implemented")
}


#### PARAMETERS ####

read_onset_to_death <- function(selected_date){
  require(lubridate)

  if(missing(selected_date)){
    # Selects the latest date
    files <- list.files(get_data_folder(), pattern="*onset_to_death*")
    selected_date <- ymd(max(sapply(files, function(x){substr(x, 1, 10)})))
  }

  selected_date <- strftime(selected_date, format="%Y_%m_%d")

  readr::read_csv(sprintf("%s/%s_onset_to_death.csv", get_data_folder(), selected_date))
}
