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
read_covid_data <- function(deaths_filename, population_filename, reference_date, start_pandemic=30, allowed_locations=NULL){
  require(readr)
  require(tidyverse)
  require(lubridate)

  beds_df <- readr::read_csv("./beds.csv") %>%
    select(-c("cod_municipio_ibge","leitos_covid_enfermaria_ocupados","leitos_covid_enfermaria_disponiveis","leitos_covid_enfermaria_existentes","leitos_covid_uti_existentes")) %>%
    rename(nom_municipio=municipio, data_ocorrencia=dia, icu_beds=leitos_covid_uti_ocupados)
  cities_df <- readr::read_csv(deaths_filename)

  pop_df <- readr::read_csv(population_filename) %>% select(-c(cod_municipio_ibge, qtd_populacao_estimada))

  cities_df$data_ocorrencia <- ymd(cities_df$data_ocorrencia)
  beds_df$data_ocorrencia <- ymd(beds_df$data_ocorrencia)

  cities_df <- cities_df %>%
    left_join(beds_df, by=c("nom_municipio","data_ocorrencia"))
  # Could remove the line below, I think
  cities_df$icu_beds <- replace_na(cities_df$icu_beds, 0)

  if(max(cities_df$data_ocorrencia, na.rm=TRUE) < reference_date-days(1)){
    stop("Your dataset does not have data up to the reference date!")
  }

  # Filter any data beyond the reference date
  cities_df <- filter(cities_df, data_ocorrencia < reference_date)

  min_date_df <- cities_df %>% group_by(nom_municipio) %>%
    summarise(min_date=min(data_ocorrencia, na.rm=T), .groups="drop")
  cities_df <- cities_df %>% merge(min_date_df) %>%
    mutate(data_ocorrencia=ifelse(is.na(data_ocorrencia), min_date, data_ocorrencia))
  cities_df <- cities_df %>% select(-min_date)
  cities_df$data_ocorrencia <- as_date(cities_df$data_ocorrencia)

  # Get healthregions and macrorregions
  # FIXME: Joining by cod_municipio_ibge would be less error-prone
  cities_df <- cities_df %>% select(-nom_regional) %>% left_join(pop_df, by="nom_municipio")

  cities_df <- cities_df %>% group_by(nom_regiaosaude, nom_regional, nom_municipio, data_ocorrencia) %>%
    summarise(casos=sum(casos), obitos=sum(obitos), icu_beds=sum(icu_beds))


  #### COMPILE casos AND obitos FOR EACH CITY, REGION AND STATE ####
  healthregions_df <- cities_df %>% group_by(nom_regiaosaude, data_ocorrencia) %>% summarise(casos=sum(casos), obitos=sum(obitos), icu_beds=sum(icu_beds))

  macrorregions_df <- cities_df %>% group_by(nom_regional, data_ocorrencia) %>% summarise(casos=sum(casos), obitos=sum(obitos), icu_beds=sum(icu_beds))

  state_df <- macrorregions_df %>% group_by(data_ocorrencia) %>% summarise(casos=sum(casos), obitos=sum(obitos), icu_beds=sum(icu_beds))

  #### GROUP DATASETS ####
  cities_df <- cities_df %>% ungroup %>%
    mutate(location_name=paste0("SC_MUN_", gsub(" ", "_", nom_municipio))) %>%
    select(location_name, data_ocorrencia, casos, obitos, icu_beds)
  healthregions_df <- healthregions_df %>% ungroup %>%
    mutate(location_name=paste0("SC_RSA_", gsub(" ", "_", nom_regiaosaude))) %>%
    select(location_name, data_ocorrencia, casos, obitos, icu_beds)
  macrorregions_df <- macrorregions_df %>% ungroup %>%
    mutate(location_name=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
    select(location_name, data_ocorrencia, casos, obitos, icu_beds)
  state_df <- state_df %>% ungroup %>%
    mutate(location_name="SC_ESTADO") %>%
    select(location_name, data_ocorrencia, casos, obitos, icu_beds)

  df <- bind_rows(cities_df, healthregions_df, macrorregions_df, state_df)

  #### FIX DATES ####
  # Dates that are NULL are attributed to the beginning of the epidemic in that location
  df$data_ocorrencia <- ymd(df$data_ocorrencia)

  if(nrow(df %>% filter(is.na(data_ocorrencia), obitos > 0)) > 0){
    stop("There is an error in the input data. Some deaths were reported without a proper date.")
  }

  # Subtract one from reference date to start predicting from it
  all_dates <- seq(min(df$data_ocorrencia, na.rm=T) - days(start_pandemic), ymd(reference_date)-days(1), by = '1 day')
  all_dates_df <- expand.grid(unique(df$location_name), all_dates)
  colnames(all_dates_df) <- c("location_name", "data_ocorrencia")
  all_dates_df <- all_dates_df %>% arrange(location_name, data_ocorrencia)
  df <- df %>% merge(all_dates_df, by=c("location_name", "data_ocorrencia"), all=T) %>%
    mutate(casos=ifelse(is.na(casos), 0, casos),
           obitos=ifelse(is.na(obitos), 0, obitos),
           icu_beds=ifelse(is.na(icu_beds), 0, icu_beds))

  df <- df %>% filter(location_name %in% allowed_locations)

  #if(any(df$data_ocorrencia > (reference_date - days(1)))){
  #  df[which(df$data_ocorrencia > (reference_date - days(1))), "data_ocorrencia"] <- reference_date - days(1)
  #}

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

read_onset_to_death <- function(onset_to_death_filename){
  require(readr)
  readr::read_csv(onset_to_death_filename)
}

read_deaths <- function(deaths_filename){
  require(readr)
  readr::read_csv(deaths_filename)
}

read_IFR <- function(IFR_filename){
  #### code to prepare `IFR` dataset goes here ####

  # Float number for the estimated Infection Fatality Rate (IFR) estimated for the state of Santa Catarina in
  # https://github.com/ImperialCollegeLondon/covid19model/blob/master/Brazil/data/IFRS-all.csv
  # The authors estimated Brazilian IFRs using four different references: IFR_UK	| IFR_Peru |	IFR_UK_poorer |	IFR_Peru_poorer
  # In their final report, they end up using IFR_Peru_poorer and that is what we use here too
  #  (https://github.com/ImperialCollegeLondon/covid19model/blob/master/Brazil/code/preprocessing-subnation-brazil.r)

  weight_fatality <- read.csv(IFR_filename)[c("X","State","IFR_Peru_poorer")]

  # IFR Estimated for the state of Santa Catarina
  IFR <- (weight_fatality %>% filter(State == "SC"))[,3]
}

read_ICUR <- function(ICU_filename=NULL){
  # http://dx.doi.org/10.1590/1806-9282.66.8.1157
  # In addition to a similar flu presentation, COVID-19 can manifest itself as a neurological syndrome, heart failure, or acute myocardial infarction7 , 8 . Most infections (80%) are mild. However, 6-10% will require transfer to the ICU9.
  # So mean on 0.08 with a std that slightly overshoots the 6-10% range (No 95%  C.I. given)
  #icu_pct ~ normal(0.08,0.015);
  0.08
}

read_serial_interval <- function(serial_interval_filename){
  #### code to prepare `serial_interval` dataset goes here ####
  readr::read_csv(serial_interval_filename, col_names=c("X", "fit"), skip=1)
}

read_infection_to_onset <- function(infection_to_onset_filename){
  #### PARAMETERS FOR GAMMA DISTRIBUTIONS ####

  # infection-to-onset distribution parameters (DEFAULT)
  infection_to_onset <- data.frame(avg_days = 5.1, coeff_variation = 0.86)
}

read_pop <- function(population_filename){
  #### POPULATION OF SANTA CATARINA ####

  SC_pop <- read.csv(population_filename)

  SC_pop_cities <- SC_pop %>%
    mutate(location_name=paste0("SC_MUN_", gsub(" ", "_", nom_municipio))) %>%
    rename(pop=qtd_populacao_estimada) %>%
    select(location_name, pop)

  SC_pop_macrorregions <- SC_pop %>%
    group_by(nom_regional) %>% summarise(pop=sum(qtd_populacao_estimada)) %>% ungroup %>%
    mutate(location_name=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
    select(location_name, pop)

  SC_pop_healthregions <- SC_pop %>%
    group_by(nom_regiaosaude) %>% summarise(pop=sum(qtd_populacao_estimada)) %>% ungroup %>%
    mutate(location_name=paste0("SC_RSA_", gsub(" ", "_", nom_regiaosaude))) %>%
    select(location_name, pop)

  SC_pop_state <- SC_pop %>% summarise(pop=sum(qtd_populacao_estimada)) %>%
    mutate(location_name="SC_ESTADO") %>%
    select(location_name, pop)

  pop <- bind_rows(SC_pop_cities, SC_pop_healthregions, SC_pop_macrorregions, SC_pop_state)
}

