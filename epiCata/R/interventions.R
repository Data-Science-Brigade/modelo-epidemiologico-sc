#### INTERVENTIONS ####

read_interventions <- function(interventions_filename, allowed_interventions=NULL, google_mobility_filename=NULL, google_mobility_window_size=0, beds_filename="./beds.csv"){
  require(readxl)
  require(tidyverse)
  require(lubridate)

  interventions <- readxl::read_excel(interventions_filename, 4)

  # Columns: "ID_ATIV"   "ATIVIDADE" "AREA"      "SITUACAO"  "NORMA"     "DATA"      "ADERENCIA"
  #   Where ADERENCIA is an estimate of how much that economic area is suspended.
  #     1 means it is completely closed,
  #     0 means that this economic area is completely open and contact between people are just like before the pandemic started
  interventions$DATA <- ymd(interventions$DATA)

  # Right now, we are only considering state-wide interventions, so all mapped interventions apply to all cities and macrorregions
  # TODO: Add the capability to inform interventions at the level of cities and macrorregions

  # This handles duplicated dates by taking the minimum ADERENCIA
  interventions <-
    interventions %>% group_by(AREA, DATA) %>%
    summarise(ADERENCIA=min(ADERENCIA), .groups="drop")

  if(!is.null(allowed_interventions) ){
    if(length(allowed_interventions) > 0){
      interventions <- interventions %>% filter(AREA %in% allowed_interventions)
    }
  }
  interventions[["location_name"]] = NULL
  print(interventions)

  google_mobility <- read_google_mobility(google_mobility_filename, window_size=google_mobility_window_size)
  print(google_mobility)
  if(!is.null(google_mobility)){
    interventions <- bind_rows(interventions, google_mobility)
  }
  print(interventions)

  beds <- read_beds(beds_filename)
  print(beds)
  if(!is.null(beds)){
    interventions <- bind_rows(interventions, beds)
  }
  print(interventions)

  interventions
}

#### GOOGLE MOBILITY ####
google_mobility_window_summary <- function(mobility_data) {
  summarise(mobility_data, start=min(DATA), end=max(DATA), ADERENCIA=mean(ADERENCIA))
}

read_google_mobility <- function(google_mobility_filename, window_size=0){
  if(is.null(google_mobility_filename)){
    NULL
  }else{
    require(readr)
    require(stringr)
    require(tidyverse)
    require(lubridate)
    require(slider)
    cols <- c("date",
              "sub_region_1",
              "retail_and_recreation_percent_change_from_baseline",
              "grocery_and_pharmacy_percent_change_from_baseline",
              "parks_percent_change_from_baseline",
              "transit_stations_percent_change_from_baseline",
              "workplaces_percent_change_from_baseline",
              "residential_percent_change_from_baseline")

    mobility <- readr::read_csv(google_mobility_filename)
    mobility <- mobility %>% filter(country_region == "State of Santa Catarina")

    mobility <- mobility[, cols] %>% mutate_if(is.character, clean_city_name_str)
    mobility$date <- ymd(mobility$date)
    print(mobility)

    mobility_long <- mobility %>%
      gather(key="AREA", value = "ADERENCIA", -date) %>%
      rename(DATA=date) %>%
      mutate(ADERENCIA=ADERENCIA/100)
    mobility_long[["location_name"]] <- NULL
    print(mobility_long)

    mobility_ordered <- mobility_long %>% arrange(DATA)

    groups <- unique(mobility_ordered$AREA)

    # TODO: Group map?
    mobility_slided_list <- list()
    for(i in seq_along(groups)) {
      group <- groups[[i]]
      group_data <- mobility_ordered %>% filter(AREA==group)
      slided_group_data <-
        slide_period_dfr(group_data, group_data$DATA, "day", google_mobility_window_summary, .before=window_size) %>%
        select(-start) %>%
        rename(DATA=end)
      slided_group_data[["AREA"]] <- group
      mobility_slided_list[[i]] <- slided_group_data
    }
    mobility_slided <- bind_rows(mobility_slided_list) %>% relocate(AREA, .before=ADERENCIA)

    print(mobility_slided)
    mobility_slided
  }
}

read_beds <- function(beds_filename, EPS=1e-6){
  if(is.null(beds_filename)){
    NULL
  }else{
    require(readr)
    require(tidyverse)
    require(lubridate)
    require(slider)
    cols <- c("leitos_covid_enfermaria_ocupados",
              "leitos_covid_enfermaria_disponiveis",
              "leitos_covid_uti_ocupados",
              "leitos_covid_uti_disponiveis")

    cities_df <- readr::read_csv(beds_filename) %>%
      rename(data_ocorrencia=dia, nom_municipio = municipio)

    # TODO pass as arg:
    population_filename <- "pop_and_regions.csv"
    pop_df <- readr::read_csv(population_filename) %>% select(-c(cod_municipio_ibge, qtd_populacao_estimada))

    cities_df$data_ocorrencia <- ymd(cities_df$data_ocorrencia)

    cities_df <- cities_df %>%
      #select(-nom_regional) %>%
      left_join(pop_df, by="nom_municipio")

    #### COMPILE leitos disponiveis AND ocupados FOR EACH CITY, REGION AND STATE ####
    cities_df <- cities_df %>% group_by(nom_regiaosaude, nom_regional, nom_municipio, data_ocorrencia) %>%
      summarise(leitos_covid_enfermaria_ocupados=sum(leitos_covid_enfermaria_ocupados),
                leitos_covid_enfermaria_disponiveis=sum(leitos_covid_enfermaria_disponiveis),
                leitos_covid_uti_ocupados=sum(leitos_covid_uti_ocupados),
                leitos_covid_uti_disponiveis=sum(leitos_covid_uti_disponiveis))

    for(c in cols){
      cities_df[[c]] <- cities_df[[c]] + EPS
    }

    healthregions_df <- cities_df %>% group_by(nom_regiaosaude, data_ocorrencia) %>%
      summarise(leitos_covid_enfermaria_ocupados=sum(leitos_covid_enfermaria_ocupados),
                leitos_covid_enfermaria_disponiveis=sum(leitos_covid_enfermaria_disponiveis),
                leitos_covid_uti_ocupados=sum(leitos_covid_uti_ocupados),
                leitos_covid_uti_disponiveis=sum(leitos_covid_uti_disponiveis))

    macrorregions_df <- cities_df %>% group_by(nom_regional, data_ocorrencia) %>%
      summarise(leitos_covid_enfermaria_ocupados=sum(leitos_covid_enfermaria_ocupados),
                leitos_covid_enfermaria_disponiveis=sum(leitos_covid_enfermaria_disponiveis),
                leitos_covid_uti_ocupados=sum(leitos_covid_uti_ocupados),
                leitos_covid_uti_disponiveis=sum(leitos_covid_uti_disponiveis))

    state_df <- macrorregions_df %>% group_by(data_ocorrencia) %>%
      summarise(leitos_covid_enfermaria_ocupados=sum(leitos_covid_enfermaria_ocupados),
                leitos_covid_enfermaria_disponiveis=sum(leitos_covid_enfermaria_disponiveis),
                leitos_covid_uti_ocupados=sum(leitos_covid_uti_ocupados),
                leitos_covid_uti_disponiveis=sum(leitos_covid_uti_disponiveis))

    # TODO: Paste from types of beds and states to a complete index
    cities_df[["leitos_covid_enfermaria_pct"]] <- cities_df[["leitos_covid_enfermaria_ocupados"]]/(cities_df[["leitos_covid_enfermaria_ocupados"]]+cities_df[["leitos_covid_enfermaria_ocupados"]])
    cities_df[["leitos_covid_uti_pct"]] <- cities_df[["leitos_covid_uti_ocupados"]]/(cities_df[["leitos_covid_uti_ocupados"]]+cities_df[["leitos_covid_uti_ocupados"]])
    cities_df <- cities_df %>% select(-cols)

    healthregions_df[["leitos_covid_enfermaria_pct"]] <- healthregions_df[["leitos_covid_enfermaria_ocupados"]]/(healthregions_df[["leitos_covid_enfermaria_ocupados"]]+healthregions_df[["leitos_covid_enfermaria_ocupados"]])
    healthregions_df[["leitos_covid_uti_pct"]] <- healthregions_df[["leitos_covid_uti_ocupados"]]/(healthregions_df[["leitos_covid_uti_ocupados"]]+healthregions_df[["leitos_covid_uti_ocupados"]])
    healthregions_df <- healthregions_df %>% select(-cols)

    macrorregions_df[["leitos_covid_enfermaria_pct"]] <- macrorregions_df[["leitos_covid_enfermaria_ocupados"]]/(macrorregions_df[["leitos_covid_enfermaria_ocupados"]]+macrorregions_df[["leitos_covid_enfermaria_ocupados"]])
    macrorregions_df[["leitos_covid_uti_pct"]] <- macrorregions_df[["leitos_covid_uti_ocupados"]]/(macrorregions_df[["leitos_covid_uti_ocupados"]]+macrorregions_df[["leitos_covid_uti_ocupados"]])
    macrorregions_df <- macrorregions_df %>% select(-cols)

    state_df[["leitos_covid_enfermaria_pct"]] <- state_df[["leitos_covid_enfermaria_ocupados"]]/(state_df[["leitos_covid_enfermaria_ocupados"]]+state_df[["leitos_covid_enfermaria_ocupados"]])
    state_df[["leitos_covid_uti_pct"]] <- state_df[["leitos_covid_uti_ocupados"]]/(state_df[["leitos_covid_uti_ocupados"]]+state_df[["leitos_covid_uti_ocupados"]])
    state_df <- state_df %>% select(-cols)

    #### GROUP DATASETS ####
    cities_df <- cities_df %>% ungroup %>%
      mutate(location_name=paste0("SC_MUN_", gsub(" ", "_", nom_municipio))) %>%
      select(location_name, data_ocorrencia, leitos_covid_enfermaria_pct, leitos_covid_uti_pct)
    healthregions_df <- healthregions_df %>% ungroup %>%
      mutate(location_name=paste0("SC_RSA_", gsub(" ", "_", nom_regiaosaude))) %>%
      select(location_name, data_ocorrencia, leitos_covid_enfermaria_pct, leitos_covid_uti_pct)
    macrorregions_df <- macrorregions_df %>% ungroup %>%
      mutate(location_name=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
      select(location_name, data_ocorrencia, leitos_covid_enfermaria_pct, leitos_covid_uti_pct)
    state_df <- state_df %>% ungroup %>%
      mutate(location_name="SC_ESTADO") %>%
      select(location_name, data_ocorrencia, leitos_covid_enfermaria_pct, leitos_covid_uti_pct)

    #df <- bind_rows(cities_df, healthregions_df, macrorregions_df, state_df)
    df <- state_df %>% select(-location_name)
    print(df)

    df_long <- df %>%
      gather(key="AREA", value = "ADERENCIA", -c(location_name,data_ocorrencia)) %>%
      rename(DATA=data_ocorrencia)
    print(df_long)

    df_long
  }
}
