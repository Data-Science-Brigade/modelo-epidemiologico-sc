#### INTERVENTIONS ####

read_interventions <- function(interventions_filename, allowed_interventions=NULL, google_mobility_filename=NULL, google_mobility_window_size=0){
  require(readxl)
  require(tidyverse)
  require(lubridate)

  if(FALSE){
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

    print(interventions)

    if(dim(interventions)[[1]]<=0) {
      interventions <- NULL
    }

  }

  google_mobility <- read_google_mobility(google_mobility_filename, window_size=google_mobility_window_size)
  if(!is.null(google_mobility)){
    interventions <- google_mobility#bind_rows(interventions, google_mobility)
  }

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
    require(tidyverse)
    require(lubridate)
    require(slider)
    cols <- c("date",
              "retail_and_recreation_percent_change_from_baseline",
              "grocery_and_pharmacy_percent_change_from_baseline",
              "parks_percent_change_from_baseline",
              "transit_stations_percent_change_from_baseline",
              "workplaces_percent_change_from_baseline",
              "residential_percent_change_from_baseline")


    coltypes <- cols_only( # c for character, D for Date and d for double.
      iso_3166_2_code="c",
      country_region_code="c",
      sub_region_2="c",
      date="D",
      retail_and_recreation_percent_change_from_baseline="d",
      grocery_and_pharmacy_percent_change_from_baseline="d",
      parks_percent_change_from_baseline="d",
      transit_stations_percent_change_from_baseline="d",
      workplaces_percent_change_from_baseline="d",
      residential_percent_change_from_baseline="d")

    mobility <- readr::read_csv(google_mobility_filename, col_types=coltypes)
    mobility <- mobility %>% filter(iso_3166_2_code == "BR-SC")

    mobility <- mobility[, cols]
    mobility$date <- ymd(mobility$date)

    if(dim(mobility)[[1]]<=0) {
      mobility <- NULL
      return(mobility)
    }

    mobility_long <- mobility %>%
      gather(key="AREA", value = "ADERENCIA", -date) %>%
      rename(DATA=date) %>%
      mutate(ADERENCIA=ADERENCIA/100)

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

    mobility_slided
  }
}

mobility_city_name_to_internal_city_name <- function(cities) {
  require(tidyverse)
  require(dplyr)
  require(stringr)
  cities <- cities %>%
    str_to_lower(locale="pt-br") %>%
    str_replace_all("[áàãâä]","a") %>%
    str_replace_all("[éèẽêë]","e") %>%
    str_replace_all("[íìĩîï]","i") %>%
    str_replace_all("[óòõôö]","o") %>%
    str_replace_all("[úùũûü]","u") %>%
    str_replace_all("[ç]","c") %>%
    str_replace_all(" ","_") %>%
    str_to_upper(locale="pt-br")

  sprintf("SC_MUN_%s", cities)
}

get_cities_pop_in_region <- function(region_name, population_filename="./pop_and_regions.csv"){
  require(tidyverse)
  require(dplyr)
  pop_df <- readr::read_csv(population_filename) %>%
    mutate(nom_municipio=paste0("SC_MUN_", gsub(" ", "_", nom_municipio)),
           nom_regiaosaude=paste0("SC_RSA_", gsub(" ", "_", nom_regiaosaude)),
           nom_regional=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
    rename(pop=qtd_populacao_estimada)

  if(grepl("SC_MUN_", region_name, fixed=TRUE)){
    pop_df <- pop_df %>% filter(nom_municipio==region_name)
  } else if(grepl("SC_RSA_", region_name, fixed=TRUE)){
    pop_df <- pop_df %>% filter(nom_regiaosaude==region_name)
  } else if(grepl("SC_MAC_", region_name, fixed=TRUE)){
    pop_df <- pop_df %>% filter(nom_regional==region_name)
  } else if(grepl("SC_ESTADO", region_name, fixed=TRUE)){
    pop_df
  } else {
    stop("Region must start either with SC_MUN, SC_RSA, SC_MAC, or SC_ESTADO")
  }

  pop_df %>% select(nom_municipio, pop)
}

read_google_mobility_for_cities <- function(region_name, google_mobility_filename="./Global_Mobility_Report.csv", window_size=7, population_filename="./pop_and_regions.csv"){
  if(is.null(google_mobility_filename)){
    NULL
  }else{
    require(readr)
    require(tidyverse)
    require(dplyr)
    require(lubridate)
    require(slider)
    cols <- c("date",
              "retail_and_recreation_percent_change_from_baseline",
              "grocery_and_pharmacy_percent_change_from_baseline",
              "parks_percent_change_from_baseline",
              "transit_stations_percent_change_from_baseline",
              "workplaces_percent_change_from_baseline",
              "residential_percent_change_from_baseline")

    coltypes <- cols_only( # c for character, D for Date and d for double.
                     iso_3166_2_code="c",
                     country_region_code="c",
                     sub_region_2="c",
                     date="D",
                     retail_and_recreation_percent_change_from_baseline="d",
                     grocery_and_pharmacy_percent_change_from_baseline="d",
                     parks_percent_change_from_baseline="d",
                     transit_stations_percent_change_from_baseline="d",
                     workplaces_percent_change_from_baseline="d",
                     residential_percent_change_from_baseline="d")

    mobility <- readr::read_csv(google_mobility_filename, col_types=coltypes)
    mobility <- mobility %>% filter(country_region_code == "BR")
    unique(mobility_city_name_to_internal_city_name(mobility$sub_region_2))
    mobility$sub_region_2 <- mobility_city_name_to_internal_city_name(mobility$sub_region_2)
    mobility <- mobility %>% rename(nom_municipio = sub_region_2)

    pop_df <- get_cities_pop_in_region(region_name)

    mobility_cities <- mobility %>%
      inner_join(pop_df, by="nom_municipio") %>%
      group_by(date) %>%
      summarise(
        retail_and_recreation_percent_change_from_baseline = weighted.mean(retail_and_recreation_percent_change_from_baseline, pop, na.rm=TRUE),
        grocery_and_pharmacy_percent_change_from_baseline = weighted.mean(grocery_and_pharmacy_percent_change_from_baseline, pop, na.rm=TRUE),
        parks_percent_change_from_baseline = weighted.mean(parks_percent_change_from_baseline, pop, na.rm=TRUE),
        transit_stations_percent_change_from_baseline = weighted.mean(transit_stations_percent_change_from_baseline, pop, na.rm=TRUE),
        workplaces_percent_change_from_baseline = weighted.mean(workplaces_percent_change_from_baseline, pop, na.rm=TRUE),
        residential_percent_change_from_baseline = weighted.mean(residential_percent_change_from_baseline, pop, na.rm=TRUE),
      ) %>%
      ungroup()

    mobility <- mobility_cities#mobility[, cols]
    mobility$date <- ymd(mobility$date)

    if(dim(mobility)[[1]]<=0) {
      mobility <- NULL
      return(mobility)
    }

    mobility_long <- mobility %>%
      gather(key="AREA", value = "ADERENCIA", -date) %>%
      rename(DATA=date) %>%
      mutate(ADERENCIA=ADERENCIA/100)

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

    mobility_slided
  }
}
