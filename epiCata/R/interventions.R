#### INTERVENTIONS ####

read_interventions <- function(selected_date, allowed_interventions, use_google_mobility=TRUE){
  require(lubridate)

  google_mobility <-
    if(use_google_mobility){
      if(missing(selected_date)){
        read_google_mobility()
      }else{
        read_google_mobility(selected_date)
      }
    }else{
      NULL
    }

  if(missing(selected_date)){
    files <- list.files(get_data_folder(), pattern="*interventions*")
    selected_date <- ymd(max(sapply(files, function(x){substr(x, 1, 10)})))
  }

  selected_date <- strftime(selected_date, format="%Y_%m_%d")

  interventions <- readxl::read_excel(sprintf("%s/%s_interventions.xls", get_data_folder(), selected_date), 4)

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

  if(!missing(allowed_interventions) ){
    if(length(allowed_interventions) > 0){
      interventions <- interventions %>% filter(AREA %in% allowed_interventions)
    }
  }

  if(use_google_mobility){
    interventions <- bind_rows(interventions, google_mobility)
  }

  interventions
}


#### GOOGLE MOBILITY ####

read_google_mobility <- function(selected_date){
  require(lubridate)

  cols <- c("date",
            "retail_and_recreation_percent_change_from_baseline",
            "grocery_and_pharmacy_percent_change_from_baseline",
            "parks_percent_change_from_baseline",
            "transit_stations_percent_change_from_baseline",
            "workplaces_percent_change_from_baseline",
            "residential_percent_change_from_baseline")

  if(missing(selected_date)){
    files <- list.files(get_data_folder(), pattern="*mobility*")
    selected_date <- ymd(max(sapply(files, function(x){substr(x, 1, 10)})))
  }

  selected_date <- strftime(selected_date, format="%Y_%m_%d")
  mobility <- readr::read_csv(sprintf("%s/%s_google_mobility.csv", get_data_folder(), selected_date))

  mobility <- mobility[, cols]
  mobility$date <- ymd(mobility$date)

  mobility_long <- mobility %>%
    gather(key="AREA", value = "ADERENCIA", -date) %>%
    rename(DATA=date) %>%
    mutate(ADERENCIA=ADERENCIA/100)

  mobility_long
}
