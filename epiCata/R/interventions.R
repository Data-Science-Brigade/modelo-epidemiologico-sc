#### INTERVENTIONS ####

read_interventions <- function(interventions_filename, allowed_interventions=NULL, google_mobility_filename=NULL){
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

  google_mobility <- read_google_mobility(google_mobility_filename)
  if(!is.null(google_mobility)){
    interventions <- bind_rows(interventions, google_mobility)
  }
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

    mobility <- readr::read_csv(google_mobility_filename)
    mobility <- mobility %>% filter(iso_3166_2_code == "BR-SC")

    mobility <- mobility[, cols]
    mobility$date <- ymd(mobility$date)

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
