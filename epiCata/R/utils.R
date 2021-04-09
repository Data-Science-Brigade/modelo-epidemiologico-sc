run_model_with_opt <- function(opt, default_locations){
  if(!is.null(opt[["reference_date"]])) { opt[["reference_date"]] <- ymd(opt[["reference_date"]]) }

  if(any(opt[["allowed_locations"]] != default_locations)) {
    exit("Allowed locations not implemented yet")
  }

  if(all(is.null(opt[["mode"]]), is.null(opt[["iter"]]), is.null(opt[["warmup"]]), is.null(opt[["chains"]]),
         is.null(opt[["adapt_delta"]]), is.null(opt[["max_treedepth"]]), is.null(opt[["verbose"]]))){
    warning("No model parameter passed, running model in DEBUG mode")
    opt[["mode"]] <- "DEBUG"
  }

  for(name in names(opt)){
    print(name)
    print(opt[[name]])
    if(!is.null(opt[[name]]) && as.character(opt[[name]]) == "NULL"){
      opt[[name]] <- NULL
    }
    print(opt[[name]])
  }

  # Change \ to / in save_path
  opt[["save_path"]] <- gsub('\\\\', '/', opt[["save_path"]])
  # Add last slash since some functions expect it to be there
  if(!endsWith(opt[["save_path"]], "/")) {
    warning("Path passed did not have last slash, which is expected")
    opt[["save_path"]] <- paste0(opt[["save_path"]], "/")
  }

  # Read data
  cat(sprintf("\nReading Data"))
  covid_data <- read_covid_data(opt[["deaths"]], opt[["population"]], opt[["reference_date"]],
                                allowed_locations = opt[["allowed_locations"]])
  interventions <- read_interventions(opt[["interventions"]], allowed_interventions=NULL, #TODO?
                                      google_mobility_filename = opt[["google_mobility"]],
                                      google_mobility_window_size = opt[["google_mobility_window_size"]])
  onset_to_death <- read_onset_to_death(opt[["onset_to_death"]])
  IFR <- read_IFR(opt[["ifr"]])
  serial_interval <- read_serial_interval(opt[["serial_interval"]])
  infection_to_onset <- read_infection_to_onset(opt[["infection_to_onset"]])
  population <- read_pop(opt[["population"]])
  stan_list <-
    prepare_stan_data(covid_data,
                      interventions,
                      onset_to_death,
                      IFR,
                      serial_interval,
                      infection_to_onset,
                      population,
                      is_weekly = opt[["is_weekly"]])

  model_output <-
    run_epidemiological_model(stan_list,
                              mode = opt[["mode"]],
                              nickname = opt[["nickname"]],
                              iter = opt[["iter"]],
                              warmup = opt[["warmup"]],
                              chains = opt[["chains"]],
                              adapt_delta = opt[["adapt_delta"]],
                              max_treedepth = opt[["max_treedepth"]],
                              verbose = opt[["verbose"]],
                              init_model_fname = opt[["model_init_filename"]],
                              is_weekly = opt[["is_weekly"]]
    )
  model_output[["covid_data"]] <- covid_data
  model_output <- save_fitted_model(model_output, opt[["reference_date"]], save_path=opt[["save_path"]])

  model_output
}

make_option_list <- function(default_locations,
                             reference_date=NULL,
                             aggregate_name=NULL,
                             nickname=NULL,
                             default_locations_text = "",
                             deaths_csv = "deaths.csv",
                             pop_csv = "pop_and_regions.csv",
                             onset_to_death_csv = "onset_to_death.csv",
                             interventions_xls = "interventions.xls", # FIXME? Use an open-source format?
                             google_mobility_csv = "Global_Mobility_Report.csv",
                             google_mobility_window_size = 0,
                             ifr_csv = "IFR.csv",
                             serial_interval_csv = "serial_interval.csv",
                             save_path = "../",
                             model_init_filename = NULL,
                             is_weekly = TRUE
                             ) {
  if(is.null(reference_date)) {
    require(lubridate)
    reference_date <- today()
  }
  require(optparse)

  list(
    make_option(c("-m", "--mode"),
                type = "character", default = NULL, dest = "mode",
                help = "Mode to run the model (FULL/DEBUG/DEVELOP). If not specified you must specify -i -w -c -d -t and -v"
    ),
    make_option(c("-i", "--iterations"),
                type = "integer", default = NULL, dest = "iter",
                help = "How many iterations to run the model with, the model takes samples using iterations-warmup iterations for each chain. OVERRIDES: -m's setting"
    ),
    make_option(c("-w", "--warmup"),
                type = "integer", default = NULL, dest = "warmup",
                help = "How many warmup iterations to run the model for, these iterations count towards the total number of iterations specified by -i. OVERRIDES: -m's setting"
    ),
    make_option(c("-c", "--chains"),
                type = "integer", default = NULL, dest = "chains",
                help = "How many of chains should be run. This uses multiple cores if your computer has them. OVERRIDES: -m's setting"
    ),
    make_option(c("-x", "--adapt-delta"),
                type = "double", default = NULL, dest = "adapt_delta",
                help = "Model's target acceptance rate. OVERRIDES: -m's setting"
    ),
    make_option(c("-t", "--max-treedepth"),
                type = "integer", default = NULL, dest = "max_treedepth",
                help = "The maximum treedepth the model will use when perfoming sampling. OVERRIDES: -m's setting"
    ),
    make_option(c("-v", "--verbose"),
                type = "logical", default = NULL, dest = "verbose",
                help = "rstan::sample's verbosity. OVERRIDES: -m's setting"
    ),
    make_option(c("-r", "--reference-date"),
                type = "character", default = reference_date, dest = "reference_date",
                help = sprintf("Reference date for the model in yyyy_mm_dd format. Will default to %s", reference_date)
    ),
    make_option(c("-l", "--allowed-locations"),
                type = "character", default = default_locations, dest = "allowed_locations",
                help = sprintf("List of Allowed locations, the default is%s: %s", default_locations_text, paste(default_locations, collapse=", "))
    ),
    make_option(c("-a", "--aggregate-name"),
                type = "character", default = aggregate_name, dest = "aggregate_name",
                help = sprintf("If we should aggregate the city data into a state data for plotting an aggregate version, the name of the aggregate should be passed here. If NULL it won't be used. Default: %s", ifelse(is.null(aggregate_name), "NULL", aggregate_name))
    ),
    make_option(c("-d", "--deaths"),
                type = "character", default = deaths_csv, dest = "deaths",
                help = sprintf("CSV file from which to read the deaths from. If not specified will default to %s", deaths_csv)
    ),
    make_option(c("-p", "--population"),
                type = "character", default = pop_csv, dest = "population",
                help = sprintf("CSV file from which to read the population and super-region data from. If not specified will default to %s", pop_csv)
    ),
    make_option(c("-o", "--onset-to-death"),
                type = "character", default = onset_to_death_csv, dest = "onset_to_death",
                help = sprintf("CSV file containing onset-to-death data for the model. The default is %s", onset_to_death_csv)
    ),
    make_option(c("-n", "--interventions"),
                type = "character", default = interventions_xls, dest = "interventions",
                help = sprintf("XLS file containing intervention data for the model. The default is %s", interventions_xls)
    ),
    make_option(c("-g", "--google-mobility"),
                type = "character", default = google_mobility_csv, dest = "google_mobility",
                help = sprintf("CSV file containing google mobility data for the model. The default is %s and if you do not wish to use google mobility data pass NULL", google_mobility_csv)
    ),
    make_option(c("-k", "--google-mobility-window-size"),
                type = "integer", default = google_mobility_window_size, dest = "google_mobility_window_size",
                help = sprintf("Size of the window for the rolling average on google mobility data. The default is %d", google_mobility_window_size)
    ),
    make_option(c("-f", "--ifr"),
                type = "character", default = ifr_csv, dest = "ifr",
                help = sprintf("IFR CSV. The default is %s", ifr_csv)
    ),
    make_option(c("-s", "--serial-interval"),
                type = "character", default = serial_interval_csv, dest = "serial_interval",
                help = sprintf("Serial Interval CSV. The default is %s", serial_interval_csv)
    ),
    make_option(c("-b", "--save-path"),
                type = "character", default = save_path, dest = "save_path",
                help = sprintf("Path to save the /figures and /results folders. The default is %s", save_path)
    ),
    make_option(c("-z", "--nickname"),
                type = "character", default = nickname, dest = "nickname",
                help = sprintf("Model nickname to prepend to mode. If NULL it won't be used. Default: %s", ifelse(is.null(nickname), "NULL", nickname))
    ),
    make_option(c("-e", "--model_init_filename"),
                type = "character", default = model_init_filename, dest = "model_init_filename",
                help = sprintf("Model to init from. If NULL it won't be used. Default: %s", ifelse(is.null(model_init_filename), "NULL", model_init_filename))
    ),
    make_option(c("-y", "--is-weekly"),
                type = "logical", default = is_weekly, dest = "is_weekly",
                help = sprintf("If the model is to be ran with weekly aggregation. Default: %s", is_weekly)
    )
  )
}


get_ranges_counting_missing_data <- function(model_output, forecast) {
  rr <- list()

  rr$is_weekly <- length(model_output$is_weekly) && model_output$is_weekly

  rr$forecast <- ifelse(rr$is_weekly, ceiling(forecast / 7), forecast)

  # Get dates
  rr$min_missing_date <- min(as_date(model_output$covid_data$data_ocorrencia))
  rr$min_date <- min(reduce(model_output$stan_list$dates, min))
  rr$max_date <- reduce(model_output$stan_list$dates, max)
  rr$max_forecast_date <- rr$max_date + rr$forecast
  if(rr$is_weekly){
    rr$min_missing_date <- ymd(cut(rr$min_missing_date, "week", start.on.monday = TRUE))
    rr$min_date <- ymd(cut(rr$min_date, "week", start.on.monday = TRUE))
    rr$max_date <- ymd(cut(rr$max_date, "week", start.on.monday = TRUE))
    rr$max_forecast_date <- rr$max_date + rr$forecast * 7
  }

  # Get data length
  rr$missingN <- time_length(rr$min_date-rr$min_missing_date, ifelse(rr$is_weekly,"week","day"))
  rr$fullN <- time_length(rr$max_date-rr$min_missing_date, ifelse(rr$is_weekly,"week","day"))+1
  rr$N <- time_length(rr$max_date-rr$min_date, ifelse(rr$is_weekly,"week","day"))+1
  rr$N2 <- time_length(rr$max_forecast_date-rr$min_date, ifelse(rr$is_weekly,"week","day"))+1

  rr
}


get_merged_forecast_dfs <- function(location_names, model_output, forecast=30, aggregate_name=NULL) {
  require(lubridate)
  require(tidyverse)
  if(length(location_names)>1){
    if(is.null(aggregate_name)){ aggregate_name <- "AGG" }

    rr <- get_ranges_counting_missing_data(model_output, forecast)


    #### COMPUTE PREDICTIONS ####
    predicted_cases <- array(0, dim=rr$N)
    predicted_cases_li <- array(0, dim=rr$N)
    predicted_cases_ui <- array(0, dim=rr$N)
    predicted_cases_li2 <- array(0, dim=rr$N)
    predicted_cases_ui2 <- array(0, dim=rr$N)

    #### COMPUTE ESTIMATED DEATHS ####
    estimated_deaths <- array(0, dim=rr$N)
    estimated_deaths_li <- array(0, dim=rr$N)
    estimated_deaths_ui <- array(0, dim=rr$N)
    estimated_deaths_li2 <- array(0, dim=rr$N)
    estimated_deaths_ui2 <- array(0, dim=rr$N)

    #### COMPUTE FORECAST OF ESTIMATED DEATHS ####
    estimated_deaths_forecast <- array(0, dim=rr$forecast+1)
    estimated_deaths_li_forecast <- array(0, dim=rr$forecast+1)
    estimated_deaths_ui_forecast <- array(0, dim=rr$forecast+1)

    #### COMPUTE RT ####
    rt <- array(0, dim=rr$N)
    rt_li <- array(0, dim=rr$N)
    rt_ui <- array(0, dim=rr$N)
    rt_li2 <- array(0, dim=rr$N)
    rt_ui2 <- array(0, dim=rr$N)
    # Weighted average sum
    rt_n <- array(0, dim=rr$N)

    #### Reported cases and deaths ####
    agg_reported_cases <- array(0, dim=rr$fullN)
    agg_reported_deaths <- array(0, dim=rr$fullN)

    for(location in location_names) {
      locations_dfs <- get_merged_forecast_dfs_on_model_data(location, model_output, forecast=forecast, aggregate_name=location)
      locations_data_df <- locations_dfs$data_location
      locations_forecast_df <- locations_dfs$data_location_forecast

      i <- which(model_output$stan_list$available_locations == location)

      location_start_date <- min(model_output$stan_list$dates[[i]])
      location_end_date <- max(model_output$stan_list$dates[[i]])

      # Index of this location's start date in the aggregate vector
      loc_N_s <-  1 + time_length(location_start_date - rr$min_date, ifelse(rr$is_weekly, "week", "day"))
      # Length of this location's variable
      loc_N_l <- model_output$stan_list$stan_data$N[i]

      # Indexes to be accessed in the aggregate vector
      agg_idx <- loc_N_s:(loc_N_s+loc_N_l-1)
      # Indexes to be accessed in this location's data
      loc_idx <- which(location_start_date<=locations_data_df$time & locations_data_df$time<=location_end_date)

      predicted_cases[agg_idx] <- predicted_cases[agg_idx] + locations_data_df$predicted_cases[loc_idx]
      predicted_cases_li[agg_idx] <- predicted_cases_li[agg_idx] + locations_data_df$predicted_min[loc_idx]
      predicted_cases_ui[agg_idx] <- predicted_cases_ui[agg_idx] + locations_data_df$predicted_max[loc_idx]
      predicted_cases_li2[agg_idx] <- predicted_cases_li2[agg_idx] + locations_data_df$predicted_min2[loc_idx]
      predicted_cases_ui2[agg_idx] <- predicted_cases_ui2[agg_idx] + locations_data_df$predicted_max2[loc_idx]

      #### COMPUTE ESTIMATED DEATHS ####
      estimated_deaths[agg_idx] <- estimated_deaths[agg_idx] + locations_data_df$estimated_deaths[loc_idx]
      estimated_deaths_li[agg_idx] <- estimated_deaths_li[agg_idx] + locations_data_df$death_min[loc_idx]
      estimated_deaths_ui[agg_idx] <- estimated_deaths_ui[agg_idx] + locations_data_df$death_max[loc_idx]
      estimated_deaths_li2[agg_idx] <- estimated_deaths_li2[agg_idx] + locations_data_df$death_min2[loc_idx]
      estimated_deaths_ui2[agg_idx] <- estimated_deaths_ui2[agg_idx] + locations_data_df$death_max2[loc_idx]

      #### COMPUTE FORECAST OF ESTIMATED DEATHS ####
      estimated_deaths_forecast <- estimated_deaths_forecast + locations_forecast_df$estimated_deaths_forecast
      estimated_deaths_li_forecast <- estimated_deaths_li_forecast + locations_forecast_df$death_min_forecast
      estimated_deaths_ui_forecast <- estimated_deaths_ui_forecast + locations_forecast_df$death_max_forecast

      #### COMPUTE RT ####
      # Weighted by pop
      rt[agg_idx] <- rt[agg_idx] + locations_data_df$rt[loc_idx] * model_output$stan_list$stan_data$pop[i]
      rt_li[agg_idx] <- rt_li[agg_idx] + locations_data_df$rt_min[loc_idx] * model_output$stan_list$stan_data$pop[i]
      rt_ui[agg_idx] <- rt_ui[agg_idx] + locations_data_df$rt_max[loc_idx] * model_output$stan_list$stan_data$pop[i]
      rt_li2[agg_idx] <- rt_li2[agg_idx] + locations_data_df$rt_min2[loc_idx] * model_output$stan_list$stan_data$pop[i]
      rt_ui2[agg_idx] <- rt_ui2[agg_idx] + locations_data_df$rt_max2[loc_idx] * model_output$stan_list$stan_data$pop[i]
      rt_n[agg_idx] <- rt_n[agg_idx] + model_output$stan_list$stan_data$pop[i]

      #### ORIGINAL DATA
      # We'll get reported cases and deaths from the original covid data, since in
      # the data preparation phase (`R/preprocessing/get_stan_data_for_location`),
      # there is a filter that can remove some data before feeding it to the model.
      original_data <- model_output$covid_data %>% filter(location_name == location)
      if(rr$is_weekly){
        original_data <- original_data %>%
          group_by(
            location_name,
            data_ocorrencia = cut(data_ocorrencia, "week", start.on.monday = TRUE)
          ) %>%
          summarise(
            casos=sum(casos),
            obitos=sum(obitos))
        original_data$data_ocorrencia <- ymd(original_data$data_ocorrencia)
      }#TODO

      min_original_data_date <- min(original_data$data_ocorrencia)
      max_original_data_date <- min(max(original_data$data_ocorrencia), rr$max_date)
      original_data_start <- 1 + time_length(min_original_data_date - rr$min_missing_date, ifelse(rr$is_weekly, "week", "day"))
      original_data_length <- time_length(max_original_data_date - min_original_data_date, ifelse(rr$is_weekly, "week", "day"))
      # Index of the original data on the aggregate vector
      orig_agg_idx <- original_data_start:(original_data_start+original_data_length)

      agg_reported_cases[orig_agg_idx] <- agg_reported_cases[orig_agg_idx] + original_data$casos

      agg_reported_deaths[orig_agg_idx] <- agg_reported_deaths[orig_agg_idx] + original_data$obitos
    }
    # Average on Rt
    rt <- rt/rt_n
    rt_li <- rt_li/rt_n
    rt_ui <- rt_ui/rt_n
    rt_li2 <- rt_li2/rt_n
    rt_ui2 <- rt_ui2/rt_n

    data_location <- data.frame(
      "time" = rr$min_date + ifelse(rr$is_weekly,weeks,days)(0:(rr$N-1)),
      "location_name" = rep(aggregate_name, rr$N),
      "reported_cases" = agg_reported_cases[(rr$missingN+1):(rr$missingN+rr$N)],
      "deaths" = agg_reported_deaths[(rr$missingN+1):(rr$missingN+rr$N)],
      "predicted_cases" = predicted_cases,
      "predicted_min" = predicted_cases_li,
      "predicted_max" = predicted_cases_ui,
      "predicted_min2" = predicted_cases_li2,
      "predicted_max2" = predicted_cases_ui2,
      "estimated_deaths" = estimated_deaths,
      "death_min" = estimated_deaths_li,
      "death_max"= estimated_deaths_ui,
      "death_min2" = estimated_deaths_li2,
      "death_max2"= estimated_deaths_ui2,
      "rt" = rt,
      "rt_min" = rt_li,
      "rt_max" = rt_ui,
      "rt_min2" = rt_li2,
      "rt_max2" = rt_ui2)

    missing_original_data <- data.frame("time" = rr$min_missing_date + ifelse(rr$is_weekly,weeks,days)(0:(rr$fullN-1)),
                                        "location_name" = rep(aggregate_name, rr$fullN),
                                        "reported_cases" = agg_reported_cases,
                                        "deaths" = agg_reported_deaths) %>%
      mutate(cum_cases=cumsum(reported_cases)) %>% filter(cum_cases > 0, time<rr$min_date) %>%
      select(-c(cum_cases))

    data_location <- bind_rows(missing_original_data, data_location) %>% replace(is.na(.), 0)

    #### ADD CUMULATIVE SUMS ####
    data_location <-
      data_location %>%
      arrange(time) %>%
      mutate(reported_cases_c = cumsum(reported_cases),
             predicted_cases_c = cumsum(predicted_cases),
             predicted_min_c = cumsum(predicted_min),
             predicted_max_c = cumsum(predicted_max),
             deaths_c = cumsum(deaths),
             estimated_deaths_c =  cumsum(estimated_deaths),
             death_min_c = cumsum(death_min),
             death_max_c= cumsum(death_max))


    data_location_forecast <- data.frame("time" = rr$max_date + ifelse(rr$is_weekly,weeks,days)(0:rr$forecast),
                                         "location_name" = rep(aggregate_name, rr$forecast + 1),
                                         "estimated_deaths_forecast" = estimated_deaths_forecast,
                                         "death_min_forecast" = estimated_deaths_li_forecast,
                                         "death_max_forecast"= estimated_deaths_ui_forecast)

    list(data_location=data_location, data_location_forecast=data_location_forecast)
  } else {
    get_merged_forecast_dfs_on_model_data(location_names, model_output, forecast=forecast, aggregate_name=aggregate_name)
  }
}

get_merged_forecast_dfs_on_model_data <- function(location_names, model_output, forecast=30, aggregate_name=NULL) {
  require(lubridate)
  require(tidyverse)
  if(is.null(aggregate_name)){
    aggregate_name <- if(length(location_names)==1) {
      location_names
    } else {
      "AGG"
    }
  }

  rr <- get_ranges_counting_missing_data(model_output, forecast)

  pred_dims <- dim(model_output$out$prediction)
  prediction_samples <- array(0, dim=c(pred_dims[[1]],rr$N))
  estimated_deaths_samples <- array(0, dim=c(pred_dims[[1]],rr$N))
  rt_samples <- array(0, dim=c(pred_dims[[1]],rr$N))
  rt_samples_n <- array(0, dim=c(pred_dims[[1]],rr$N))
  estimated_deaths_forecast_samples <- array(0, dim=c(pred_dims[[1]],rr$forecast+1))
  agg_reported_cases <- array(0, dim=rr$fullN)
  agg_reported_deaths <- array(0, dim=rr$fullN)

  for(location in location_names) {
    i <- which(model_output$stan_list$available_locations == location)

    location_start_date <- min(model_output$stan_list$dates[[i]])
    location_end_date <- max(model_output$stan_list$dates[[i]])

    # Index of this location's start date in the aggregate vector
    loc_N_s <- 1 + time_length(location_start_date - rr$min_date, ifelse(rr$is_weekly, "week", "day"))
    # Length of this location's variable
    loc_N_l <- model_output$stan_list$stan_data$N[i]

    # Indexes to be accessed in the aggregate vector
    agg_idx <- loc_N_s:(loc_N_s+loc_N_l-1)
    # Indexes to be accessed in this location's data
    loc_idx <- 1:loc_N_l


    prediction_samples[,agg_idx] <- prediction_samples[,agg_idx] + model_output$out$prediction[,loc_idx,i]

    estimated_deaths_samples[,agg_idx] <- estimated_deaths_samples[,agg_idx] + model_output$out$E_deaths[,loc_idx,i]

    # (UNUSED) Unweighted
    #rt_samples[,agg_idx] <- rt_samples[,agg_idx] + model_output$out$Rt[,loc_idx,i]
    # Weighted by pop
    rt_samples[,agg_idx] <- rt_samples[,agg_idx] + model_output$out$Rt[,loc_idx,i] * model_output$stan_list$stan_data$pop[i]

    # (UNUSED) Unweighted
    #rt_samples_n[,agg_idx] <- rt_samples_n[,agg_idx] + 1
    # Weighted by pop
    rt_samples_n[,agg_idx] <- rt_samples_n[,agg_idx] + model_output$stan_list$stan_data$pop[i]

    estimated_deaths_forecast_samples[,1:(rr$forecast+1)] <- estimated_deaths_samples[,1:(rr$forecast+1)] + model_output$out$E_deaths[,loc_N_l:(loc_N_l+rr$forecast),i]

    # (UNUSED) Getting cases and deaths from the data available for the model's input
    #agg_reported_cases[agg_idx] <-agg_reported_cases[orig_agg_idx] + model_output$stan_list$reported_cases[[i]]
    #agg_reported_deaths[agg_idx] <- agg_reported_deaths[orig_agg_idx] + model_output$stan_list$deaths_by_location[[i]]

    #### ORIGINAL DATA
    # We'll get reported cases and deaths from the original covid data, since in
    # the data preparation phase (`R/preprocessing/get_stan_data_for_location`),
    # there is a filter that can remove some data before feeding it to the model.
    original_data <- model_output$covid_data %>% filter(location_name == location)
    if(rr$is_weekly){
      original_data <- original_data %>%
        group_by(
          location_name,
          data_ocorrencia = cut(data_ocorrencia, "week", start.on.monday = TRUE)
        ) %>%
        summarise(
          casos=sum(casos),
          obitos=sum(obitos))
      original_data$data_ocorrencia <- ymd(original_data$data_ocorrencia)
    }

    min_original_data_date <- min(original_data$data_ocorrencia)
    max_original_data_date <- min(max(original_data$data_ocorrencia), rr$max_date)
    original_data_start <- 1 + time_length(min_original_data_date - rr$min_missing_date, ifelse(rr$is_weekly, "week", "day"))
    original_data_length <- time_length(max_original_data_date - min_original_data_date, ifelse(rr$is_weekly, "week", "day"))
    orig_agg_idx <- original_data_start:(original_data_start+original_data_length)

    agg_reported_cases[orig_agg_idx] <- agg_reported_cases[orig_agg_idx] + original_data$casos

    agg_reported_deaths[orig_agg_idx] <- agg_reported_deaths[orig_agg_idx] + original_data$obitos
  }
  # Average on Rt
  rt_samples <- rt_samples/rt_samples_n

  #### COMPUTE PREDICTIONS ####
  predicted_cases <- colMeans(prediction_samples)
  predicted_cases_li <- matrixStats::colQuantiles(prediction_samples, probs=.025)
  predicted_cases_ui <- matrixStats::colQuantiles(prediction_samples, probs=.975)
  predicted_cases_li2 <- matrixStats::colQuantiles(prediction_samples, probs=.25)
  predicted_cases_ui2 <- matrixStats::colQuantiles(prediction_samples, probs=.75)

  #### COMPUTE ESTIMATED DEATHS ####
  estimated_deaths <- colMeans(estimated_deaths_samples)
  estimated_deaths_li <- matrixStats::colQuantiles(estimated_deaths_samples, probs=.025)
  estimated_deaths_ui <- matrixStats::colQuantiles(estimated_deaths_samples, probs=.975)
  estimated_deaths_li2 <- matrixStats::colQuantiles(estimated_deaths_samples, probs=.25)
  estimated_deaths_ui2 <- matrixStats::colQuantiles(estimated_deaths_samples, probs=.75)

  #### COMPUTE FORECAST OF ESTIMATED DEATHS ####
  estimated_deaths_forecast <- colMeans(estimated_deaths_forecast_samples)
  estimated_deaths_li_forecast <- matrixStats::colQuantiles(estimated_deaths_forecast_samples, probs=.025)
  estimated_deaths_ui_forecast <- matrixStats::colQuantiles(estimated_deaths_forecast_samples, probs=.975)
  estimated_deaths_li2_forecast <- matrixStats::colQuantiles(estimated_deaths_forecast_samples, probs=.25)
  estimated_deaths_ui2_forecast <- matrixStats::colQuantiles(estimated_deaths_forecast_samples, probs=.75)

  #### COMPUTE RT ####
  rt <- colMeans(rt_samples)
  rt_li <- matrixStats::colQuantiles(rt_samples,probs=.025)
  rt_ui <- matrixStats::colQuantiles(rt_samples,probs=.975)
  rt_li2 <- matrixStats::colQuantiles(rt_samples,probs=.25)
  rt_ui2 <- matrixStats::colQuantiles(rt_samples,probs=.75)

  data_location <- data.frame(
                              "time" = rr$min_date + ifelse(rr$is_weekly,weeks,days)(0:(rr$N-1)),
                              "location_name" = rep(aggregate_name, rr$N),
                              "reported_cases" = agg_reported_cases[(rr$missingN+1):(rr$missingN+rr$N)],
                              "deaths" = agg_reported_deaths[(rr$missingN+1):(rr$missingN+rr$N)],
                              "predicted_cases" = predicted_cases,
                              "predicted_min" = predicted_cases_li,
                              "predicted_max" = predicted_cases_ui,
                              "predicted_min2" = predicted_cases_li2,
                              "predicted_max2" = predicted_cases_ui2,
                              "estimated_deaths" = estimated_deaths,
                              "death_min" = estimated_deaths_li,
                              "death_max"= estimated_deaths_ui,
                              "death_min2" = estimated_deaths_li2,
                              "death_max2"= estimated_deaths_ui2,
                              "rt" = rt,
                              "rt_min" = rt_li,
                              "rt_max" = rt_ui,
                              "rt_min2" = rt_li2,
                              "rt_max2" = rt_ui2)

  missing_original_data <- data.frame("time" = rr$min_missing_date + ifelse(rr$is_weekly,weeks,days)(0:(rr$fullN-1)),
                                      "location_name" = rep(aggregate_name, rr$fullN),
                                      "reported_cases" = agg_reported_cases,
                                      "deaths" = agg_reported_deaths) %>%
    mutate(cum_cases=cumsum(reported_cases)) %>% filter(cum_cases > 0, time<rr$min_date) %>%
    select(-c(cum_cases))

  data_location <- bind_rows(missing_original_data, data_location) %>% replace(is.na(.), 0)

  #### ADD CUMULATIVE SUMS ####
  data_location <-
    data_location %>%
    arrange(time) %>%
    mutate(reported_cases_c = cumsum(reported_cases),
           predicted_cases_c = cumsum(predicted_cases),
           predicted_min_c = cumsum(predicted_min),
           predicted_max_c = cumsum(predicted_max),
           deaths_c = cumsum(deaths),
           estimated_deaths_c =  cumsum(estimated_deaths),
           death_min_c = cumsum(death_min),
           death_max_c= cumsum(death_max))


  data_location_forecast <- data.frame("time" = rr$max_date + ifelse(rr$is_weekly,weeks,days)(0:rr$forecast),
                                       "location_name" = rep(aggregate_name, rr$forecast + 1),
                                       "estimated_deaths_forecast" = estimated_deaths_forecast,
                                       "death_min_forecast" = estimated_deaths_li_forecast,
                                       "death_max_forecast"= estimated_deaths_ui_forecast)

  list(data_location=data_location, data_location_forecast=data_location_forecast)
}

round_y_breaks <- function(max_y_break, n_breaks=4, min_y_break=0){
  n_integer_digits <- floor(log10(max_y_break)) - 1
  # Check if we only have two digits or less
  if(n_integer_digits<=0){
    base <- 10^(n_integer_digits+1) * (floor(max_y_break/10^(n_integer_digits+1)))
    second_base <- if(max_y_break-base<=5) {5} else {10}
  } else { # Otherwise, round as usual
    first_digit <- floor(max_y_break/10^(n_integer_digits+1))
    base <- 10^(n_integer_digits+1) * first_digit
    second_base_first_digit <- (1+floor((max_y_break-base)/10^(n_integer_digits)))
    second_base <- 10^(n_integer_digits) * second_base_first_digit
    # But check if the first digit of the second base is higher than 5
    # Or if the y_break divided by the number of breaks won't be divisible by 5
    #if(second_base_first_digit>5 && (((base+second_base)/n_breaks) %% 5)>0){
      # If so, just increment the first digit by one which guarantees divisible by 5
    #  base <- 10^(n_integer_digits+1) * (first_digit + 1)
    #  second_base <- 0
    #}
    # While the new max_y_break is not divisible by 5 times the number of breaks
    # And the second_base_first_digit is lower than 10
    while(!near(((((base+second_base)-min_y_break)/n_breaks) %% 5),0) && second_base_first_digit<10) {
      second_base_first_digit <- second_base_first_digit+1
      second_base <- 10^(n_integer_digits) * second_base_first_digit
    }
  }
  max_y_break <- base + second_base
  max_y_break
}

save_data_for_dashboard <- function(model_output, save_path="./", aggregate_name=NULL) {
  available_locations <- model_output$stan_list$available_locations

  dir.create(paste0(save_path, "dashboard_results/", model_output$reference_date_str), recursive = TRUE, showWarnings = FALSE)

  for(location_name in available_locations){
    print(paste0("Saving data for ", location_name))
    locdir <- paste0(save_path, "dashboard_results/", model_output$reference_date_str, "/", location_name)
    dir.create(locdir, recursive = TRUE, showWarnings = FALSE)
    dfs <- get_merged_forecast_dfs(location_name, model_output)
    write.csv(dfs$data_location, paste0(locdir, "/data_location.csv"))
    write.csv(dfs$data_location_forecast, paste0(locdir, "/data_location_forecast.csv"))
    write_lines(location_name, paste0(locdir, "/location_names.txt"))
    write_file(location_name, paste0(locdir, "/aggregate_name.txt"))
  }
  if(!is.null(aggregate_name)){
    print(paste0("Saving data for ", aggregate_name))
    locdir <- paste0(save_path, "dashboard_results/", model_output$reference_date_str, "/", aggregate_name)
    dir.create(locdir, recursive = TRUE, showWarnings = FALSE)
    dfs <- get_merged_forecast_dfs(available_locations, model_output, aggregate_name = aggregate_name)
    write.csv(dfs$data_location, paste0(locdir, "/data_location.csv"))
    write.csv(dfs$data_location_forecast, paste0(locdir, "/data_location_forecast.csv"))
    write_lines(available_locations, paste0(locdir, "/location_names.txt"))
    write_file(aggregate_name, paste0(locdir, "/aggregate_name.txt"))
  }
}

