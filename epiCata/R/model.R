

run_epidemiological_model <- function(selected_date, model_name="base", mode="FULL",
                                      reference_date,
                                      allowed_interventions=NULL,
                                      allowed_locations,
                                      use_google_mobility=TRUE){
  require(rstan)
  require(lubridate)

  if(missing(reference_date)){
    reference_date <- Sys.Date()
  }else{
    reference_date <- ymd(reference_date)
  }

  cat(sprintf("\nReading Data"))
  if(missing(selected_date)){
    covid_data <- read_covid_data(reference_date=reference_date)
    interventions <- read_interventions(allowed_interventions=allowed_interventions)
    onset_to_death <- read_onset_to_death()
    google_mobility <-
      if(use_google_mobility){
        read_google_mobility()
      }else{
        NULL
      }
  }else{
    covid_data <- read_covid_data(selected_date, reference_date=reference_date)
    interventions <- read_interventions(selected_date, allowed_interventions=allowed_interventions)
    onset_to_death <- read_onset_to_death(selected_date)
    google_mobility <-
      if(use_google_mobility){
        read_google_mobility(selected_date)
      }else{
        NULL
      }
  }

  if(!missing(allowed_locations)){
    covid_data <- covid_data %>% filter(location_name %in% allowed_locations)
  }

  # Handle incorrect input: Assume any future dates were input incorrectly and are assigned to reference date
  if(any(covid_data$data_ocorrencia > (reference_date - days(1)))){
    covid_data[which(covid_data$data_ocorrencia > (reference_date - days(1))), "data_ocorrencia"] <- reference_date - days(1)
  }

  stan_list <- prepare_stan_data(covid_data, interventions, onset_to_death)

  model_filename <- sprintf("%s/stan-models/%s.stan", get_data_folder(), model_name)
  cat(sprintf("\nReading model: %s", model_filename))
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(model_filename)

  cat(sprintf("\nRunning in %s mode", mode))

  fit <-
    if(mode == "DEBUG") {
      sampling(model, data=stan_list$stan_data, iter=40, warmup=20, chains=1, verbose=TRUE,
               control = list(adapt_delta = 0.95, max_treedepth = 5))
    } else if (mode == "FULL") {
      sampling(model, data=stan_list$stan_data, iter=1800, warmup=1000, chains=7,
               thin=1, control = list(adapt_delta = 0.95, max_treedepth = 15))
    } else {
      sampling(model, data=stan_list$stan_data, iter=1000, warmup=500, chains=4,
               thin=1, control = list(adapt_delta = 0.95, max_treedepth = 10))
    }

  out = rstan::extract(fit)

  model_output <- list(fit=fit, out=out, stan_list=stan_list, model_name=model_name, mode=mode, covid_data=covid_data)

  save_fitted_model(model_output, reference_date)
}

save_fitted_model <- function(model_output, reference_date){

  reference_date_str <- strftime(reference_date, "%Y_%m_%d")

  # Assign a random number to JOBID
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
  filename_suffix <- paste0(reference_date_str, '_', model_output$model_name,'_', model_output$mode, '_', JOBID)

  dir.create(paste("results", reference_date_str, sep="/"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste("figures", reference_date_str, sep="/"), recursive = TRUE, showWarnings = FALSE)

  model_output$reference_date_str <- reference_date_str
  model_output$filename_suffix <- filename_suffix

  model_output_filename <- paste0('results/', reference_date_str, '/', filename_suffix, '-stanfit.Rdata')
  cat(sprintf("\nSaving model objects to %s", model_output_filename))
  save(model_output, file=model_output_filename)

  return(model_output)
}
