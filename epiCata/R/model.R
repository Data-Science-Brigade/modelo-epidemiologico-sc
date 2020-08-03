NAMED_MODELS <- list(
    iter = list(FULL=1800, DEBUG=40, DEVELOP=400),
    warmup = list(FULL=1000, DEBUG=20, DEVELOP=200),
    chains = list(FULL=7, DEBUG=1, DEVELOP=8),
    verbose = list(FULL=FALSE, DEBUG=TRUE, DEVELOP=TRUE),
    adapt_delta = list(FULL=0.95, DEBUG=0.95, DEVELOP=0.95),
    max_treedepth = list(FULL=15, DEBUG=5, DEVELOP=10)
)

run_epidemiological_model <- function(selected_date, model_name="base",
                                      reference_date,
                                      allowed_interventions=NULL,
                                      allowed_locations,
                                      use_google_mobility=TRUE,
                                      mode,
                                      iter,
                                      warmup,
                                      chains,
                                      adapt_delta,
                                      max_treedepth,
                                      verbose
                                      ){
  require(rstan)
  require(lubridate)

  if(missing(allowed_locations)){
    stop("Must specify allowed locations")
  }
  if(missing(mode) && any(missing(iter),missing(warmup),missing(chains),
                          missing(adapt_delta),missing(max_treedepth),
                          missing(verbose))) {
    stop("epiCata/run_epidemiological_model: You should either specify mode or specify all model parameters")
  } else if(!missing(mode) && any(!missing(iter),!missing(warmup),
                                  !missing(chains),!missing(adapt_delta),
                                  !missing(max_treedepth),!missing(verbose))) {
    warning("epiCata/run_epidemiological_model: Mode specified but a parameter was overriden, using the parameter instead of the mode's default")
  }
  if(missing(mode)) {
    mode <- sprintf("CUSTOM-%s-%s-%s-%s-%s", iter, warmup, chains, adapt_delta, max_treedepth)
  } else {
    if(missing(iter)){ iter <- NAMED_MODELS$iter[[mode]] }
    if(missing(warmup)){ warmup <- NAMED_MODELS$warmup[[mode]] }
    if(missing(chains)){ chains <- NAMED_MODELS$chains[[mode]] }
    if(missing(adapt_delta)){ adapt_delta <- NAMED_MODELS$adapt_delta[[mode]] }
    if(missing(max_treedepth)){ max_treedepth <- NAMED_MODELS$max_treedepth[[mode]] }
    if(missing(verbose)){ verbose <- NAMED_MODELS$verbose[[mode]] }
  }
  mode_str <- sprintf("%s-%d-%d-%d-%f-%d", mode, iter, warmup, chains, adapt_delta, max_treedepth)

  # Read data
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

  cat(sprintf("\nRunning in mode %s", mode_str))

  fit <- rstan::sampling(model, data=stan_list$stan_data, iter=iter, warmup=warmup, chains=chains, verbose=verbose,
               control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth))

  out = rstan::extract(fit)

  model_output <- list(fit=fit, out=out, stan_list=stan_list, model_name=model_name, mode=mode_str, covid_data=covid_data)

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
