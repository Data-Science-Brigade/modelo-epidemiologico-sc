NAMED_MODELS <- list(
    iter = list(FULL=1800, DEBUG=40, DEVELOP=400),
    warmup = list(FULL=1000, DEBUG=20, DEVELOP=200),
    chains = list(FULL=7, DEBUG=1, DEVELOP=8),
    verbose = list(FULL=FALSE, DEBUG=TRUE, DEVELOP=TRUE),
    adapt_delta = list(FULL=0.95, DEBUG=0.95, DEVELOP=0.95),
    max_treedepth = list(FULL=15, DEBUG=5, DEVELOP=10)
)

run_epidemiological_model <- function(stan_list,
                                      nickname=NULL,
                                      model_name="base",
                                      mode=NULL,
                                      iter=NULL,
                                      warmup=NULL,
                                      chains=NULL,
                                      adapt_delta=NULL,
                                      max_treedepth=NULL,
                                      verbose=NULL
                                      ){
  require(rstan)
  require(lubridate)


  if(is.null(mode) && any(is.null(iter),is.null(warmup),is.null(chains),
                          is.null(adapt_delta),is.null(max_treedepth),
                          is.null(verbose))) {
    stop("epiCata/run_epidemiological_model: You should either specify mode or specify all model parameters")
  } else if(!is.null(mode) && any(!is.null(iter),!is.null(warmup),
                                  !is.null(chains),!is.null(adapt_delta),
                                  !is.null(max_treedepth),!is.null(verbose))) {
    warning("epiCata/run_epidemiological_model: Mode specified but a parameter was overriden, using the parameter instead of the mode's default")
  }
  if(is.null(mode)) {
    mode <- sprintf("CUSTOM-%s-%s-%s-%s-%s", iter, warmup, chains, adapt_delta, max_treedepth)
  } else {
    if(is.null(iter)){ iter <- NAMED_MODELS$iter[[mode]] }
    if(is.null(warmup)){ warmup <- NAMED_MODELS$warmup[[mode]] }
    if(is.null(chains)){ chains <- NAMED_MODELS$chains[[mode]] }
    if(is.null(adapt_delta)){ adapt_delta <- NAMED_MODELS$adapt_delta[[mode]] }
    if(is.null(max_treedepth)){ max_treedepth <- NAMED_MODELS$max_treedepth[[mode]] }
    if(is.null(verbose)){ verbose <- NAMED_MODELS$verbose[[mode]] }
  }
  mode_str <- sprintf("%s-%d-%d-%d-%f-%d", mode, iter, warmup, chains, adapt_delta, max_treedepth)

  model_filename <- sprintf("%s/stan-models/%s.stan", get_data_folder(), model_name)
  cat(sprintf("\nReading model: %s", model_filename))
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(model_filename)

  cat(sprintf("\nRunning in mode %s", mode_str))

  fit <- rstan::sampling(model, data=stan_list$stan_data, iter=iter, warmup=warmup, chains=chains, verbose=verbose,
               control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth))

  out = rstan::extract(fit)

  if(!is.null(nickname)) {
    mode_str <- sprintf("%s-%s", nickname, mode_str)
  }

  model_output <- list(fit=fit, out=out, stan_list=stan_list, model_name=model_name, mode=mode_str)

  model_output
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
