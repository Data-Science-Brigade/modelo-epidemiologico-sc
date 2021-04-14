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
                                      verbose=NULL,
                                      init_model_fname=NULL,
                                      is_weekly=FALSE
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

  init_model <- NULL
  if(!is.null(init_model_fname)){
    print("Initialising model with:")
    print(init_model_fname)
    load(init_model_fname)
    init_model <- model_output
    init_is_weekly <- ifelse(is.null(model_output[["is_weekly"]]),FALSE,model_output$is_weekly)
    if(is_weekly!=init_is_weekly){
      error("Model passed as initialisation and model to be ran are on different time scales!")
    }
    model_output <- NULL
  }

  init <- "random"
  if(!is.null(init_model)){
    init <- rstan::get_inits(init_model$fit)
    to_init <- init_model$out
    to_init_chains <- length(init)
    if(chains<to_init_chains){
      init <- init[1:chains]
    }
    init_chains <- length(init)
    chain_length <- dim(to_init[["mu"]])[1]/to_init_chains
    n_chains <- 0
    while(n_chains<init_chains){
      print(names(init[[n_chains+1]]))
      for(name in names(init[[n_chains+1]])){
        idx <- (1+chain_length*n_chains):(chain_length*(n_chains+1))
        dims <- dim(to_init[[name]])
        if(length(dims)==1){
          init[[n_chains+1]][[name]] <- mean(to_init[[name]][idx])
        } else if(length(dims)==2){
          init[[n_chains+1]][[name]][1:dims[2]] <- mean(to_init[[name]][idx,1:dims[2]])
        } else if(length(dims)==3){
          init[[n_chains+1]][[name]][1:dims[2],1:dims[3]] <- mean(to_init[[name]][idx,1:dims[2],1:dims[3]])
        } else {
          error("Wrong number of dimensions in init vector")
        }
         #<- apply(, 1, function(array){(array[])})
      }
      n_chains <- n_chains+1
    }
  }
  init_model <- NULL
  gc()

  cat(sprintf("\nRunning in mode %s", mode_str))

  fit <- rstan::sampling(model, data=stan_list$stan_data, iter=iter, warmup=warmup, chains=chains, verbose=verbose,
               control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), init=init)

  out = rstan::extract(fit)

  if(!is.null(nickname)) {
    mode_str <- sprintf("%s-%s", nickname, mode_str)
  }

  model_output <- list(fit=fit, out=out, stan_list=stan_list, model_name=paste0(model_name,ifelse(is_weekly,"_weekly","")), mode=mode_str, is_weekly=is_weekly)

  model_output
}

save_fitted_model <- function(model_output, reference_date, save_path="./"){

  reference_date_str <- strftime(reference_date, "%Y_%m_%d")

  # Assign a random number to JOBID
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
  filename_suffix <- paste0(reference_date_str, '_', model_output$model_name,'_', model_output$mode, '_', JOBID)

  dir.create(paste0(save_path, "results/", reference_date_str), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(save_path, "figures/", reference_date_str), recursive = TRUE, showWarnings = FALSE)

  model_output$reference_date_str <- reference_date_str
  model_output$filename_suffix <- filename_suffix

  model_output_filename <- paste0(save_path, 'results/', reference_date_str, '/', filename_suffix, '-stanfit.Rdata')
  cat(sprintf("\nSaving model objects to %s", model_output_filename))
  save(model_output, file=model_output_filename)

  return(model_output)
}

change_model_name <- function(model_output, new_model_name, save_path="./"){

  reference_date_str <- model_output$reference_date_str
  model_output$model_name <- new_model_name

  # Assign a random number to JOBID
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
  filename_suffix <- paste0(reference_date_str, '_', model_output$model_name,'_', model_output$mode, '_', JOBID)

  dir.create(paste0(save_path, "results/", reference_date_str), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(save_path, "figures/", reference_date_str), recursive = TRUE, showWarnings = FALSE)

  model_output$filename_suffix <- filename_suffix

  model_output_filename <- paste0(save_path, 'results/', reference_date_str, '/', filename_suffix, '-stanfit.Rdata')
  cat(sprintf("\nSaving model objects to %s", model_output_filename))
  save(model_output, file=model_output_filename)

  return(model_output)
}
