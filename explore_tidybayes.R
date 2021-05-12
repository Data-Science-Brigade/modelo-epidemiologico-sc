library(tidyverse)
library(tidybayes)

# Make sure to have a model_output model on the global environment
# ex: load("../../results/2021_04_19/2021_04_19_base_weekly_MAC+AGG-DEVELOP-3000-2400-4-0.950000-8_98584-stanfit.Rdata")


get_tidy_df <- function(model_fit, is_reported=FALSE){

  covariates <-
    model_fit$stan_list$common_interventions %>% dplyr::select(-1) %>% colnames()
  covariates <- gsub("_percent_change_from_baseline", "", covariates)
  covariates <- covariates %>% as.factor() 
  
  location_names <- model_fit$stan_list$available_locations %>% as.factor()
  
  tidy_model <- 
    if(is_reported){
      model_fit$fit %>% recover_types(list(P=covariates, M=location_names)) %>% 
        spread_draws(mu[M], alpha[P], alpha1[P, M], y[M], ifr_noise[M], E_deaths[N2, M], 
                     prediction[N2, M], infection_overestimate[m])
    }else{
      model_fit$fit %>% recover_types(list(P=covariates, M=location_names)) %>% 
        spread_draws(mu[M], alpha[P], alpha1[P, M], y[M], ifr_noise[M], E_deaths[N2, M], 
                     prediction[N2, M])
    }
  
  real_deaths <- 
    enframe(model_fit$stan_list$deaths_by_location, "M", "deaths") %>% 
    group_by(M) %>% unnest(deaths) %>% mutate(N2=row_number()) %>% ungroup() 
  
  real_cases <- 
    enframe(model_fit$stan_list$reported_cases, "M", "cases") %>% 
    group_by(M) %>% unnest(cases) %>% mutate(N2=row_number()) %>% ungroup() 
  
  tidy_model <- tidy_model %>% left_join(real_deaths) %>% left_join(real_cases)
  
  tidy_model
}

plot_death_vs_estimate <- function(tidy_model, location_name, stan_model, N=NULL){
  
  if(missing(location_name)){
    stop("Missing location name")
  }
  
  
  location_df <- tidy_model %>% filter(M == !!location_name)
  deaths_df <- tidy_model %>% filter(M == !!location_name) %>% group_by(N2) %>% summarise(deaths=mean(deaths))
  
  if(!is.null(N)){
    location_df <- location_df %>% filter(N2 <= (N + 4))
    deaths_df <- deaths_df %>% filter(N2 <= (N + 4))
  }
  
  ggplot(location_df, aes(x=N2, y=E_deaths, group=N2)) + 
    geom_boxplot() + 
    geom_line(aes(y=deaths), deaths_df, group=1, alpha=0.7, size=1.2, color="red") + 
    theme_ggdist() + 
    ggtitle(location_name)
  
}

plot_cases_vs_estimate <- function(tidy_model, location_name, stan_model, N=NULL){
  
  if(missing(location_name)){
    stop("Missing location name")
  }
  
  
  location_df <- tidy_model %>% filter(M == !!location_name)
  cases_df <- tidy_model %>% filter(M == !!location_name) %>% group_by(N2) %>% summarise(cases=mean(cases))
  
  if(!is.null(N)){
    location_df <- location_df %>% filter(N2 <= (N + 4))
    cases_df <- cases_df %>% filter(N2 <= (N + 4))
  }
  
  ggplot(location_df, aes(x=N2, y=prediction, group=N2)) + 
    geom_boxplot() + 
    geom_line(aes(y=cases), cases_df, group=1, alpha=0.7, size=1.2, color="red") + 
    theme_ggdist() + 
    ggtitle(location_name)
  
}

plot_compare_estimate <- function(tidy_model_base, tidy_model_reported){
  df <- bind_rows(tidy_model_base %>% group_by(M, N2) %>% median_qi(estimate = mu) %>% mutate(model = "base"), 
                  tidy_model_reported %>% group_by(M, N2) %>% median_qi(estimate = mu) %>% mutate(model = "reported")) %>%
    to_broom_names()
  ggplot(df, aes(y = M, x = estimate, xmin = conf.low, xmax = conf.high, color = model)) + 
    geom_pointinterval(position = position_dodge(width = .3)) + theme_ggdist() + xlab("R0") + ylab("")
}


reconstruct_vars <- function(model_output, i, m, iteration){
  
  is_seeding <- iteration <= model_output$stan_list$stan_data$N0
  
  linear_effect <- 
    sum(-model_output$stan_list$stan_data$X[m,i,] * 
          (base_reported$out$alpha1[iteration,,m] + base_reported$out$alpha[iteration,]))
  
  list(linear_effect=linear_effect)
}

