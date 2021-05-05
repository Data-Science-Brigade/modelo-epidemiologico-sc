library(tidyverse)
library(tidybayes)

# Make sure to have a model_output model on the global environment
# ex: load("../../results/2021_04_19/2021_04_19_base_weekly_MAC+AGG-DEVELOP-3000-2400-4-0.950000-8_98584-stanfit.Rdata")


get_tidy_df <- function(model_fit){

  covariates <-
    model_fit$stan_list$common_interventions %>% select(-1) %>% colnames()
  covariates <- gsub("_percent_change_from_baseline", "", covariates)
  covariates <- covariates %>% as.factor() 
  
  location_names <- model_fit$stan_list$available_locations %>% as.factor()
  
  tidy_model <- 
    model_fit$fit %>% recover_types(list(P=covariates, M=location_names)) %>% 
    spread_draws(mu[M], alpha[P], alpha1[P, M], y[M], ifr_noise[M], E_deaths[N2, M], prediction[N2, M])
  
  real_deaths <- 
    enframe(model_fit$stan_list$deaths_by_location, "M", "deaths") %>% 
    group_by(M) %>% unnest(deaths) %>% mutate(N2=row_number()) %>% ungroup() 
  
  tidy_model <- tidy_model %>% left_join(real_deaths)
  
  tidy_model
}

plot_death_vs_estimate <- function(tidy_model, location_name){
  
  if(missing(location_name)){
    stop("Missing location name")
  }
  
  location_df <- tidy_model %>% filter(M == !!location_name)
  deaths_df <- tidy_model %>% filter(M == !!location_name) %>% group_by(N2) %>% summarise(deaths=mean(deaths))
  
  ggplot(location_df, aes(x=N2, y=E_deaths, group=N2)) + 
    geom_boxplot() + 
    geom_line(aes(y=deaths), deaths_df, group=1, alpha=0.7, size=1.2, color="red") + 
    theme_ggdist() + 
    ggtitle(location_name)
  
}


