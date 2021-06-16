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

plot_case_curve <- function(model_output, location_name){
  dfs <- get_dfs(location_name, model_output) 
  selected_dates <- model_output$stan_list$dates[[location_name]]
  ggplot(dfs$data_location %>% filter(time %in% selected_dates), aes(x=time+days(6))) + 
    geom_col(aes(y=reported_cases), fill='coral4', alpha=0.7) + 
    geom_ribbon(aes(ymin=predicted_min2, ymax=predicted_max2), linetype="dotted", alpha=0.65, fill="deepskyblue4") + 
    geom_ribbon(aes(ymin=predicted_min, ymax=predicted_max), linetype="dotted", alpha=0.45, fill="deepskyblue4") + 
    ggthemes::theme_clean() + 
    scale_x_date(date_breaks="2 weeks") + 
    theme(axis.text.x = element_text(angle=45, hjust=1)) + 
    xlab("Week starting on day") + ylab("Cases") + 
    ggtitle(sprintf("Case vs Estimated infections - %s", location_name))
}

plot_death_curve <- function(model_output, location_name){
  dfs <- get_dfs(location_name, model_output) 
  selected_dates <- model_output$stan_list$dates[[location_name]]
  ggplot(dfs$data_location %>% filter(time %in% selected_dates), aes(x=time+days(6))) + 
    geom_col(aes(y=deaths), fill='coral4', alpha=0.7) + 
    geom_ribbon(aes(ymin=death_min2, ymax=death_max2), linetype="dotted", alpha=0.65, fill="deepskyblue4") + 
    geom_ribbon(aes(ymin=death_min, ymax=death_max), linetype="dotted", alpha=0.45, fill="deepskyblue4") + 
    ggthemes::theme_clean() + 
    scale_x_date(date_breaks="2 weeks") + 
    theme(axis.text.x = element_text(angle=45, hjust=1)) + 
    xlab("Week starting on day") + ylab("Deaths") + 
    ggtitle(sprintf("Deaths vs Estimated deaths - %s", location_name))
}

plot_Rt_curve <- function(model_output, location_name){
  dfs <- get_dfs(location_name, model_output) 
  selected_dates <- model_output$stan_list$dates[[location_name]]
  ggplot(dfs$data_location %>% filter(time %in% selected_dates), aes(x=time+days(6))) + 
    geom_line(aes(y=rt), color='black', size=1.3) + 
    geom_ribbon(aes(ymin=rt_min2, ymax=rt_max2), linetype="dotted", alpha=0.75, fill="seagreen") + 
    geom_ribbon(aes(ymin=rt_min, ymax=rt_max), linetype="dotted", alpha=0.5, fill="seagreen") + 
    ggthemes::theme_clean() + 
    scale_x_date(date_breaks="2 weeks") + 
    theme(axis.text.x = element_text(angle=45, hjust=1)) + 
    xlab("Week starting on day") + ylab("Rt") + 
    ggtitle(sprintf("R(t) - %s", location_name))
}

get_all_dfs <- function(model_output){
  data_location <- list()
  data_location_forecast <- list()
  
  i <- 1
  for(location_name in model_output$stan_list$available_locations){
    dfs <- get_dfs(location_name, model_output)
    selected_dates <- model_output$stan_list$dates[[location_name]]
    
    data_location[[i]] <- 
      dfs$data_location %>% 
      filter(time %in% selected_dates) %>% 
      mutate(index=row_number(), cum_cases=cumsum(reported_cases), cum_deaths=cumsum(deaths))
    data_location_forecast[[i]] <- dfs$data_location_forecast
    i <- i + 1
  }
  
  dfs <- list(data_location=bind_rows(data_location),
              data_location_forecast=bind_rows(data_location_forecast))
  
  dfs
}

