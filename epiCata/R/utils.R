get_forecast_dfs <- function(location_name, model_output, forecast=30){
  require(lubridate)

  i <- which(model_output$stan_list$available_locations == location_name)

  N <- model_output$stan_list$stan_data$N[i]
  N2 <- N + forecast


  #### COMPUTE PREDICTIONS ####
  prediction <- model_output$out$prediction

  predicted_cases <- colMeans(prediction[,1:N,i])
  predicted_cases_li <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.025)
  predicted_cases_ui <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.975)
  predicted_cases_li2 <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.25)
  predicted_cases_ui2 <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.75)

  #### COMPUTE ESTIMATED DEATHS ####
  estimated.deaths <- model_output$out$E_deaths
  estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
  estimated_deaths_li <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.025)
  estimated_deaths_ui <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.975)
  estimated_deaths_li2 <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.25)
  estimated_deaths_ui2 <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.75)


  #### COMPUTE FORECAST OF ESTIMATED DEATHS ####
  estimated_deaths_forecast <- colMeans(estimated.deaths[,1:N2,i])[N:N2]
  estimated_deaths_li_forecast <- matrixStats::colQuantiles(estimated.deaths[,1:N2,i], probs=.025)[N:N2]
  estimated_deaths_ui_forecast <- matrixStats::colQuantiles(estimated.deaths[,1:N2,i], probs=.975)[N:N2]
  estimated_deaths_li2_forecast <- matrixStats::colQuantiles(estimated.deaths[,1:N2,i], probs=.25)[N:N2]
  estimated_deaths_ui2_forecast <- matrixStats::colQuantiles(estimated.deaths[,1:N2,i], probs=.75)[N:N2]

  #### COMPUTE RT ####
  rt <- colMeans(model_output$out$Rt[,1:N,i])
  rt_li <- matrixStats::colQuantiles(model_output$out$Rt[,1:N,i],probs=.025)
  rt_ui <- matrixStats::colQuantiles(model_output$out$Rt[,1:N,i],probs=.975)
  rt_li2 <- matrixStats::colQuantiles(model_output$out$Rt[,1:N,i],probs=.25)
  rt_ui2 <- matrixStats::colQuantiles(model_output$out$Rt[,1:N,i],probs=.75)

  data_location <- data.frame("time" = as_date(as.character(model_output$stan_list$dates[[i]])),
                             "location_name" = rep(location_name, length(model_output$stan_list$dates[[i]])),
                             "reported_cases" = model_output$stan_list$reported_cases[[i]],
                             "reported_cases_c" = cumsum(model_output$stan_list$reported_cases[[i]]),
                             "predicted_cases_c" = cumsum(predicted_cases),
                             "predicted_min_c" = cumsum(predicted_cases_li),
                             "predicted_max_c" = cumsum(predicted_cases_ui),
                             "predicted_cases" = predicted_cases,
                             "predicted_min" = predicted_cases_li,
                             "predicted_max" = predicted_cases_ui,
                             "predicted_min2" = predicted_cases_li2,
                             "predicted_max2" = predicted_cases_ui2,
                             "deaths" = model_output$stan_list$deaths_by_location[[i]],
                             "deaths_c" = cumsum(model_output$stan_list$deaths_by_location[[i]]),
                             "estimated_deaths_c" =  cumsum(estimated_deaths),
                             "death_min_c" = cumsum(estimated_deaths_li),
                             "death_max_c"= cumsum(estimated_deaths_ui),
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

  times <- as_date(as.character(model_output$stan_list$dates[[i]]))
  times_forecast <- times[length(times)] + 0:forecast
  data_location_forecast <- data.frame("time" = times_forecast,
                                      "location_name" = rep(location_name, forecast + 1),
                                      "estimated_deaths_forecast" = estimated_deaths_forecast,
                                      "death_min_forecast" = estimated_deaths_li_forecast,
                                      "death_max_forecast"= estimated_deaths_ui_forecast)

  list(data_location=data_location, data_location_forecast=data_location_forecast)
}
