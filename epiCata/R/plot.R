
##### DIAGNOSTICS ####
plot_mu <- function(model_output, auto_save=TRUE, save_path="./"){
  Sys.setlocale("LC_ALL","pt_BR.utf8")
  require(bayesplot)
  require(ggplot2)

  mu <- (as.matrix(model_output$out$mu))
  colnames(mu) <- model_output$stan_list$available_locations
  g <- (mcmc_intervals(mu,prob = .9))
  g <- g + theme_dsb_light() + ggtitle("Estimativas de R0 para cada localização")

  if(auto_save){
    ggsave(sprintf("%sfigures/%s/%s_mu.png", save_path, model_output$reference_date_str, model_output$filename_suffix), g,
           width=7, height=4, dpi=150)
  }

  g

}

plot_final_Rt <- function(model_output, auto_save=TRUE, save_path="./"){
  Sys.setlocale("LC_ALL","pt_BR.utf8")
  tmp <- lapply(1:length(model_output$stan_list$available_locations),
                function(i) (model_output$out$Rt_adj[,model_output$stan_list$stan_data$N[i],i]))
  Rt_adj <- do.call(cbind,tmp)
  colnames(Rt_adj) <- model_output$stan_list$available_locations
  g = (mcmc_intervals(Rt_adj,prob = .9))

  g <- g + theme_dsb_light() + ggtitle("Estimativas de Rt final para cada localização")

  if(auto_save){
    ggsave(sprintf("%sfigures/%s/%s_final_rt.png", save_path, model_output$reference_date_str, model_output$filename_suffix), g,
           width=7, height=4, dpi=150)
  }
  g
}

# GET DFS

get_dfs <- function(location_names, model_output, aggregate_name=NULL) {
  if(is.null(aggregate_name) && length(location_names)==1) {
    aggregate_name <- location_names[[1]]
  } else if(is.null(aggregate_name) && length(location_names)>1) {
    stop("You must either pass an aggregate name or pass a single location")
  }
  dfs <- if(length(location_names)>1){
    get_merged_forecast_dfs(location_names, model_output, aggregate_name=aggregate_name)
  } else {
    get_merged_forecast_dfs(location_names, model_output, aggregate_name=aggregate_name)
  }
  dfs
}

#### 3-PANEL ####

make_all_three_panel_plot <- function(model_output,aggregate_name=NULL, save_path="./"){
  require(epiCataPlot)
  available_locations <- model_output$stan_list$available_locations

  for(location_name in available_locations){
    make_three_panel_plot(location_name,
                          get_dfs(location_name, model_output),
                          model_output$reference_date_str,
                          auto_save=TRUE, save_path=save_path,
                          filename_suffix=model_output$filename_suffix)
  }
  if(!is.null(aggregate_name)){
    make_three_panel_plot(aggregate_name,
                          get_dfs(available_locations, model_output, aggregate_name),
                          model_output$reference_date_str,
                          auto_save=TRUE,
                          save_path=save_path,
                          filename_suffix=model_output$filename_suffix)
  }
}

#### FORECAST ####

make_all_forecast_plots <- function(model_output, aggregate_name=NULL, min_y_breaks=NULL, max_y_breaks=NULL, week_max_y_breaks=NULL, save_path="./"){
  require(epiCataPlot)
  available_locations <- model_output$stan_list$available_locations

  for(location_name in available_locations){
    min_y_break <- if(!is.null(min_y_breaks) && length(min_y_breaks)>1){
      min_y_breaks[[location_name]]
    } else {
      min_y_breaks
    }
    max_y_break <- if(!is.null(max_y_breaks) && length(max_y_breaks)>1){
      max_y_breaks[[location_name]]
    } else {
      max_y_breaks
    }
    week_max_y_break <- if(!is.null(week_max_y_breaks) && length(week_max_y_breaks)>1){
      week_max_y_breaks[[location_name]]
    } else {
      week_max_y_breaks
    }
    dfs <- get_dfs(location_name, model_output)
    make_forecast_plot(location_name,
                       dfs,
                       model_output$reference_date_str,
                       is_weekly=FALSE,
                       min_y_break=min_y_break,
                       max_y_break=max_y_break,
                       auto_save=TRUE,
                       save_path=save_path,
                       filename_suffix=model_output$filename_suffix)
    make_forecast_plot(location_name,
                       dfs,
                       model_output$reference_date_str,
                       is_weekly=TRUE,
                       min_y_break=min_y_break,
                       max_y_break=week_max_y_break,
                       auto_save=TRUE,
                       save_path=save_path,
                       filename_suffix=model_output$filename_suffix)
  }

  if(!is.null(aggregate_name)){
    min_y_break <- if(!is.null(min_y_breaks) && length(min_y_breaks)>1){
      min_y_breaks[[aggregate_name]]
    } else {
      min_y_breaks
    }
    max_y_break <- if(!is.null(max_y_breaks) && length(max_y_breaks)>1){
      max_y_breaks[[aggregate_name]]
    } else {
      max_y_breaks
    }
    week_max_y_break <- if(!is.null(week_max_y_breaks) && length(week_max_y_breaks)>1){
      week_max_y_breaks[[aggregate_name]]
    } else {
      week_max_y_breaks
    }

    dfs <- get_dfs(available_locations, model_output, aggregate_name)
    make_forecast_plot(location_name,
                       dfs,
                       model_output$reference_date_str,
                       is_weekly=FALSE,
                       min_y_break=min_y_break,
                       max_y_break=max_y_break,
                       auto_save=TRUE,
                       save_path=save_path,
                       filename_suffix=model_output$filename_suffix)
    make_forecast_plot(location_name,
                       dfs,
                       model_output$reference_date_str,
                       is_weekly=TRUE,
                       min_y_break=min_y_break,
                       max_y_break=week_max_y_break,
                       auto_save=TRUE,
                       save_path=save_path,
                       filename_suffix=model_output$filename_suffix)
  }
}

### Recent C plots ###

make_all_C_plot <- function(model_output, min_x_break=NULL, aggregate_name=NULL, save_path="./"){
  require(epiCataPlot)
  available_locations <- model_output$stan_list$available_locations

  for(location_name in available_locations){
    make_C_plot(location_name, model_output, min_x_break=min_x_break, auto_save=TRUE, save_path=save_path)
  }
  if(!is.null(aggregate_name)){
    make_C_plot(available_locations, model_output, min_x_break=min_x_break, auto_save=TRUE, aggregate_name = aggregate_name, save_path=save_path)
  }
}

make_C_plot <- function(location_names, model_output, auto_save=TRUE, min_x_break=NULL, aggregate_name=NULL, save_path="./"){
  Sys.setlocale("LC_ALL","pt_BR.utf8")
  require(epiCataPlot)
  require(tidyverse)
  require(ggplot2)
  require(scales)
  require(lubridate)

  if(is.null(aggregate_name) && length(location_names)==1) {
    aggregate_name <- location_names[[1]]
  } else if(is.null(aggregate_name) && length(location_names)>1) {
    stop("You must either pass an aggregate name or pass a single location")
  }

  cat(sprintf("\n> Making 3-plots panel for %s", aggregate_name))

  reference_date_str <- model_output$reference_date_str
  dfs <- if(length(location_names)>1){
    get_merged_forecast_dfs(location_names, model_output, aggregate_name=aggregate_name)
  } else {
    get_merged_forecast_dfs(location_names, model_output, aggregate_name=aggregate_name)
  }

  #### CONFIGURE X BREAKS ####
  if(is.null(min_x_break)){
    x_min_date <- if(max(dfs$data_location$reported_cases_c)>=50) {
      min(filter(dfs$data_location, predicted_cases_c>0)$time) - 7
    } else {
      min(dfs$data_location$time)
    }
    x_max_date <- max(dfs$data_location$time)

    rest <- as.integer(x_max_date - x_min_date) %% 7
    x_min_date <- x_min_date - days(7 - rest)
  } else {
    x_max_date <- max(dfs$data_location$time)
    x_min_date <- min_x_break
  }

  x_breaks <- seq(x_min_date, x_max_date, by="week")

  if(length(x_breaks) > 11){
    x_breaks <- seq(x_min_date, x_max_date, by="2 weeks")
    if(x_breaks[length(x_breaks)] != x_max_date){
      x_breaks <- seq(x_min_date + days(7), x_max_date, by="2 weeks")
    }
  }

  plot_C <- plot_graph_C(aggregate_name, model_output, x_breaks, dfs)

  if(auto_save){
    plot_C_filename <- sprintf("%sfigures/%s/GRAFICO_RECENTE_C_%s_%s.png", save_path, reference_date_str, aggregate_name, model_output$filename_suffix)
    cat(sprintf("\n   Saving %s", plot_C_filename))
    ggsave(file=plot_C_filename, plot_C, width = 9, height=4, type="cairo")
  }

  plot_C
}

plot_state_forecast <- function(model_output, x_min = "2020-05-31", x_max=NULL, y_breaks=NULL) {
  Sys.setlocale("LC_ALL","pt_BR.utf8")
  library(epiCata)
  require(epiCataPlot)
  require(tidyverse)
  require(ggrepel)
  require(ggplot2)
  require(scales)
  require(lubridate)

  # Load latest model
  location_name <- "SC_ESTADO"

  reference_date_str <- model_output$reference_date_str
  reference_date = ymd(reference_date_str)

  dfs <- get_merged_forecast_dfs(model_output$stan_list$available_locations, model_output, aggregate_name = "SC_ESTADO")
  cum_deaths <- get_cumulative_df(data_location=dfs$data_location,
                                  dfs$data_location_forecast,
                                  reference_date = ymd(reference_date_str))

  followup <- cum_deaths %>%
    filter(time <= ymd(reference_date)- days(1)) %>%
    select(time, `Cenario 1`) %>% rename(`Obitos confirmados`=`Cenario 1`) %>%
    gather("key" = key, "value" = value, -time) %>% drop_na()

  cumulative_deaths <- cum_deaths %>%
    gather("key" = key, "value" = value, -time) %>%
    filter(time >= ymd(reference_date) - days(1)) %>% drop_na()

  cumulative_deaths <- bind_rows(cumulative_deaths, followup)
  fill_labels <- c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados")

  cumulative_deaths$key <-
    factor(cumulative_deaths$key,
           levels=c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados"),
           labels=fill_labels)

  color_vals <- c("#FFA600", "#FF6F00", "#FF0000", "#7300FF")
  names(color_vals) <- levels(cumulative_deaths$key)

  x_breaks_by <- "weeks"

  x_min = ymd(x_min)
  if(is.null(x_max)){
    x_max <- reference_date + 4*7
  }
  x_breaks <- seq(x_min, x_max,by=x_breaks_by)

  cumulative_deaths <- cumulative_deaths %>% filter(time >= x_min)

  p <- ggplot(mapping=aes(x=time, y=value, label=value, color=key, group=key)) +
    geom_line(data=cumulative_deaths %>% filter(key != "Obitos confirmados"),
              size=0.5, alpha=0.5, linetype="dashed") +
    geom_line(data=cumulative_deaths %>% filter(key == "Obitos confirmados"),
              size=1, linetype="solid", color="#7300FF") +
    geom_point(data=cumulative_deaths %>% filter(time %in% x_breaks), alpha=1, size=2) +
    geom_label_repel(data=cumulative_deaths %>% filter(time == x_max),
                     size=4, show.legend = FALSE) +
    geom_label(data=cumulative_deaths %>% filter(time == ymd(reference_date) - days(1)),
               size=4, show.legend = FALSE, fill="#7300FF", color="#FFFFFF")

  if(is.null(y_breaks)){
    # Workaround to try to get better looking Y axis legends
    min_y_break <- min(cumulative_deaths$value)
    n_integer_digits <- floor(log10(min_y_break)) + 1
    min_y_break <- floor(min_y_break/10^(n_integer_digits - 1)) * 10^(n_integer_digits - 1)
    max_y_break <- max(cumulative_deaths$value)
    max_y_break <- round_y_breaks(max_y_break, min_y_break=min_y_break)

    y_breaks <- seq(min_y_break, max_y_break, (max_y_break-min_y_break)/4)
  }

  pp <- p +
    geom_label(data=cumulative_deaths %>% filter(key == "Cenário Real", time >= ymd(reference_date)),
               label.padding = unit(0.2, "lines"), size=4) +
    xlab("Data") +
    ylab("Número TOTAL de óbitos\n") +
    scale_x_date(labels = date_format("%e %b"),
                 breaks=x_breaks,
                 limits=c(x_min - days(1), x_max)) +
    scale_y_continuous(limits=c(min(y_breaks), max(y_breaks)), breaks=y_breaks) +
    theme_dsb_light() +
    theme(panel.grid.major.x = element_line(linetype = "dotted", color = "grey", size=0.4),
          legend.text = element_text(margin = margin(l=2.5, t=0)),
          legend.title = element_text(margin=margin(t=0))) +
    scale_color_manual(name="Cenarios", values=color_vals) +
    scale_fill_manual(name="Cenarios", values=color_vals)

  plot_filename <- sprintf("figures/%s/FOLLOWUP_%s_%s.png",
                           reference_date_str,
                           location_name,
                           model_output$filename_suffix, ".png")
  cat(sprintf("\n   Saving %s", plot_filename))
  ggsave(file= plot_filename, pp, width = 11.08, height=4.8, type="cairo")
  pp
}
