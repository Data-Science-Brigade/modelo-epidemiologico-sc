
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

#### 3-PANEL ####

make_all_three_panel_plot <- function(model_output,aggregate_name=NULL, save_path="./"){
  available_locations <- model_output$stan_list$available_locations

  for(location_name in available_locations){
    make_three_panel_plot(location_name, model_output, auto_save=TRUE, save_path=save_path)
  }
  if(!is.null(aggregate_name)){
    make_three_panel_plot(available_locations, model_output, auto_save=TRUE, aggregate_name = aggregate_name, save_path=save_path)
  }
}


make_three_panel_plot <- function(location_names, model_output, auto_save=TRUE, min_x_break=NULL, aggregate_name=NULL, save_path="./"){
  Sys.setlocale("LC_ALL","pt_BR.utf8")
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
  x_min_date <- if(max(dfs$data_location$reported_cases_c)>=50) {
    min(filter(dfs$data_location, predicted_cases_c>0)$time) - 7
  } else {
    min(dfs$data_location$time)
  }
  x_max_date <- max(dfs$data_location$time)

  rest <- as.integer(x_max_date - x_min_date) %% 7
  x_min_date <- x_min_date - days(7 - rest)

  x_breaks <- seq(x_min_date, x_max_date, by="week")

  if(length(x_breaks) > 17){
    x_breaks <- seq(x_min_date, x_max_date, by="4 weeks")
    if(x_breaks[length(x_breaks)] != x_max_date){
      x_breaks <- seq(x_min_date + days(7), x_max_date, by="4 weeks")
    }
  }else if(length(x_breaks) > 11){
    x_breaks <- seq(x_min_date, x_max_date, by="2 weeks")
    if(x_breaks[length(x_breaks)] != x_max_date){
      x_breaks <- seq(x_min_date + days(7), x_max_date, by="2 weeks")
    }
  }

  if(!is.null(min_x_break)){
    valid_x_breaks <- sapply(x_breaks, function(x_break){x_break >= ymd(min_x_break)})
    x_breaks <- x_breaks[valid_x_breaks]
  }

  #### PLOTS ####

  plot_A <- plot_graph_A(aggregate_name, x_breaks, dfs)

  if(auto_save){
    plot_A_filename <- sprintf("%sfigures/%s/GRAFICO_A_%s_%s.png", save_path, reference_date_str, aggregate_name, model_output$filename_suffix)
    cat(sprintf("\n   Saving %s", plot_A_filename))
    ggsave(file=plot_A_filename, plot_A, width = 6, height=4, type="cairo")
  }

  plot_B <- plot_graph_B(aggregate_name, x_breaks, dfs)

  if(auto_save){
    plot_B_filename <- sprintf("%sfigures/%s/GRAFICO_B_%s_%s.png", save_path, reference_date_str, aggregate_name, model_output$filename_suffix)
    cat(sprintf("\n   Saving %s", plot_B_filename))
    ggsave(file=plot_B_filename, plot_B, width = 6, height=4, type="cairo")
  }

  plot_C <- plot_graph_C(aggregate_name, model_output, x_breaks, dfs)

  if(auto_save){
    plot_C_filename <- sprintf("%sfigures/%s/GRAFICO_C_%s_%s.png", save_path, reference_date_str, aggregate_name, model_output$filename_suffix)
    cat(sprintf("\n   Saving %s", plot_C_filename))
    ggsave(file=plot_C_filename, plot_C, width = 9, height=4, type="cairo")
  }

  p <- cowplot::plot_grid(plot_A, plot_B, plot_C, ncol = 3, rel_widths = c(1.5, 1, 2))

	if(auto_save){
		plot_filename <- sprintf("%sfigures/%s/3_PANEL_%s_%s.png", save_path, reference_date_str, aggregate_name, model_output$filename_suffix)
		cat(sprintf("\n   Saving %s", plot_filename))
		cowplot::save_plot(plot_filename, p, base_width=14)
	}

	p
}

plot_graph_A <- function(location_name, x_breaks, dfs){
  require(tidyverse)
  require(ggplot2)
  require(scales)
  require(lubridate)

  #### CREATE DATAFRAMES FOR 95% INTERVAL AND 50% INTERVAL ####
  data_cases_95 <- data.frame(dfs$data_location$time,
                              dfs$data_location$predicted_min,
                              dfs$data_location$predicted_max)
  names(data_cases_95) <- c("time", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))

  data_cases_50 <- data.frame(dfs$data_location$time,
                              dfs$data_location$predicted_min2,
                              dfs$data_location$predicted_max2)
  names(data_cases_50) <- c("time", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$time))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")

  cases_legend <- data.frame(text=c("Confirmados: ", "Estimado (min): ", "Estimado (max): "),
                             x=max(x_breaks),
                             y=c(max(dfs$data_location$reported_cases),
                                 round(max(data_cases$cases_min)),
                                 round(max(data_cases$cases_max))))
  final_values <- data.frame(x=max(x_breaks),
                             y=c((data_cases %>% slice(n()) %>% summarise(y=mean(c(cases_min, cases_max))))$y,
                                 (data_cases_95 %>% slice(n()))$cases_max))
  final_values$y <- round(final_values$y)

  max_y_break <- max(c(max(data_cases$cases_max),round(max(dfs$data_location$reported_cases))))
  max_y_break <- round_y_breaks(max_y_break)

  original_y_breaks <- seq(0, max_y_break, max_y_break/4)

  valid_new_break <- sapply(final_values$y, function(y){all(abs(y - original_y_breaks) >= max_y_break/4/10)})
  y_breaks <- c(original_y_breaks, final_values$y[valid_new_break])

  bold_face <- sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){"plain"}else{"italic"}})

  plot_A <- ggplot(dfs$data_location) +
    geom_bar(aes(x = time, y = reported_cases),
             color = "coral4", fill= "coral4", stat='identity', size=1.2) +
    geom_ribbon(data = data_cases,
                aes(x = time, ymin = cases_min, ymax = cases_max, fill = key)) +
    geom_hline(yintercept = final_values$y, linetype="dashed", size=0.06) +
    scale_x_date(name="", labels = date_format("%e %b"), breaks=x_breaks, limits=c(min(x_breaks), max(x_breaks))) +
    scale_y_continuous(name="Numero de infecções diárias",
                       breaks=y_breaks, limits=c(0, max_y_break),
                       labels=label_comma(big.mark=".", decimal.mark=",", accuracy=1)) +
    scale_fill_manual(name = "Faixa de erro", labels = c("Vlr. Medio", "Variacao"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    theme_dsb_light() +
    theme(legend.position = "None",
          plot.title = element_text(size=7.7, face="plain"),
          axis.text.y = element_text(face=bold_face)) +
    ggtitle("Numero de infecções diárias. Confirmado (vermelho) vs Estimado (Azul)") +
    guides(fill=guide_legend(ncol=1))

  plot_A

}

plot_graph_B <- function(location_name, x_breaks, dfs, is_cumulative=FALSE){
  require(tidyverse)
  require(ggplot2)
  require(scales)
  require(lubridate)

  #### CREATE DATAFRAMES FOR 95% INTERVAL AND 50% INTERVAL ####
  data_deaths_95 <- data.frame(dfs$data_location$time,
                               dfs$data_location$death_min,
                               dfs$data_location$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))

  data_deaths_50 <- data.frame(dfs$data_location$time, dfs$data_location$death_min2,
                               dfs$data_location$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))

  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")

  if(is_cumulative) {
    data_deaths <- data_deaths %>%
      filter(key == "fifty") %>% mutate(death_min=cumsum(death_min), death_max=cumsum(death_max))
    data_location <- dfs$data_location %>% mutate(deaths_cum=cumsum(deaths))
  } else {
    data_deaths <- data_deaths %>%
      filter(key == "fifty") %>% mutate(death_min=death_min, death_max=death_max)
    data_location <- dfs$data_location %>% mutate(deaths_cum=deaths)
  }

  total_deaths <- max(data_location$deaths_cum)

  plot_B <-   ggplot(data_location, aes(x = time)) +
    geom_bar(data = data_location, aes(y = deaths_cum, fill = "reported"),
             fill = "coral4", stat='identity', alpha=1) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(labels = date_format("%e %b"),
                 breaks=x_breaks,
                 limits=c(min(x_breaks), max(x_breaks))) +
    geom_hline(yintercept = max(data_location$deaths_cum), linetype="dashed", size=0.07) +
    ylab(if(is_cumulative) { "Numero TOTAL de óbitos" } else { "Numero DIÁRIO de óbitos" }) + xlab("Data") +
    scale_fill_manual(name = "", labels = c("Vlr. Medio", "Variacao"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    theme_dsb_light() +
    theme(legend.position = "None")  +
    guides(fill=guide_legend(ncol=1))

  if(location_name == "SC_ESTADO"){
    plot_B <- plot_B +
      annotate("text", label=sprintf("Total óbitos em %s: %d", strftime(ymd(max(x_breaks)), "%d/%m/%Y"), total_deaths),
                                    x=min(x_breaks) + days(as.integer(ceiling(max(x_breaks) - min(x_breaks))/2.6)),
                                    y=total_deaths + (max(data_deaths$death_max) - total_deaths)/4, size=2.5)
  }

  # scale_y_continuous(breaks=seq(0, 90, 10))
  plot_B
}

plot_graph_C <- function(location_name, model_output, x_breaks, dfs){
  require(tidyverse)
  require(ggplot2)
  require(scales)
  require(lubridate)

  #### CREATE DATAFRAMES FOR 95% INTERVAL AND 50% INTERVAL ####
  data_rt_95 <- data.frame(dfs$data_location$time,
                           dfs$data_location$rt_min, dfs$data_location$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))

  data_rt_50 <- data.frame(dfs$data_location$time, dfs$data_location$rt_min2,
                           dfs$data_location$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))

  x_min_date <- min(x_breaks)
  x_max_date <- max(x_breaks)

  data_rt <- rbind(data_rt_95, data_rt_50) %>% filter(x_min_date<=time, time<=x_max_date)
  levels(data_rt$key) <- c("ninetyfive", "fifth")

  max_rt <- ceiling(max(data_rt$rt_max))
  ##### CHANGE POINTS ####
  n_features <- model_output$stan_list$interventions_wide$AREA %>% n_distinct()
  if(n_features <= 6){

    changepoints_df <-
      model_output$stan_list$interventions_wide %>% group_by(AREA) %>%
      mutate(lag_combination = lag(ADERENCIA,  default = 1, order_by = AREA),
             changes=cumsum(ADERENCIA != lag_combination),
             has_change=changes > lag(changes, default=0, order_by=AREA)) %>%
      filter(row_number() == 1 | has_change) %>% select(-c(lag_combination, changes, has_change))
    changepoints_df$x <- rep(NA, nrow(changepoints_df))
    unique_dates <- unique(changepoints_df$DATA)

    for (k in 1:length(unique_dates)){
      idxs <- which(changepoints_df$DATA == unique_dates[k])
      max_val <- max(data_rt$rt_max) + 0.3
      for (j in idxs){
        changepoints_df$x[j] <- max_val
        max_val <- max_val - 0.3
      }
    }
  }

  original_y_breaks <- seq(1, ceiling(max_rt))

  priority_rt <- round(data_rt %>%
                         filter(time == max(time)) %>%
                         summarize(rt=c(min(rt_min), max(rt_max), mean(c(rt_min, rt_max)))), 2)$rt

  # Did this location had a higher R0?
  R0_df <- bind_rows(data_rt_50 %>% filter(time == min(time), rt_min >= 2))
  if(nrow(R0_df) > 0){
    priority_rt <- c(priority_rt, round(mean(c(R0_df$rt_min, R0_df$rt_max)), 2))
  }

  valid_original_break <- sapply(original_y_breaks, function(y){all(abs(y - priority_rt) > 1/9)})

  y_breaks <- c(original_y_breaks[valid_original_break], priority_rt)

  custom_font_face <- sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){"plain"}else{"bold"}})
  custom_linetype <- sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){"solid"}else{"dashed"}})
  custom_linesize <- sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){0.1}else{0.3}})
  custom_color <- sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){"#bbbbbb"}else{"#212121"}})

  # Are priority_rt properly separated?
  priority_separation <- all(sapply(priority_rt, function(y){sum(abs(y - priority_rt) > 1/9) > 1}))
  custom_font_size <-
    if(priority_separation){
      sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){10}else{9}})
    }else if(max(y_breaks)<=2){
      sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){10}else{9}})
    }else{
      sapply(y_breaks, function(y_break){if(y_break %in% original_y_breaks){10}else{7}})
    }

  plot_C <- ggplot(dfs$data_location) +
    geom_stepribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max,
                                        group = key,
                                        fill = key)) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "Faixa de Erro", labels = c("Vlr. Medio", "Variacao"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) +
    scale_x_date(labels = date_format("%e %b"),
                 breaks=x_breaks,
                 limits=c(min(x_breaks), max(x_breaks))) +
    scale_y_continuous(breaks=y_breaks, labels=sprintf("%.2f", y_breaks), limits=c(0, max_rt)) +
    theme_dsb_light() +
    theme(
      axis.text.y = element_text(face=custom_font_face, size=custom_font_size),
      panel.grid.major.y = element_line(size=custom_linesize, linetype=custom_linetype, color=custom_color),
      legend.title=element_text(size=8, margin=margin(t=1, b=1, l=1, r=1)),
      legend.text=element_text(size=6, face="bold", margin=margin(t=1, b=1, l=1, r=1)),
      legend.position = "right",
      legend.spacing=unit(0, "points")
    )

  plot_C <- if(n_features <= 6){
    plot_C +
      geom_segment(data = changepoints_df,
                   aes(x = DATA, y = 0, xend = DATA, yend=max(x)),
                   linetype = "dashed", colour = "grey", alpha = 0.75, size=0.3) +
        geom_point(data = changepoints_df, aes(x = DATA,
                                               y = x,
                                               group = AREA,
                                               shape = AREA,
                                               col = AREA), size = 2) +
        scale_shape_manual(name = "Restrições e Liberações",
                           labels = unique(changepoints_df$AREA),
                           values = c(21, 22, 23, 24, 25, 12)) +
        scale_colour_discrete(name = "Restrições e Liberações", labels = unique(changepoints_df$AREA))
  }else{
    plot_C
  }

  plot_C

}

#### FORECAST ####

make_all_forecast_plots <- function(model_output, aggregate_name=NULL, min_y_breaks=NULL, max_y_breaks=NULL, week_max_y_breaks=NULL, save_path="./"){
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
    make_forecast_plot(location_name, model_output, auto_save=TRUE, min_y_break=min_y_break, max_y_break=max_y_break, week_max_y_break=week_max_y_break ,save_path=save_path)
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
    make_forecast_plot(available_locations, model_output, auto_save=TRUE, min_y_break=min_y_break, max_y_break=max_y_break, week_max_y_break=week_max_y_break, aggregate_name = aggregate_name, save_path=save_path)
  }
}

make_forecast_plot <- function(location_names, model_output, auto_save=TRUE, min_y_break=NULL, max_y_break=NULL, week_max_y_break=NULL, aggregate_name=NULL, save_path="./"){
  Sys.setlocale("LC_ALL","pt_BR.utf8")
  require(tidyverse)
  require(ggrepel)
  require(ggplot2)
  require(scales)
  require(lubridate)

  if(missing(aggregate_name) && length(location_names)==1) {
    aggregate_name <- location_names[[1]]
  } else if(is.null(aggregate_name) && length(location_names)>1) {
    stop("You must either pass an aggregate name or pass a single location")
  }

  cat(sprintf("\n> Making forecast plots for %s", aggregate_name))

  reference_date_str <- model_output$reference_date_str
  dfs <- get_merged_forecast_dfs(location_names, model_output, aggregate_name=aggregate_name)

  df_rts <- dfs$data_location %>% filter(row_number() == n()) %>% select(rt_min, rt, rt_max)

  cum_deaths <- get_cumulative_df(data_location=dfs$data_location,
                                  dfs$data_location_forecast,
                                  reference_date = ymd(reference_date_str))

  for(next_week in c(TRUE, FALSE)){
    p <- make_cumulative_plot(aggregate_name, cum_deaths, df_rts, ymd(reference_date_str), next_week=next_week,
                              min_y_break=min_y_break,
                              max_y_break=if(next_week && !is.null(week_max_y_break)){week_max_y_break}else{max_y_break})
    p <- p + ggtitle(paste0("(", aggregate_name, ") Cenarios do Modelo do dia ", strftime(ymd(reference_date_str), "%d/%m/%Y")))

    if(auto_save){
      plot_filename <- sprintf("%sfigures/%s/FORECAST_%s_%s_%s.png",
                               save_path,
                               reference_date_str,
                               aggregate_name,
                               ifelse(next_week, "_week", ""),
                               model_output$filename_suffix, ".png")
      cat(sprintf("\n   Saving %s", plot_filename))
      ggsave(file= plot_filename, p, width = 11.08, height=4.8, type="cairo")
    }
  }

  p
}

make_cumulative_plot <- function(location_name, cumulative_deaths, df_rts=NULL,
                                 reference_date, max_date=NULL,
                                 next_week=FALSE,
                                 min_y_break=NULL,
                                 max_y_break=NULL){
  require(tidyverse)
  require(ggplot2)
  require(scales)
  require(lubridate)

  cumulative_deaths <- cumulative_deaths %>%
    gather("key" = key, "value" = value, -time) %>%
    filter(time >= ymd(reference_date) - days(1)) %>% drop_na()

  if(is.null(df_rts)){
    fill_labels <- c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados")
  }else{
    fill_labels <- c(sprintf("Cenario 1 (Rt = %.2f)", df_rts$rt_min),
                     sprintf("Cenario 2 (Rt = %.2f)", df_rts$rt),
                     sprintf("Cenario 3 (Rt = %.2f)", df_rts$rt_max), "Obitos confirmados")
  }

  cumulative_deaths$key <-
    factor(cumulative_deaths$key,
           levels=c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados"),
           labels=fill_labels)

  color_vals <- c("#FFA600", "#FF6F00", "#FF0000", "#7300FF")
  names(color_vals) <- levels(cumulative_deaths$key)

  x_breaks_by <- ifelse(next_week, "days", "weeks")

  x_min <- ymd(reference_date) - days(1)

  x_max <-
    if(next_week){
      ymd(reference_date) + days(6)
    }else{
      max(cumulative_deaths$time)
    }

  x_breaks <- seq(x_min, x_max,by=x_breaks_by)
  cumulative_deaths <- cumulative_deaths %>% filter(time >= ymd(reference_date) - days(1))

  if(next_week){
    cumulative_deaths <- cumulative_deaths %>% filter(time <= x_max)
  }

  p <- ggplot(mapping=aes(x=time, y=value, label=value, color=key, group=key)) +
    geom_line(data=cumulative_deaths %>% filter(key != "Obitos confirmados"),
              size=0.5, alpha=0.5, linetype="dashed")

  p <- p +
    geom_point(data=cumulative_deaths %>% filter(key != "Obitos confirmados", time %in% x_breaks), alpha=1, size=2)

  p <- p + geom_label(data=cumulative_deaths %>% filter(time == ymd(reference_date) - days(1)),
                      color='#FCFCFC', fill='#C8255f', size=4, show.legend = FALSE)

  if(is.null(min_y_break)){
    min_y_break <- min(cumulative_deaths$value)
    n_integer_digits <- floor(log10(min_y_break)) + 1
    min_y_break <- floor(min_y_break/10^(n_integer_digits - 1)) * 10^(n_integer_digits - 1)
  }

  if(is.null(max_y_break)){
    max_y_break <- max(cumulative_deaths$value)
    max_y_break <- round_y_breaks(max_y_break, min_y_break=min_y_break)
  }

  y_separation <- floor(max_y_break - min_y_break)

  #y_breaks <- seq(min_y_break, max_y_break + (y_separation %% 4), floor(y_separation/4))
  y_breaks <- seq(min_y_break, max_y_break, floor(y_separation/4))
  print(y_breaks)
  print(y_separation)

  p <- p +
    geom_label_repel(data=cumulative_deaths %>% filter(time >= ymd(reference_date), time %in% x_breaks),
                     aes(fill=as.character(key)),
                     label.padding = unit(0.2, "lines"), color='#FCFCFC', size=4, alpha=1, show.legend=FALSE) +
    geom_label(data=cumulative_deaths %>% filter(key == "Cenário Real", time >= ymd(reference_date)),
                     label.padding = unit(0.2, "lines"), size=4) +
    xlab("Data") +
    ylab("Número TOTAL de óbitos\n") +
    scale_x_date(labels = date_format("%e %b"),
                 breaks=x_breaks,
                 limits=c(min(cumulative_deaths$time), max(cumulative_deaths$time))) +
    scale_y_continuous(limits=c(min(y_breaks), max(y_breaks)), breaks=y_breaks) +
    theme_dsb_light() +
    theme(panel.grid.major.x = element_line(linetype = "dotted", color = "grey", size=0.4)) +
    scale_color_manual(name="Cenarios", values=color_vals) +
    scale_fill_manual(name="Cenarios", values=color_vals)

  p
}

get_cumulative_df <- function(data_location, data_location_forecast, reference_date){

  #### Cumulative ####
  cumulative_deaths_min <-
    bind_rows(data_location %>% select(time, deaths) %>% filter(time < ymd(reference_date)),
              data_location_forecast %>% filter(time >= ymd(reference_date)) %>%
                rename(deaths=death_min_forecast) %>% select(time, deaths)) %>%
    mutate(`Cenario 1`=round(cumsum(deaths))) %>% select(-deaths)

  cumulative_deaths_medium <-
    bind_rows(data_location %>% select(time, deaths) %>% filter(time < ymd(reference_date)),
              data_location_forecast %>% filter(time >= ymd(reference_date)) %>%
                rename(deaths=estimated_deaths_forecast) %>% select(time, deaths)) %>%
    mutate(`Cenario 2`=round(cumsum(deaths))) %>% select(-deaths)

  cumulative_deaths_max <-
    bind_rows(data_location %>% select(time, deaths) %>% filter(time < ymd(reference_date)),
              data_location_forecast %>% filter(time >= ymd(reference_date)) %>%
                rename(deaths=death_max_forecast) %>% select(time, deaths)) %>%
    mutate(`Cenario 3`=round(cumsum(deaths))) %>% select(-deaths)


  cumulative_deaths <- cumulative_deaths_max %>%
    left_join(cumulative_deaths_medium, by=c("time")) %>%
    left_join(cumulative_deaths_min , by=c("time"))

  cumulative_deaths
}


### Recent C plots ###



make_all_C_plot <- function(model_output, min_x_break=NULL, aggregate_name=NULL, save_path="./"){
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
