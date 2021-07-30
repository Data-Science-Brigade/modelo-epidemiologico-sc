reference_date <- "2021_04_26"
include_forecast = TRUE

library(lubridate)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(scales)
library(dplyr)
library(epiCataPlot)

get_ref_date_from_path <- function(path) {
    ymd(get_ref_date_str_from_path(path))
}

get_ref_date_str_from_path <- function(path) {
    strsplit(path,"/")[[1]][[1]]
}

read_location <- function(location, prefix="") {
    data <- list()
    data$locations <- read_lines(paste0(prefix, location,"/location_names.txt"))
    data$aggregate_name <- read_file(paste0(prefix, location,"/aggregate_name.txt"))
    data$dfs = list(
        data_location = read.csv(paste0(prefix, location, "/data_location.csv")),
        data_location_forecast = read.csv(paste0(prefix, location, "/data_location_forecast.csv"))
    )

    data$dfs$data_location$time <- ymd(data$dfs$data_location$time)
    data$dfs$data_location_forecast$time <- ymd(data$dfs$data_location_forecast$time)
    data
}

get_x_breaks <- function(dfs, max_breaks=16) {
    x_min_date <- if(max(dfs$data_location$reported_cases_c)>=50) {
        min(filter(dfs$data_location, predicted_cases_c>0)$time) - 7
    } else {
        min(dfs$data_location$time)
    }
    x_max_date <- max(dfs$data_location$time)

    # Could do a non-loop version, but I believe this is more readable
    weeks <- 0
    repeat{
        weeks <- weeks + 1
        rest <- as.integer(x_max_date - x_min_date) %% (7*weeks)
        x_min_date_shifted <- x_min_date - days((7*weeks) - rest)

        x_breaks <- seq(x_min_date_shifted, x_max_date, by=ifelse(weeks==1,"week",paste0(weeks, " weeks")))
        if(length(x_breaks) <= max_breaks){
            break
        }
    }
    x_breaks
}



followup_width <- 15
followup_height <- 6
followup_dpi_div = 1.5

heatmap_width <- 14
heatmap_height <- 6
heatmap_dpi_div = 1.5

threepanel_width <- 14
threepanel_height <- 4

ab_width <- 10
ab_height <- 4

c_width <- 9
c_height <- 4

forecast_width <- 12
forecast_height <- 5

dpi = 600

is_weekly_filename <- paste0(reference_date,"/is_weekly.txt")
model_is_weekly <- if(file.exists(is_weekly_filename)){
    as.logical(read_lines(is_weekly_filename)[[1]])
} else {
    FALSE
}

regions <- list.dirs(reference_date, recursive=FALSE)

regions_dfs <- list()
for(region in regions) {
    data <- read_location(region)
    if(grepl("SC_RSA", data$aggregate_name, fixed=TRUE)) {
        regions_dfs <- append(regions_dfs, list(data$dfs))
    }
}

ggsave(
    "followup.png",
    plot_weekly_average_followup(regions_dfs, include_forecast=include_forecast, model_is_weekly=model_is_weekly),
    type="cairo", width = followup_width, height = followup_height, dpi=dpi/followup_dpi_div
)

full_df <- get_full_df_from_region_dfs(regions_dfs)
half_regions_sets <- list()
region_set <- list()
ordered_unique_locations <- unique(
  (
    full_df %>%
      group_by(location_name) %>%
      summarize(deaths_ravg=max(deaths_ravg, na.rm=TRUE)) %>%
      arrange(deaths_ravg)
  )$location_name
)

half1_regions <- ordered_unique_locations[1:length(ordered_unique_locations)/2]
print(half1_regions)
half1_dfs <- list()
for(region in half1_regions) {
    data <- read_location(region, prefix=paste0(reference_date,"/"))
    if(grepl("SC_RSA", data$aggregate_name, fixed=TRUE)) {
        half1_dfs <- append(half1_dfs, list(data$dfs))
    }
}
ggsave(
    "followup1.png",
    plot_weekly_average_followup(half1_dfs, include_forecast=include_forecast, model_is_weekly=model_is_weekly),
    type="cairo", width = followup_width, height = followup_height, dpi=dpi/followup_dpi_div
)


half2_regions <- ordered_unique_locations[(1+length(ordered_unique_locations)/2):length(ordered_unique_locations)]
print(half2_regions)
half2_dfs <- list()
for(region in half2_regions) {
    data <- read_location(region, prefix=paste0(reference_date,"/"))
    if(grepl("SC_RSA", data$aggregate_name, fixed=TRUE)) {
        half2_dfs <- append(half2_dfs, list(data$dfs))
    }
}
ggsave(
    "followup2.png",
    plot_weekly_average_followup(half2_dfs, include_forecast=include_forecast, model_is_weekly=model_is_weekly),
    type="cairo", width = followup_width, height = followup_height, dpi=dpi/followup_dpi_div
)

ggsave(
    "heatmap.png", plot_weekly_average_heatmap(regions_dfs, include_forecast=include_forecast, model_is_weekly=model_is_weekly),
    type="cairo", width = heatmap_width, height = heatmap_height, dpi=dpi/heatmap_dpi_div
)

for(location in regions) {
    print(location)
    aggregate_name <- read_file(paste0(location,"/aggregate_name.txt"))
    data <- read_location(location)
    ref_date <- get_ref_date_from_path(location)
    ref_date_str <- get_ref_date_str_from_path(location)
    x_breaks <- get_x_breaks(data$dfs)

    print(aggregate_name)
    print(ref_date_str)

    ggsave(
        paste0(aggregate_name,"_3panel.png"),
        make_three_panel_plot(data$aggregate_name, data$dfs, data$reference_date, auto_save=FALSE, model_is_weekly=model_is_weekly),
        type="cairo", width = threepanel_width, height = threepanel_height, dpi=dpi
    )

    if("SC_ESTADO" %in% aggregate_name){
        ggsave(
            paste0(aggregate_name,"_a.png"),
            plot_graph_A(data$aggregate_name, x_breaks, data$dfs, model_is_weekly=model_is_weekly),
            type="cairo", width = ab_width, height = ab_height, dpi=dpi
        )
        ggsave(
            paste0(aggregate_name,"_b.png"),
            plot_graph_B(data$aggregate_name, x_breaks, data$dfs, model_is_weekly=model_is_weekly),
            type="cairo", width = ab_width, height = ab_height, dpi=dpi
        )
    }

    # Unused
    # ggsave(paste0(aggregate_name,"c.png"), plot_graph_C(data$aggregate_name, x_breaks, data$dfs, model_is_weekly=model_is_weekly))
    ggsave(
        paste0(aggregate_name,"_c.png"),
        plot_graph_C(data$aggregate_name, seq(max(x_breaks)-8*7, max(x_breaks), by="week"), data$dfs, model_is_weekly=model_is_weekly),
        type="cairo", width = c_width, height = c_height, dpi=dpi
    )

    ggsave(
        paste0(aggregate_name,"_forecast_month.png"),
        make_forecast_plot(data$aggregate_name, data$dfs, ref_date_str, is_weekly = FALSE, auto_save=FALSE, model_is_weekly=model_is_weekly),
        type="cairo", width = forecast_width, height = forecast_height, dpi=dpi
    )

    ggsave(
        paste0(aggregate_name,"_forecast_week.png"),
        make_forecast_plot(data$aggregate_name, data$dfs, ref_date_str, is_weekly = TRUE, auto_save=FALSE, model_is_weekly=model_is_weekly),
        type="cairo", width = forecast_width, height = forecast_height, dpi=dpi
    )
}
