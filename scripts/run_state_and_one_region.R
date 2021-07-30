library(optparse)
library(epiCataPlot)
library(epiCata)
library(lubridate)

default_locations <- c("SC_ESTADO", "SC_MAC_SUL")

option_list <- make_option_list(default_locations,
                                default_locations_text = " the state and a single macro-regions",
                                reference_date="2020_08_10",
                                nickname="SC+1mac")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

model_output <- run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name)

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name)

print("Done")
