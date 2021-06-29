library(optparse)
library(epiCataPlot)
library(epiCata)
library(lubridate)

default_locations <- c("SC_MAC_GRANDE_FLORIANOPOLIS")

option_list <- make_option_list(default_locations,
                                mode="FULL",
                                default_locations_text = "only single-region",
                                reference_date="2021_06_07",
                                nickname="")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

model_output <- run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name, 
                          save_path = opt$save_path)

mi <- NULL
wma <- NULL
ma <- NULL

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name, 
                        min_y_breaks=mi,max_y_breaks=ma, week_max_y_breaks=wma, 
                        save_path = opt$save_path)

last_8_weeks = ymd(model_output$reference_date_str) - 8*7 - 1

make_all_C_plot(model_output, aggregate_name = opt$aggregate_name, min_x_break=last_8_weeks, save_path = opt$save_path)

#save_data_for_dashboard(model_output, save_path = "~/epiCataDashboard/", aggregate_name = opt$aggregate_name)

print("Done")
