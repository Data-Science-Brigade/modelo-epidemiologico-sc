library(optparse)
library(epiCata)
library(lubridate)

default_locations <- c("SC_MUN_JOINVILLE",
                       "SC_MUN_ITAJAI",
                       "SC_MUN_FLORIANOPOLIS",
                       "SC_MUN_BLUMENAU",
                       "SC_MUN_CRICIUMA",
                       "SC_MUN_LAGES",
                       "SC_MUN_CHAPECO")

option_list <- make_option_list(default_locations,
                                default_locations_text = " some cities",
                                reference_date="2020_08_10",
                                nickname="MUN")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name)

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name)

print("Done")
