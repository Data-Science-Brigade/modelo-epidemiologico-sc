library(optparse)
library(epiCata)
library(lubridate)

default_locations <- c("SC_MAC_FOZ_DO_RIO_ITAJAI",
                       "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
                       "SC_MAC_GRANDE_OESTE",
                       "SC_MAC_GRANDE_FLORIANOPOLIS",
                       "SC_MAC_SUL",
                       "SC_MAC_ALTO_VALE_DO_ITAJAI",
                       "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE")

option_list <- make_option_list(default_locations,
                                default_locations_text = " all macro-regions",
                                reference_date="2020_08_10",
                                aggregate_name = "SC_ESTADO",
                                nickname="MAC+AGG")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name)

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name

print("Done"))
