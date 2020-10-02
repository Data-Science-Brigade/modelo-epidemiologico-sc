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

model_output <- run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name)

mi <- list(SC_ESTADO=NULL, SC_MAC_ALTO_VALE_DO_ITAJAI=NULL, SC_MAC_FOZ_DO_RIO_ITAJAI=NULL, SC_MAC_GRANDE_FLORIANOPOLIS=NULL,
           SC_MAC_GRANDE_OESTE=NULL, SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE=NULL, SC_MAC_PLANALTO_NORTE_E_NORDESTE=NULL, SC_MAC_SUL=NULL)
wma <- list(SC_ESTADO=NULL, SC_MAC_ALTO_VALE_DO_ITAJAI=NULL, SC_MAC_FOZ_DO_RIO_ITAJAI=NULL, SC_MAC_GRANDE_FLORIANOPOLIS=NULL,
            SC_MAC_GRANDE_OESTE=NULL, SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE=NULL, SC_MAC_PLANALTO_NORTE_E_NORDESTE=NULL, SC_MAC_SUL=NULL)
ma <- list(SC_ESTADO=NULL, SC_MAC_ALTO_VALE_DO_ITAJAI=NULL, SC_MAC_FOZ_DO_RIO_ITAJAI=NULL, SC_MAC_GRANDE_FLORIANOPOLIS=NULL,
           SC_MAC_GRANDE_OESTE=NULL, SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE=NULL, SC_MAC_PLANALTO_NORTE_E_NORDESTE=NULL, SC_MAC_SUL=NULL)

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name, min_y_breaks=mi, max_y_breaks=ma, week_max_y_breaks=wma)

last_8_weeks = ymd(model_output$reference_date_str) - 8*7 - 1

make_all_C_plot(model_output, aggregate_name = opt$aggregate_name, min_x_break=last_8_weeks)

plot_state_forecast(model_output)

print("Done")
