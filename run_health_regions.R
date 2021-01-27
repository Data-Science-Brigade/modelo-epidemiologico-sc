library(optparse)
library(epiCata)
library(lubridate)

default_locations <- c("SC_RSA_ALTO_URUGUAI_CATARINENSE",
                       "SC_RSA_ALTO_VALE_DO_ITAJAI",
                       "SC_RSA_ALTO_VALE_DO_RIO_DO_PEIXE",
                       "SC_RSA_CARBONIFERA",
                       "SC_RSA_EXTREMO_OESTE",
                       "SC_RSA_EXTREMO_SUL_CATARINENSE",
                       "SC_RSA_FOZ_DO_RIO_ITAJAI",
                       "SC_RSA_GRANDE_FLORIANOPOLIS",
                       "SC_RSA_LAGUNA",
                       "SC_RSA_MEDIO_VALE_DO_ITAJAI",
                       "SC_RSA_MEIO_OESTE",
                       "SC_RSA_NORDESTE",
                       "SC_RSA_OESTE",
                       "SC_RSA_PLANALTO_NORTE",
                       "SC_RSA_SERRA_CATARINENSE",
                       "SC_RSA_XANXERE")

option_list <- make_option_list(default_locations,
                                default_locations_text = " all health-regions",
                                reference_date="2020_08_10",
                                aggregate_name="SC_ESTADO",
                                nickname="RSA")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

model_output <- run_model_with_opt(opt,default_locations)

make_all_three_panel_plot(model_output, aggregate_name = opt$aggregate_name, save_path = opt$save_path)

make_all_forecast_plots(model_output, aggregate_name = opt$aggregate_name, save_path = opt$save_path)

print("Done")
