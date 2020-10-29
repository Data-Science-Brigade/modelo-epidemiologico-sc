library(optparse)
library(epiCata)
library(lubridate)

default_locations <- c("SC_ESTADO",
                       "SC_MAC_FOZ_DO_RIO_ITAJAI",
                       "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
                       "SC_MAC_GRANDE_OESTE",
                       "SC_MAC_GRANDE_FLORIANOPOLIS",
                       "SC_MAC_SUL",
                       "SC_MAC_ALTO_VALE_DO_ITAJAI",
                       "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE",
                       "SC_MUN_JOINVILLE",
                       "SC_MUN_ITAJAI",
                       "SC_MUN_FLORIANOPOLIS",
                       "SC_MUN_BLUMENAU",
                       "SC_MUN_CRICIUMA",
                       "SC_MUN_LAGES",
                       "SC_MUN_CHAPECO")

option_list <- make_option_list(default_locations,
                                default_locations_text = " the state, all macro-regions, and some cities",
                                reference_date="2020_08_10")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

model_output <- run_model_with_opt(opt,default_locations)

print("Done")
