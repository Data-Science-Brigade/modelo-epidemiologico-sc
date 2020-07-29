library(epiCata)

allowed_locations <-
  c("SC_ESTADO",
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
    "SC_MUN_CHAPECO"
)

reference_date <- "2020-07-27"

model_output <-
  run_epidemiological_model(allowed_locations=allowed_locations,
                            reference_date=reference_date,
                            mode="DEBUG")

for(location_name in model_output$stan_list$available_locations){
  make_three_panel_plot(location_name, model_output)
}

make_all_forecast_plots(model_output)

