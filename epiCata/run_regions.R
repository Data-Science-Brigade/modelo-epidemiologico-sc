library(epiCata)

allowed_locations <-
  c("SC_MAC_FOZ_DO_RIO_ITAJAI",
    "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
    "SC_MAC_GRANDE_OESTE",
    "SC_MAC_GRANDE_FLORIANOPOLIS",
    "SC_MAC_SUL",
    "SC_MAC_ALTO_VALE_DO_ITAJAI",
    "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE",
)

reference_date <- "2020-07-27"

model_output <-
  run_epidemiological_model(allowed_locations=allowed_locations,
                            reference_date=reference_date,
                            mode="DEBUG")

make_all_three_panel_plot(model_output, aggregate_name = "SC_ESTADO")

make_all_forecast_plots(model_output, aggregate_name = "SC_ESTADO")

