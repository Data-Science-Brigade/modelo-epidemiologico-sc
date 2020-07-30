library(epiCata)

allowed_locations <-
  c("SC_MUN_JOINVILLE",
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

make_all_three_panel_plot(model_output, aggregate_name = "SC_ESTADO")

make_all_forecast_plots(model_output, aggregate_name = "SC_ESTADO")

