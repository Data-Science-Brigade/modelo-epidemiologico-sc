library(optparse)
library(epiCataPlot)
library(epiCata)
library(lubridate)
library(data.table)


macro_health_regions <- c("SC_MAC_FOZ_DO_RIO_ITAJAI",
                          "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
                          "SC_MAC_GRANDE_OESTE",
                          "SC_MAC_GRANDE_FLORIANOPOLIS",
                          "SC_MAC_SUL",
                          "SC_MAC_ALTO_VALE_DO_ITAJAI",
                          "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE")

micro_health_regions <- c("SC_RSA_ALTO_URUGUAI_CATARINENSE",
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

all_possible_locations <- c(macro_health_regions, micro_health_regions)

for (location in micro_health_regions){ 
    i <- which(micro_health_regions==location)
    option_list <- make_option_list(location,
                                    mode="DEBUG",
                                    default_locations_text = "all health-regions independently",
                                    reference_date="2020_07_10",
                                    aggregate_name="SC_ESTADO",
                                    nickname="RSA")
    
    opt_parser <- OptionParser(option_list=option_list);
    opt <- parse_args(opt_parser);
    
    
    # TODO: allowed_locations is a misnomer. Rename it to "selected_locations" so it makes more sense.
    model_output <- run_model_with_opt(opt,opt[["allowed_locations"]])
    
    # TODO: update model_output_all
    if (i==1){
        model_output_all <- copy(model_output)
        model_output_all$stan_list$stan_data$M <- length(micro_health_regions)
    } else {
        model_output_all <- update_aggregated_model(model_output_all, model_output)
    }
}

# update covid_data in model_output_all
covid_data_all <- read_covid_data(opt[["deaths"]], opt[["population"]], opt[["reference_date"]],
                              allowed_locations = micro_health_regions 
)

model_output_all[["covid_data"]] <- covid_data_all

make_all_three_panel_plot(model_output_all, aggregate_name = opt$aggregate_name, 
                          save_path = opt$save_path)

mi <- NULL
wma <- NULL
ma <- NULL

make_all_forecast_plots(model_output_all, aggregate_name = opt$aggregate_name, 
                        min_y_breaks=mi,max_y_breaks=ma, week_max_y_breaks=wma, 
                        save_path = opt$save_path)

last_8_weeks = ymd(model_output_all$reference_date_str) - 8*7 - 1

make_all_C_plot(model_output_all, aggregate_name = opt$aggregate_name, min_x_break=last_8_weeks, save_path = opt$save_path)

#save_data_for_dashboard(model_output, save_path = "~/epiCataDashboard/", aggregate_name = opt$aggregate_name)

print("Done")
