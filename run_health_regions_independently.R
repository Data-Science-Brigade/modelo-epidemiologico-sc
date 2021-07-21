library(optparse)
library(epiCataPlot)
library(epiCata)
library(lubridate)
library(data.table)
library(abind)
library(optparse)


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

option_list <- make_option_list(micro_health_regions,
                                mode="FULL",
                                default_locations_text = "all health-regions independently",
                                reference_date="2020_07_10",
                                aggregate_name="SC_ESTADO",
                                nickname="RSA")

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


for (location in micro_health_regions){
    w<-strsplit(location, "_")
    short_name <- w[[1]][3:length((w[[1]]))]
    location_nickname <-paste(short_name, collapse = '_')
    
micro_health_regions <- c("SC_RSA_ALTO_VALE_DO_ITAJAI",
                          "SC_RSA_ALTO_VALE_DO_RIO_DO_PEIXE",
                          "SC_RSA_CARBONIFERA",
                          "SC_RSA_EXTREMO_OESTE",
                          "SC_RSA_EXTREMO_SUL_CATARINENSE")
for (location in micro_health_regions){ 
    i <- which(micro_health_regions==location)
    
    if (!is.null(opt$model_init_filename)) {
        last_model_date <- strftime(as.Date(opt$reference_date, "%Y-%m-%d") - 7, "%Y_%m_%d")
        dir_last_model <- paste0("../results/", last_model_date)
        glob_var<-paste0("*", location_nickname, "*")
        model_init_fname <- fs::dir_ls(path = dir_last_model, type = "file", glob = glob_var)
    } else {
        model_init_fname <- NULL
    }
    
    opt$model_init_filename <- model_init_fname
    opt$allowed_locations <- location

    # TODO: allowed_locations is a misnomer. Rename it to "selected_locations" so it makes more sense.
    model_output <- run_model_with_opt_independent(opt,opt[["allowed_locations"]])
    if (i == 1){
        model_files <- copy(model_output$filename_suffix)
    } else {
        model_files <- append(model_files, model_output$filename_suffix )
    }
    rm(model_output)

}

# update covid_data in model_output_all
covid_data_all <- read_covid_data(opt[["deaths"]], opt[["population"]], opt[["reference_date"]],
                                  allowed_locations = micro_health_regions
)

dir_saved_models <- paste0(opt$save_path,"results/", strftime(opt$reference_date, "%Y_%m_%d") )
this_dir = getwd()

setwd(dir_saved_models)

if (length(model_files) != length(micro_health_regions)){
    sprintf("Not enough single-region models! Check if all models concluded successfully. Expected %s, got %s",
                    length(micro_health_regions), length(model_files)    )
    # HOW DO I PRINT AN ERROR HERE?

} else {
    for (model_file in model_files){
        i <- which(model_files==model_file)
        load(paste0(model_file, "-stanfit.Rdata") )

        if (i==1){
            model_output_all <- copy(model_output)
            model_output_all$stan_list$stan_data$M <- length(micro_health_regions)
        } else {
            model_output_all <- update_aggregated_model(model_output_all, model_output)
        }
        rm(model_output)
    }
}

model_output_all[["covid_data"]] <- covid_data_all

filename_suffix <- paste0(model_output_all$reference_date_str, "_",
                          model_output_all$model_name, "_",
                          model_output_all$mode, "_",
                          "HEALTH-REG-INDEPEND")

model_output_all$filename_suffix <- filename_suffix

model_output_all_filename <- paste0(model_output_all$filename_suffix, "-stanfit.Rdata")

cat(sprintf("\nSaving joint model objects to %s", model_output_all_filename))
save(model_output_all, file = model_output_all_filename)

setwd(this_dir)


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

save_data_for_dashboard(model_output_all, save_path = "~/epiCataDashboard/", aggregate_name = opt$aggregate_name)

print("Done")



