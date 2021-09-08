library(optparse)
library(epiCataPlot)
library(epiCata)
library(lubridate)
library(data.table)
library(abind)


macro_health_regions <- c("SC_MAC_FOZ_DO_RIO_ITAJAI",
                          "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
                          "SC_MAC_GRANDE_OESTE",
                          "SC_MAC_GRANDE_FLORIANOPOLIS",
                          "SC_MAC_SUL",
                          "SC_MAC_ALTO_VALE_DO_ITAJAI",
                          "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE")

micro_health_regions_A <- c("SC_RSA_ALTO_URUGUAI_CATARINENSE",
                            "SC_RSA_ALTO_VALE_DO_ITAJAI",
                            "SC_RSA_ALTO_VALE_DO_RIO_DO_PEIXE",
                            "SC_RSA_CARBONIFERA")

# micro_health_regions_B <- c("SC_RSA_EXTREMO_OESTE",
#                             "SC_RSA_EXTREMO_SUL_CATARINENSE",
#                             "SC_RSA_FOZ_DO_RIO_ITAJAI",
#                             "SC_RSA_GRANDE_FLORIANOPOLIS")

micro_health_regions_B <- c("SC_RSA_LAGUNA")


micro_health_regions_C <- c("SC_RSA_LAGUNA",
                            "SC_RSA_MEDIO_VALE_DO_ITAJAI",
                            "SC_RSA_MEIO_OESTE",
                            "SC_RSA_NORDESTE")

micro_health_regions_D <- c("SC_RSA_OESTE",
                            "SC_RSA_PLANALTO_NORTE",
                            "SC_RSA_SERRA_CATARINENSE",
                            "SC_RSA_XANXERE")

micro_health_regions <- c(micro_health_regions_A, micro_health_regions_B, micro_health_regions_C, micro_health_regions_D)


for (location in micro_health_regions_B){
  i <- which(micro_health_regions_B == location)
  option_list <- make_option_list(location,
                                  mode="FULL",
                                  default_locations_text = "all health-regions independently",
                                  reference_date="2021_07_05",
                                  aggregate_name="SC_ESTADO",
                                  nickname="RSA")
  
  opt_parser <- OptionParser(option_list=option_list);
  opt <- parse_args(opt_parser);
  
  
  w <- strsplit(location, "_")
  short_name <- w[[1]][3:length((w[[1]]))]
  location_nickname <- paste(short_name, collapse = '_')
  
  i <- which(micro_health_regions_B == location)
  
  if (!is.null(opt$model_init_filename)) {
    last_model_date <- strftime(as.Date(opt$reference_date, "%Y-%m-%d") - 7, "%Y_%m_%d")
    dir_last_model <- paste0("../results/", last_model_date)
    glob_var <- paste0("*", location_nickname, "*")
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




