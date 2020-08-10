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

option_list = list(
  make_option(c("-m", "--mode"), type = "character", default = NULL, dest = "mode",
              help = "Mode to run the model (FULL/DEBUG/DEVELOP). If not specified you must specify -i -w -c -d -t and -v"),
  make_option(c("-i", "--iterations"), type = "integer", default = NULL, dest = "iter",
              help = "How many iterations to run the model with, the model takes samples using iterations-warmup iterations for each chain. OVERRIDES: -m's setting"),
  make_option(c("-w", "--warmup"), type = "integer", default = NULL, dest = "warmup",
              help = "How many warmup iterations to run the model for, these iterations count towards the total number of iterations specified by -i. OVERRIDES: -m's setting"),
  make_option(c("-c", "--chains"), type = "integer", default = NULL, dest = "chains",
              help = "How many of chains should be run. This uses multiple cores if your computer has them. OVERRIDES: -m's setting"),
  make_option(c("-d", "--adapt-delta"), type = "double", default = NULL, dest = "adapt_delta",
              help = "Model's target acceptance rate. OVERRIDES: -m's setting"),
  make_option(c("-t", "--max-treedepth"), type = "integer", default = NULL, dest = "max_treedepth",
              help = "The maximum treedepth the model will use when perfoming sampling. OVERRIDES: -m's setting"),
  make_option(c("-v", "--verbose"), type = "logical", default = NULL, dest = "verbose",
              help = "rstan::sample's verbosity. OVERRIDES: -m's setting"),
  make_option(c("-s", "--selected-date"), type = "character", default = NULL, dest = "selected_date",
              help = "Selected date for the model in ymd format. Will default to today if not passed."),
  make_option(c("-r", "--reference-date"), type = "character", default = "2020-08-10", dest = "reference_date",
              help = "Reference date for the model in ymd format."),
  make_option(c("-g", "--use-google-mobility"), type = "logical", default = TRUE, dest = "use_google_mobility",
              help = "If we should use google mobility data for the model"),
  make_option(c("-a", "--aggregate-name"), type = "character", default = "SC_ESTADO", dest = "aggregate_name",
              help = "If we should aggregate the macro-region data into a state data for plotting an aggregate version, the name of the aggregate should be passed here otherwise it will defalut to SC_ESTADO."),
  make_option(c("-l", "--allowed_locations"),
              default = default_locations, dest = "allowed_locations",
              help = sprintf("List of Allowed locations, the default is all macro-regions from the Santa Catarina state: %s", paste(default_locations, collapse=", ")))
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if(!is.null(opt$selected_date)) { opt$selected_date <- ymd(opt$selected_date) }
if(!is.null(opt$reference_date)) { opt$reference_date <- ymd(opt$reference_date) }

if(all(is.null(opt$mode), is.null(opt$iter), is.null(opt$warmup), is.null(opt$chains),
       is.null(opt$adapt_delta), is.null(opt$max_treedepth), is.null(opt$verbose))){
  warning("No model parameter passed, running model in DEBUG mode")
  opt$mode <- "DEBUG"
}

model_output <-
  run_epidemiological_model(selected_date = opt$selected_date,
                            reference_date = opt$reference_date,
                            allowed_interventions = opt$allowed_interventions,
                            allowed_locations = opt$allowed_locations,
                            use_google_mobility = opt$use_google_mobility,
                            mode = opt$mode,
                            iter = opt$iter,
                            warmup = opt$warmup,
                            chains = opt$chains,
                            adapt_delta = opt$adapt_delta,
                            max_treedepth = opt$max_treedepth,
                            verbose = opt$verbose
  )

make_all_three_panel_plot(model_output, aggregate_name="SC_ESTADO")

make_all_forecast_plots(model_output, aggregate_name="SC_ESTADO")



