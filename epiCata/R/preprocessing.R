prepare_stan_data <- function(covid_data, interventions, onset_to_death, IFR,
                              serial_interval, infection_to_onset, population,
                              forecast = 30, is_weekly = FALSE, google_mobility_filename,
                              google_mobility_window_size, population_filename) {
  require(tidyverse)

  if (is_weekly) {
    covid_data <-
      covid_data %>%
      group_by(
        location_name,
        data_ocorrencia = cut(data_ocorrencia, "week", start.on.monday = TRUE)
      ) %>%
      summarise(
        casos = sum(casos),
        obitos = sum(obitos)
      )
    covid_data$data_ocorrencia <- ymd(covid_data$data_ocorrencia)

    infection_to_onset_std_days <- infection_to_onset$avg_days[[1]] * infection_to_onset$coeff_variation[[1]]
    infection_to_onset_std_days <- infection_to_onset_std_days / 7
    infection_to_onset$avg_days <- infection_to_onset$avg_days / 7
    infection_to_onset$coeff_variation <- infection_to_onset_std_days / infection_to_onset$avg_days

    interventions <-
      interventions %>%
      group_by(
        AREA,
        DATA = cut(DATA, "week", start.on.monday = TRUE)
      ) %>%
      summarise(
        ADERENCIA = mean(ADERENCIA)
      )
    interventions$DATA <- ymd(interventions$DATA)

    onset_to_death$avg_days <- onset_to_death$avg_days / 7
    onset_to_death$std_days <- onset_to_death$std_days / 7
    onset_to_death$coeff_variation <- onset_to_death$std_days / onset_to_death$avg_days

    serial_interval_weekly_idx <- c()
    serial_interval_weekly_fit <- c()
    for (i in 0:floor(length(serial_interval$fit) / 7)) {
      serial_interval_weekly_idx[[1 + i]] <- 1 + i
      serial_interval_weekly_fit[[1 + i]] <- sum(serial_interval$fit[(1 + (i * 7)):((1 + i) * 7)], na.rm = TRUE)
      # cut(serial_interval$X, seq(1,length(serial_interval$X)+7, by=7), right=FALSE, labels = FALSE)
    }
    serial_interval <- data.frame(
      X = serial_interval_weekly_idx,
      fit = serial_interval_weekly_fit
    )

    forecast <- ceiling(forecast / 7)
  }

  #### SELECT LOCATIONS ####
  available_locations <-
    covid_data %>%
    group_by(location_name) %>%
    summarise(casos = sum(casos), obitos = sum(obitos), .groups = "drop") %>%
    arrange(desc(obitos)) %>%
    filter(obitos >= 10)
  available_locations <- available_locations$location_name

  cat(sprintf("Locations available to model: \n  > %s\n", paste(available_locations, collapse = "\n  > ")))

  # Maximum number of days to simulate
  N2 <- (time_length(max(covid_data$data_ocorrencia) - min(covid_data$data_ocorrencia), ifelse(is_weekly, "week", "day")) + 1 + forecast)[[1]]

  #### Statistical distributions required for modeling ####
  x1 <- EnvStats::rgammaAlt(1e6, infection_to_onset$avg_days, infection_to_onset$coeff_variation) # infection-to-onset distribution
  x2 <- EnvStats::rgammaAlt(1e6, onset_to_death$avg_days, onset_to_death$coeff_variation) # onset-to-death distribution
  ecdf.saved <- ecdf(x1 + x2)
  
  gen_serial_interval <- ecdf(EnvStats::rgammaAlt(1e6, 6.05, 0.7)) # serial-interval distribution
  SI <- rep(0, N2) # f is the probability of dying on day i given infection
  SI[1] <- (gen_serial_interval(1.5) - gen_serial_interval(0))
  for (i in 2:N2) {
    SI[i] <- (gen_serial_interval(i + .5) - gen_serial_interval(i - .5))
  }

  #### INTERVENTIONS ####

  # A join with all_dates_df will ensure that all dates are represented
  all_dates <- seq(min(covid_data$data_ocorrencia), max(covid_data$data_ocorrencia), 
                   by = ifelse(is_weekly, "1 week", "1 day"))
  all_dates_df <- expand.grid(sort(unique(interventions$AREA)), all_dates)
  colnames(all_dates_df) <- c("AREA", "DATA")
  common_interventions <- interventions %>%
    right_join(all_dates_df) %>%
    arrange(AREA, DATA, ADERENCIA) %>%
    group_by(AREA) %>%
    fill(ADERENCIA, .direction = "down") %>%
    replace_na(list(ADERENCIA = 0)) %>%
    tidyr::spread(AREA, ADERENCIA)

  n_covariates <- common_interventions %>% ncol() - 1

  #### BUILD STAN DATA ####
  dates <- list()
  reported_cases <- list()
  deaths_by_location <- list()
  stan_data <- list(
    M = length(available_locations), N = NULL, deaths = NULL, f = NULL, N0 = ifelse(is_weekly, 3, 6), # N0 = 6 to make it consistent with Rayleigh
    cases = NULL, SI = SI, features = NULL,
    pop = NULL, N2 = N2, x = 1:N2, P = n_covariates
  )

  # Covariates array
  stan_data$X <- array(NA, dim = c(stan_data$M, stan_data$N2, stan_data$P))

  mobility <- read_google_mobility(google_mobility_filename)
  pop_df <- read_pop_df(population_filename)

  i <- 1
  for (location_name in available_locations) {
    result_list <- get_stan_data_for_location(
      location_name = location_name,
      population = population,
      IFR = IFR, N2 = N2,
      ecdf.saved = ecdf.saved,
      covid_data = covid_data,
      common_interventions = common_interventions,
      is_weekly = is_weekly,
      mobility = mobility,
      google_mobility_window_size = google_mobility_window_size,
      pop_df = pop_df
    )

    dates[[location_name]] <- result_list$dates
    reported_cases[[location_name]] <- result_list$reported_cases
    deaths_by_location[[location_name]] <- result_list$deaths_by_location

    stan_data$pop <- c(stan_data$pop, result_list$location_pop)
    stan_data$N <- c(stan_data$N, result_list$N)
    stan_data$f <- cbind(stan_data$f, result_list$f)
    stan_data$deaths <- cbind(stan_data$deaths, result_list$deaths)
    stan_data$cases <- cbind(stan_data$cases, result_list$cases)

    stan_data$X[i, , ] <- result_list$location_covariates
    i <- i + 1

    if (length(stan_data$N) == 1) {
      stan_data$N <- as.array(stan_data$N)
    }

    cat(sprintf("  > %s\n", location_name))
    cat(sprintf("  > %s - %s\n", min(result_list$dates), max(result_list$dates)))
  }

  if(typeof(stan_data$pop) == "integer"){
    stan_data$pop <- as.array(stan_data$pop)
  }
  
  return(list(
    "stan_data" = stan_data,
    "dates" = dates,
    "reported_cases" = reported_cases,
    "deaths_by_location" = deaths_by_location,
    "common_interventions" = common_interventions,
    "interventions_wide" = interventions,
    "available_locations" = names(dates)
  ))
}

get_stan_data_for_location <- function(location_name, population, IFR, N2, ecdf.saved, 
                                       covid_data, common_interventions, is_weekly = FALSE, mobility, 
                                       google_mobility_window_size, pop_df) {

  #### FILTER RELEVANT INFORMATION ####
  cat(sprintf("\n\nParsing data for location: %s\n", location_name))
  location_data <- covid_data[covid_data$location_name == location_name, ] %>% arrange(data_ocorrencia)

  # Epidemic start -- for the model -- starts 30 days before the 10th death happened
  # From Imperial's Technical Report
  # (https://github.com/ImperialCollegeLondon/covid19model/blob/master/Technical_description_of_Imperial_COVID_19_Model.pdf):
  #   "We assume that seeding of new infections begins 30 days before the day after a country has cumulatively observed 10 deaths.
  #    From this date, we seed our model with 6 sequential days of an equal number of infections:
  #      c_{1,m}=···=c_{6,m} ~ Exponential(1t), where t~Exponential(0.03).
  #    These seed infections are inferred in our Bayesian posterior distribution."
  idx_deaths_mark <- which(cumsum(location_data$obitos) >= 10)[1]
  month_before_deaths_mark <- idx_deaths_mark - ifelse(is_weekly, 4, 30)

  cat(sprintf(
    "   > First case was reported on %s %d\n   > The 10th death happened on %s %d\n",
    ifelse(is_weekly, "week", "day"),
    which(location_data$casos > 0)[1],
    ifelse(is_weekly, "week", "day"),
    idx_deaths_mark
  ))
  start_date <- as.Date("2020-03-08")
  idx_start_date <-which(location_data$data_ocorrencia == start_date)
  location_data <- location_data[idx_start_date:nrow(location_data), ]

  #### EPIDEMIC START AND POPULATION ####
  location_pop <- population[population$location_name == location_name, ]$pop

  #### N and N2 ####
  N <- nrow(location_data)
  cat(sprintf("%s has %d %s of data", location_name, N, ifelse(is_weekly, "weeks", "days")))
  location_forecast <- N2 - N

  if (location_forecast < 0) {
    print(sprintf("%s: %d", location_name, N))
    print("ERROR!!!! increasing N2")
    N2 <- N
    location_forecast <- N2 - N
  }

  #### INTERVENTIONS ####
  interventions <- process_google_mobility_for_cities(location_name, mobility = mobility, pop_df = pop_df, window_size = google_mobility_window_size)

  if (is_weekly) {
    interventions <-
      interventions %>%
      group_by(
        AREA,
        DATA = cut(DATA, "week", start.on.monday = TRUE)
      ) %>%
      summarise(
        ADERENCIA = mean(ADERENCIA)
      )
    interventions$DATA <- ymd(interventions$DATA)
  }

  all_dates <- seq(min(covid_data$data_ocorrencia), max(covid_data$data_ocorrencia), by = ifelse(is_weekly, "1 week", "1 day"))
  all_dates_df <- expand.grid(sort(unique(interventions$AREA)), all_dates)
  colnames(all_dates_df) <- c("AREA", "DATA")
  common_interventions <- interventions %>%
    right_join(all_dates_df) %>%
    arrange(AREA, DATA, ADERENCIA) %>%
    group_by(AREA) %>%
    fill(ADERENCIA, .direction = "down") %>%
    replace_na(list(ADERENCIA = 0)) %>%
    tidyr::spread(AREA, ADERENCIA)

  location_covariates <- common_interventions %>% filter(DATA >= min(location_data$data_ocorrencia))
  location_covariates[N:(N + location_forecast), ] <- location_covariates[N, ]
  location_covariates <- location_covariates %>%
    select(-DATA) %>%
    as.matrix()

  #### CONVOLUTION ####
  # IFR is the overall probability of dying given infection
  convolution <- function(u) (IFR * ecdf.saved(u))

  f <- rep(0, N2) # f is the probability of dying on day i given infection
  f[1] <- (convolution(1.5) - convolution(0))
  for (i in 2:N2) {
    f[i] <- (convolution(i + .5) - convolution(i - .5))
  }

  reported_cases <- as.vector(as.numeric(location_data$casos))
  deaths <- c(as.vector(as.numeric(location_data$obitos)), rep(-1, location_forecast))
  cases <- c(as.vector(as.numeric(location_data$casos)), rep(-1, location_forecast))

  return(list(
    location_pop = location_pop, N = N, N2 = N2, f = f,
    deaths = deaths, cases = cases, x = 1:N2,
    location_covariates = location_covariates,
    dates = location_data$data_ocorrencia,
    reported_cases = reported_cases,
    deaths_by_location = as.vector(as.numeric(location_data$obitos))
  ))
}

get_padded_serial_interval <- function(serial_interval, N2) {
  size_serial_interval <- nrow(serial_interval)

  # Pads serial interval with 0 if N2 is greater than the length of the serial
  # interval array
  padded_serial_interval <-
    if (N2 > size_serial_interval) {
      pad_serial.interval <- data.frame(
        "X" = (size_serial_interval + 1):N2,
        "fit" = rep(1e-17, max(N2 - size_serial_interval, 0))
      )
      rbind(serial_interval, pad_serial.interval)
    } else {
      serial_interval
    }
  padded_serial_interval
}
