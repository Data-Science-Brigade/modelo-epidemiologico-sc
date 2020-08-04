library(dplyr)
library(usethis)

#### code to prepare `serial_interval` dataset goes here ####

# Serial Interval used in the project

serial_interval <- readr::read_csv("data-raw/serial_interval.csv", col_names=c("X", "fit"), skip=1)

#### code to prepare `IFR` dataset goes here ####

# Float number for the estimated Infection Fatality Rate (IFR) estimated for the state of Santa Catarina in
# https://github.com/ImperialCollegeLondon/covid19model/blob/master/Brazil/data/IFRS-all.csv
# The authors estimated Brazilian IFRs using four different references: IFR_UK	| IFR_Peru |	IFR_UK_poorer |	IFR_Peru_poorer
# In their final report, they end up using IFR_Peru_poorer and that is what we use here too
#  (https://github.com/ImperialCollegeLondon/covid19model/blob/master/Brazil/code/preprocessing-subnation-brazil.r)

weight_fatality <- read.csv("data-raw/Brazil-IFRS-all.csv")[c("X","State","IFR_Peru_poorer")]

# IFR Estimated for the state of Santa Catarina
IFR <- (weight_fatality %>% filter(State == "SC"))[,3]

#### PARAMETERS FOR GAMMA DISTRIBUTIONS ####

# infection-to-onset distribution parameters (DEFAULT)
infection_to_onset <- data.frame(avg_days = 5.1, coeff_variation = 0.86)

#### POPULATION OF SANTA CATARINA ####

SC_pop <- read.csv("data-raw/SC_populacao_macrorregioes_regioesdesaude.csv")

SC_pop_cities <- SC_pop %>%
  mutate(location_name=paste0("SC_MUN_", gsub(" ", "_", nom_municipio))) %>%
  rename(pop=qtd_populacao_estimada) %>%
  select(location_name, pop)

SC_pop_macrorregions <- SC_pop %>%
  group_by(nom_regional) %>% summarise(pop=sum(qtd_populacao_estimada)) %>% ungroup %>%
  mutate(location_name=paste0("SC_MAC_", gsub(" ", "_", nom_regional))) %>%
  select(location_name, pop)

SC_pop_healthregions <- SC_pop %>%
  group_by(nom_regiaosaude) %>% summarise(pop=sum(qtd_populacao_estimada)) %>% ungroup %>%
  mutate(location_name=paste0("SC_RSA_", gsub(" ", "_", nom_regiaosaude))) %>%
  select(location_name, pop)

SC_pop_state <- SC_pop %>% summarise(pop=sum(qtd_populacao_estimada)) %>%
  mutate(location_name="SC_ESTADO") %>%
  select(location_name, pop)

pop <- bind_rows(SC_pop_cities, SC_pop_healthregions, SC_pop_macrorregions, SC_pop_state)

#### CREATE GOOGLE MOBILITY ####

df <- readr::read_csv("data-raw/Global_Mobility_Report_latest.csv")
df %>% filter(iso_3166_2_code == "BR-SC") %>% write_csv("inst/extdata/2020_08_02_google_mobility.csv")

#### SAVE IT ALL ####

usethis::use_data(IFR, serial_interval, infection_to_onset, pop,
                  overwrite = TRUE, internal=TRUE)
