
library(auk)
library(tidyverse)
library(lubridate)

data_root <- "data/"
results_root <- "output/"
figures_root <- "figures/"


# --------------------------------------------------------------------
# read in all sampling events

birds <- auk_sampling(file = "ebd_US-NY_relFeb-2021/ebd_US-NY_relFeb-2021.txt")

auk_filters <- birds %>% 
  auk_protocol(protocol = c("Stationary", "Traveling", "Incidental")) %>% 
  auk_state("US-NY")

birds_out <- str_glue("{data_root}/checklist_data_NY_state.txt")
auk_filters %>%
		auk_filter(file = birds_out, overwrite = TRUE)

# list all species
species_names <- auk::ebird_taxonomy %>%
		filter(category == "species") %>%
		dplyr::select(scientific_name)

# split into one file for each species
auk_split(
       file = birds_out,
       species = species_names$scientific_name,
       prefix = str_glue("{data_root}/NY_ebd_split_species/")
     )


# --------------------------------------------------------------------
# reduce the sampling file to only the required columns. 

samp_file <- auk_sampling(file = "ebd_sampling_relFeb-2021/ebd_sampling_relFeb-2021.txt")
auk_filter_samp <- samp_file %>% 
		auk_protocol(protocol = c("Stationary", "Traveling", "Incidental")) %>%
		auk_state("US-NY") 
samp_out <- str_glue("{data_root}/sampling_data_NY_state.txt")

samp <- auk_filter_samp %>%
		auk_filter(file = samp_out, overwrite = TRUE) %>%
		read_sampling()

samp2 <- samp %>% 
		dplyr::select(checklist_id, sampling_event_identifier, observer_id, latitude, longitude, observation_date, duration_minutes, all_species_reported)

csv_loc <- str_glue("{data_root}/sampling_data_NY_state_slim.csv")
write_csv(samp2, csv_loc)



